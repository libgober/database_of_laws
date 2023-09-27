#libraries
library(tidyverse)
library(jsonlite)
library(here)
library(skimr)
library(stringdist)
library(here)
source("build/match_two_laws.R")

#%% source helper functions

source(here("build","match_two_laws.R"))

# load the raw data
popnames_raw <- stream_in(file(here("build","popnames.jsonl")))   %>%
  tibble() %>%
  #eliminate rows that are merely parts of laws
  filter(!cite_contains_title) %>%
  mutate(raw_id=row_number()) 

# first problem, there are rows just cross-referencing other rows

second_names = popnames_raw %>%
  filter(!is.na(duplicate) & is.na(us_code)) %>% 
  select(alternate_name=popular_name,popular_name=duplicate) %>%
  group_by(popular_name) %>%
  summarise(alternate_names=paste(alternate_name,collapse="|"),
            number_of_alternate_names = n())


# there is one strange act that has a citation and is a duplicate
# but does not appear in the above,
# we can also add to avoid some issues later

second_names = bind_rows(second_names,
                         data.frame(popular_name="Randolph-Sheppard Act",
                                    alternate_names="Randolph-Sheppard Vending Stand Act",
                                    number_of_alternate_names=1))




# second issue, there are laws with the same public law number enacted on different dates!
# we will simply drop these public laws because we don't have the time to address it
suspicious_public_laws = popnames_raw %>%
  group_by(public_law) %>%
  summarise(number_of_unique_dates = length(unique(date)),
            number_of_unique_citations=length(unique(citation))) %>%
  filter(number_of_unique_dates>1 | number_of_unique_citations>1) %>%
  filter(!is.na(public_law))


popnames_raw %>%
  filter(public_law %in% suspicious_public_laws$public_law) %>%
  nrow()


# same issue with citations
suspicious_citations = popnames_raw %>%
  group_by(citation) %>%
  summarise(number_of_unique_dates = length(unique(date))) %>%
  filter(number_of_unique_dates>1)%>%
  filter(!is.na(citation))

# same issue with chapter and congress and session for middle years


popnames_raw_with_session_chapter = popnames_raw %>%
  mutate(
    stat_volumes=str_extract(citation,"(\\d+)\\s+Stat\\.?\\s+(\\d+)",group=1),
    congress=lookup_congress(date,public_law,stat_volumes),
    session=lookup_session(date,public_law,stat_volumes))  


suspicious_chapters = popnames_raw_with_session_chapter %>%
    group_by(chapter,congress,session) %>%
    count() %>%
    filter(n>1)  %>%
    filter(!is.na(chapter) & !is.na(congress) & !is.na(session)) %>%
    left_join(popnames_raw_with_session_chapter,by=c("chapter","congress","session")) 

popnames_raw_with_session_chapter

# also there are some rows where you've got no date, public law
suspicious_entry = popnames_raw %>%
  filter(is.na(public_law) & is.na(date) & is.na(citation))

# now another thing that occurs is that we have multiple popular names
# associated with a particular public law, often on different pages

popnames_raw %>%
  anti_join(second_names) %>% 
  anti_join(suspicious_public_laws) %>%
  anti_join(suspicious_citations) %>%
  group_by(public_law) %>%
  summarise(length(unique(popular_name)),
            length(unique(citation)),
            sum(!is.na(duplicate)),
            n=n()) %>%
  filter(n>1) #1236 rows with as many as 31 popular names to 1 citation, or 3 popular names to same citation

# pretty quickly you realize that these are actually popular names for titles
# or various parts of the overall document. We therefore suspect that these are not
# popular names for laws as we have defined it, or if they are it's too hard to sort
# out what they are given our constraints.

multiply_occuring_public_laws = popnames_raw %>%
  group_by(public_law) %>%
  count() %>%
  filter(!is.na(public_law)) %>%
  filter(n>1)


multiply_occuring_citations = popnames_raw %>%
  group_by(citation) %>%
  count() %>%
  filter(!is.na(citation)) %>%
  filter(n>1)

# so what we are left with will be at a better level of aggregation

popnames_of_laws = popnames_raw %>%
  filter(is.na(duplicate)) %>% 
  anti_join(suspicious_public_laws) %>%
  anti_join(suspicious_citations) %>%
  anti_join(multiply_occuring_public_laws) %>%
  anti_join(multiply_occuring_citations) %>%
  anti_join(suspicious_chapters,by="raw_id")

popnames = popnames_of_laws %>%
  filter(is.na(duplicate) | !is.na(us_code)) %>%
  select(-duplicate,-raw_id) %>%
  mutate(id=row_number())  %>%
  left_join(second_names,by = join_by(popular_name)) %>%
  replace_na(list(number_of_alternate_names=0))


#clean popnames
popnames_2 = popnames %>%
  mutate(sal_volume = str_extract(popnames$citation,
                                  "(\\d+) Stat\\.? (\\d+)",
                                  group=1) %>% as.integer(),
         sal_page_start=str_extract(popnames$citation,
                                    "(\\d+) Stat\\.? (\\d+)",
                                    group=2) %>% as.integer())

popnames_3 = popnames_2 %>%
  rename(pl_no="public_law") %>%
  mutate(pl_no=str_match(pl_no,"\\d+\\-\\d+")[,1])

popnames_4 = popnames_3 %>%
  mutate(congress=lookup_congress(dates = ymd(date),
                           pl_nos=pl_no,
                           volumes=sal_volume),
         session=lookup_session(dates = ymd(date),
                       pl_nos=pl_no,
                       volumes=sal_volume))

popnames_4 %>%
  filter(!is.na(pl_no) | !is.na(chapter)) %>% 
  write.csv(.,file = "build/popnames.csv")
         