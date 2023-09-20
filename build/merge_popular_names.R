#libraries
library(tidyverse)
library(jsonlite)
library(here)
library(skimr)
library(stringdist)

#data 
bdl_laws = read_csv(here("build","master.csv"))

popnames_raw <- stream_in(file(here("build","popnames2.jsonl")))   %>%
  tibble() %>%
  filter(!cite_contains_title) %>%
  mutate(raw_id=row_number()) 

# ---- Popular Names

# First we need to do some munging to work the data to the point where
# each row is a single law

# first problem, there are rows just cross-referencing other rows

second_names = popnames_raw %>%
  filter(!is.na(duplicate) & is.na(us_code)) %>% 
  select(alternate_name=popular_name,popular_name=duplicate) %>%
  group_by(popular_name) %>%
  summarise(alternate_names=paste(alternate_name,collapse="|"),
            number_of_alternate_names = n())

# there is one strange act that has a citation and is a duplicate
# but does not appear we can also add to avoid some issues later

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
  anti_join(second_names) %>% 
  anti_join(suspicious_public_laws) %>%
  anti_join(suspicious_citations) %>%
  group_by(public_law) %>%
  count() %>%
  filter(!is.na(public_law)) %>%
  filter(n>1)


multiply_occuring_citations = popnames_raw %>%
  anti_join(second_names) %>% 
  anti_join(suspicious_public_laws) %>%
  anti_join(suspicious_citations) %>%
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
  anti_join(multiply_occuring_citations)
  
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
  rename(pl_no="public_law")


#only NAs
popnames_3 %>% group_by(sal_volume,sal_page_start) %>% count() %>% filter(n>1)
popnames_3 %>% group_by(pl_no) %>% count() %>% filter(n>1)
popnames_3 %>%
  filter(is.na(date))

# now test
popnames_3 %>%
  anti_join(bdl_laws,by=c("pl_no"),na_matches="never") %>%
  anti_join(bdl_laws,by=c("sal_volume","sal_page_start"),na_matches="never") %>%
  filter(date<=lubridate::ymd("2017-01-03"))


bdl_laws1 = bdl_laws %>%
  mutate(bdl_id=row_number()) %>%
  inner_join(popnames_3 %>%
            select(popular_name,alternate_names,number_of_alternate_names,pl_no),
            by=c("pl_no"),
            na_matches='never')

sal_matches = bdl_laws %>%
  mutate(bdl_id=row_number()) %>%
  anti_join(bdl_laws1,by="bdl_id",
            na_matches='never') %>%
  inner_join(popnames_3 %>%
               select(popular_name,
                      alternate_names,
                      number_of_alternate_names,
                      sal_volume,
                      sal_page_start),
             by=c("sal_volume","sal_page_start"),
             na_matches='never',
             relationship='many-to-many')

# the problem with the above is that it is not uncommon to have two or three laws
# on the same page! So multiple matches using this approach are inevitable.
# sadly, we don't have  much we can do here besides manual coding, difficult given
# that there are 783 such laws.
sal_matches %>%
 group_by(sal_volume,sal_page_start) %>% 
  count() %>%
  filter(n>1) 

unique_sal_matches = sal_matches %>% semi_join(sal_matches %>%
  group_by(sal_volume,sal_page_start) %>% 
  count() %>%
  filter(n==1))

bdl_laws1 %>%
  semi_join(unique_sal_matches,by="bdl_id",na_matches="never")

unique_sal_matches %>%
  semi_join(bdl_laws1,by="bdl_id",na_matches="never")

bdl_laws2 = bind_rows(
  bdl_laws1,
  unique_sal_matches,
  bdl_laws %>%
    mutate(bdl_id=row_number()) %>%
    anti_join(bdl_laws1,by="bdl_id",na_matches="never") %>%
    anti_join(unique_sal_matches,by="bdl_id",na_matches="never")
)

#test that everything is as it should be

sum(1:nrow(bdl_laws) %in% bdl_laws2$row_number) #48844
bdl_laws2 %>% group_by(bdl_id) %>% count() %>% filter(n>1) %>% nrow() #0

bdl_laws2 %>%
  select(bdl_id=row_number,
         popular_name) %>%
  arrange(desc(bdl_id)) %>%
  write.csv(here("build","popular_name_appends.csv"))


# Performance Stats -----
number_of_unproblematic_popular_names_for_laws = popnames_3%>%
  filter(date<=lubridate::ymd("2017-01-03")) %>%
  nrow() 
number_of_appended_popular_names = bdl_laws2 %>% filter(!is.na(popular_name)) %>% nrow()

number_of_appended_popular_names/number_of_unproblematic_popular_names_for_laws

number_of_matched_laws = c(bdl_laws1 %>% pull(popular_name),
  sal_matches %>% pull(popular_name)) %>% 
  unique() %>%
  length()

number_of_matched_laws/number_of_unproblematic_popular_names_for_laws
