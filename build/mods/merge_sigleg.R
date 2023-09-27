
#libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(jsonlite)
library(stringr)
library(here)
library(assertthat)

#load data

raw_data = read_csv(here('build','significant_legislation_database.csv'))
corrections = read_csv(here("build","aps_corrections.csv"))

master = read_csv(here('build','master.csv'))

# cleaning raw data from replication file and other fixes

APS_fixed = bind_rows(raw_data %>%
  anti_join(corrections,by="id"),
  corrections) %>%
  filter(category!='Constitutional Amendment')

#note that there are a few non laws
dropids = c(1224)

APS_fixed=APS_fixed %>%
  filter(!(id %in% dropids))
            
            
          


APS_fixed$pl <- gsub("PL", "", APS_fixed$pl)
APS_fixed$pl <- gsub(" ", "", APS_fixed$pl)
APS_fixed$pl <- gsub("N/A", "", APS_fixed$pl)
APS_fixed$pl <- gsub("Failed", "", APS_fixed$pl)
APS_fixed$pl <- gsub("NONE", "", APS_fixed$pl)
APS_fixed$pl <- gsub("_", "-", APS_fixed$pl)


stat_at_large_regex = "(\\d+)\\s*Stat\\.?\\s*(\\d+)"

stat_large_citations1= APS_fixed %>%
  filter(!is.na(pl) & nchar(pl)>1) %>%
  select(id,pl) %>%
  mutate(
    sal_volume=str_extract(pl,stat_at_large_regex,group=1),
         sal_page_start=str_extract(pl,stat_at_large_regex,group=2)) %>%
  select(-pl) %>%
  filter(!is.na(sal_volume) & !is.na(sal_page_start))
  
stat_large_citations2 = APS_fixed %>%
  filter(!(str_detect(pl,stat_at_large_regex) )) %>%
  select(id,legislation) %>%
  mutate(
    sal_volume=str_extract(legislation,stat_at_large_regex,group=1),
    sal_page_start=str_extract(legislation,stat_at_large_regex,group=2))  %>%
  select(-legislation) %>%
  filter(!is.na(sal_volume) & !is.na(sal_page_start))

#check that these don't overlap
intersect(stat_large_citations1$id,stat_large_citations2$id)

stat_large_citations = bind_rows(stat_large_citations1,stat_large_citations2)
stat_large_citations$sal_volume = as.numeric(stat_large_citations$sal_volume)
stat_large_citations$sal_page_start = as.numeric(stat_large_citations$sal_page_start)

pl_citation = "(\\d+)\\-(\\d+)"

pl_data = APS_fixed %>% 
  filter(!is.na(pl) & nchar(pl)>1 &!(str_detect(pl,stat_at_large_regex)  )) %>%
  select(id,congress,pl) %>%
  mutate(congress_no=str_extract(pl,pl_citation,group=1),
         law_no=str_extract(pl,pl_citation,group=2)) %>%
  filter(congress==congress_no) %>%
  select(-congress,-pl)


APS_fixed = APS_fixed %>%
  left_join(stat_large_citations,by="id") %>%
  left_join(pl_data,by="id") %>%
  filter((!is.na(congress_no) & !is.na(law_no)) |
        (!is.na(sal_volume) & !is.na(sal_page_start)))

# Merging -----------------------------------------------------------------



# Merging Late ------------------------------------------------------------


aps_late <- APS_fixed %>%
  filter(congress >= 85)
       
master_late <- master %>%
  filter(congress_number >=85)

aps_late_merge = master_late %>%
  select(bdl_id=row_number,pl_no) %>%
  separate(pl_no,into=c("congress_no","law_no")) %>%
  full_join(aps_late)

#pl number or nothing for this period
aps_late %>% filter(!is.na(sal_volume))  %>% nrow()





# Early Period, only Statutes at Large -----

aps_early <- APS_fixed %>%
  filter(sal_volume <= 31)

master_early = master %>%
  filter(sal_volume<=31)
#merge sigleg, because of multiple laws on same page there will not be unique matches
early_matches = master_early %>%
  full_join(aps_early,by = c("sal_page_start", "sal_volume"),
            relationship='many-to-many')

#because multiple
master_overmatched_ids = early_matches %>%
  group_by(row_number) %>%
  count() %>%
  filter(n>1) %>%
  pull(row_number)

aps_overmatched_ids = early_matches %>%
  group_by(id) %>%
  count() %>%
  filter(n>1) %>%
  pull(id)

succesful_early_matches = early_matches %>%
  filter(!(row_number %in% master_overmatched_ids)) %>%
  filter(!(id %in% aps_overmatched_ids))

aps_early_merge = bind_rows(
  master_early %>%
  anti_join(succesful_early_matches,by="row_number"),
  succesful_early_matches
)


# Middle period -----------------------------------------------------------

aps_middle = APS_fixed%>%
  filter(congress > 57 & congress <85)


