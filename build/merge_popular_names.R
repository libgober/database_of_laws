#libraries
library(tidyverse)
library(jsonlite)
library(here)
library(skimr)
library(stringdist)
library(here)
#%% source helper functions

source(here("build","match_two_laws.R"))


#data 
master = read_csv(here("build","master.csv"))
popnames = read_csv(here('build',"popnames.csv")) %>%
  select(-`...1`)


master.vars = list(pl_no="pl_no",
              congress_number="congress_number",
              session="session_number",
              chapter="chapter")

popnames.vars = list(pl_no="pl_no",
              congress_number="congress",
              session="session",
              chapter="chapter")


combined = merge_law_databases(master,
                               popnames,
                               master.vars,popnames.vars,
                               all.x=T,all.y=F)



duplicates = combined %>%
  group_by(row_number) %>%
  count() %>%
  filter(n>1)


combined %>%
  filter(row_number %in% duplicates$row_number) %>%
  arrange(row_number) %>%
  nrow()

#test that everything is as it should be

sum(master$row_number %in% combined$row_number)/nrow(master) # 1 

combined %>% group_by(row_number) %>% count() %>% filter(n>1) %>% nrow() #0

combined %>%
  select(bdl_id=row_number,
         popular_name) %>%
  arrange(desc(bdl_id)) %>%
  write.csv(here("build","popular_name_appends.csv"))

citation_hits = master %>%
  select(sal_volume,sal_page_start) %>%
  left_join(popnames) %>%
  filter(!is.na(id)) %>%
  pull(id)  %>%
  unique()

better_hits = combined %>%
  filter(!is.na(id)) %>%
  pull(id) %>%
  unique()


# Performance Stats -----
number_of_unproblematic_popular_names_for_laws = popnames%>%
  nrow() 

number_of_appended_popular_names = combined %>% filter(!is.na(popular_name)) %>% nrow()

number_of_appended_popular_names/number_of_unproblematic_popular_names_for_laws

sum(popnames$id %in% unique(c(citation_hits,better_hits)))/nrow(popnames)
