library(tidyverse)
library(here)

Mods = read_csv(here("build","mods.csv")) %>%
  mutate(Source="GPO")
hein_data_raw =  read_csv(here("build","heinonline","hein_data_raw.csv")) %>%
  mutate(Source="HeinOnline")

# cleaning inputs

hein_data_raw_1 = hein_data_raw %>%
  filter(!is.na(title) & nchar(title)>0)

hein_overlapping_periods = hein_data_raw_1 %>%
  filter(sal_volume<=112 & sal_volume>=65) %>%
  mutate(period = case_when(
    sal_volume>=73 ~ "Late",
    sal_volume>=65 ~ "Mid"
  ))


mods_overlapping_periods = Mods %>%
  filter(sal_volume<=112) %>%
  mutate(period = case_when(
    sal_volume>=73 ~ "Late",
    sal_volume>=65 ~ "Mid"
  ))



#%% now begin merging

## Late Period Master -----

late_period_merged = hein_overlapping_periods %>%
  filter(period=="Late") %>%
  full_join(mods_overlapping_periods %>%
              filter(period=="Late"),
            by="pl_no")

# example of laws [sic] that were in HeinOnline
# but not in the GPO data. 
# Manually checked and confirmed,
# this should be in the dataset
hein_overlapping_periods %>%
  filter(period=="Late") %>%
  anti_join(mods_overlapping_periods %>%
              filter(period=="Late"),
            by="pl_no")

# example of laws that were in GPO data
# but not in the HeinOnline data. 
# Manually checked and confirmed,
# this should be in the dataset

mods_overlapping_periods %>%
  filter(period=="Late") %>%
  anti_join(hein_overlapping_periods %>%
              filter(period=="Late"),
            by="pl_no")

# this will have the effect of preferring the heinonline page/volumes
master_late = late_period_merged %>%
  mutate(sal_volume = coalesce(sal_volume.x, sal_volume.y)) %>%
  mutate(sal_page_start = coalesce(sal_page_start.x, sal_page_start.y)) %>%
  mutate(Title = coalesce(Title, title)) %>%
  transmute(pl_no, 
         sal_volume, 
         sal_page_start, 
         date_of_passage=Date,
         secondary_date=date, 
         chapter, 
         congress_number, 
         session_number, 
         action, 
         Title,
         StatuteCitation, 
         BillCitation, 
         URL)

## Mid Period Master -----

midperiod_merged_on_pl = hein_overlapping_periods %>%
  filter(period=="Mid") %>%
  full_join(mods_overlapping_periods %>%
              filter(period=="Mid"),
            by="pl_no")

# there are no missing matches based on
hein_overlapping_periods %>%
  filter(period=="Mid") %>%
  anti_join(mods_overlapping_periods %>%
              filter(period=="Mid"),
            by="pl_no") %>%
  nrow()

# there was a missing match here
# that has been confirmed as really an existing law

mods_overlapping_periods %>%
  filter(period=="Mid") %>%
  anti_join(hein_overlapping_periods %>%
              filter(period=="Mid"),
            by="pl_no")

# in retrospect, it is not obvious what justifies
# the separation of the periods into middle and late
# although Jamie Gall did see this

master_mid = midperiod_merged_on_pl %>% 
  mutate(sal_volume = coalesce(sal_volume.x, sal_volume.y)) %>%
  mutate(sal_page_start = coalesce(sal_page_start.x, sal_page_start.y)) %>%
  mutate(Title = coalesce(Title, title)) %>%
  transmute(pl_no, 
         sal_volume, 
         sal_page_start, 
         date_of_passage=Date,
         secondary_date=date, 
         chapter, 
         congress_number, 
         session_number, 
         action, 
         Title, 
         StatuteCitation,
         BillCitation, 
         URL) 

master_mid_and_late = bind_rows(master_mid,master_late) %>%
  mutate(
    date_of_passage=coalesce(date_of_passage,secondary_date),
    dates_conflict=date_of_passage!=secondary_date) 

master_mid_and_late$secondary_date[!master_mid_and_late$dates_conflict] = NA



# Early Period Master -----------------------------------------------------------

hein_data_raw_old <- hein_data_raw_1 %>%
  filter(sal_volume <= 64) %>%
  mutate(sal_page_start=as.integer(sal_page_start)) %>%
  select(action, 
         Title=title,
         sal_volume, 
         sal_page_start, 
         congress_number, 
         chapter, 
         session_number, 
         pl_no, 
         date_of_passage=date,
         Source)


master_early_mid_and_late = bind_rows(
  hein_data_raw_old,
  master_mid_and_late,
)


# Very recent -------------------------------------------------------------
Mods_new <- Mods%>%
  filter(sal_volume >112)

Mods_new$congress_number <- as.integer(gsub("^(\\d+)-.*", "\\1", Mods_new$pl_no))
Mods_new <- Mods_new %>% rename(date_of_passage = Date)
Mods_new$action <- NA
Mods_new$chapter <- NA
Mods_new$session_number <- NA
Mods_new$secondary_date <- NA
Mods_new$dates_conflict <- NA


Mods_new <- Mods_new %>%
  select(action, Title, sal_volume, sal_page_start, StatuteCitation, BillCitation, congress_number, chapter, session_number, pl_no, date_of_passage, secondary_date, dates_conflict, Source, URL )

master = bind_rows(
  master_early_mid_and_late,
  Mods_new
) %>%
  arrange(sal_volume, date_of_passage, secondary_date) %>%
  mutate(row_number = row_number()) %>%
  select(row_number, action, Title, sal_volume, sal_page_start, StatuteCitation, BillCitation, congress_number, chapter, session_number, pl_no, date_of_passage, secondary_date, dates_conflict, Source, URL )

## Now spot error fixing ----

problematic_congress_number = master %>%
  filter(is.na(congress_number))


congress_dates = read_csv(here("build","congress_dates.csv")) %>%
  mutate(congress_number=as.numeric(congress),
         session_number=as.numeric(session))

fixes = problematic_congress_number %>%
  select(date_of_passage) %>%
  cross_join(congress_dates) %>%
  filter(date_of_passage>=begin & date_of_passage<=end) %>%
  select(date_of_passage,congress_number,session_number)

fixed_congress_numbers = problematic_congress_number %>%
  left_join(fixes,by="date_of_passage") %>%
  mutate(congress_number=coalesce(congress_number.x,congress_number.y),
         session_number=coalesce(session_number.x,session_number.y)) %>%
  select(-congress_number.x,
         -congress_number.y,
         -session_number.x,
         -session_number.y)


master = bind_rows(
  master %>%
  filter(!(row_number %in% problematic_congress_number$row_number)),
  fixed_congress_numbers
)

master %>% filter(is.na(sal_page_start))

master$alternate_sal_volume = str_extract(master$StatuteCitation, "(\\d+) Stat. (\\d+)",group=1)
master$alternate_sal_page_start <- str_extract(master$StatuteCitation, 
                                   "(\\d+) Stat. (\\d+)",group=2)

master = master %>%
  mutate(has_alternate_sal_citation = (sal_volume!=alternate_sal_volume | sal_page_start!=alternate_sal_volume))

master$has_alternate_sal_citation[is.na(master$has_alternate_sal_citation)] = F


master$alternate_sal_volume[!master$has_alternate_sal_citation] = NA
master$alternate_sal_page_start[!master$has_alternate_sal_citation] = NA

master = master %>% select(-StatuteCitation) 

master %>%
  filter(is.na(session_number)) %>%
  summarise(min(date_of_passage))

#that seems ok, session number is missing but only for very recent stuff

# Output ----

write.csv(master, file = here("build","master.csv"), row.names=FALSE)
