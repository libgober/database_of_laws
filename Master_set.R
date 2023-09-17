#allow multiple users to access
if (Sys.getenv("USER") == 'brianlibgober'){
  setwd("~/Downloads/")
} else {
  setwd("/Users/jamiegall/Desktop/")
}

#libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(jsonlite)
library(stringr)
library(data.table)


#fixing Mods
mods_all <- read.csv("mods_all.csv")
mods_all_2 <- read.csv("mods_all_2.csv")

#subset mod data
values_to_exclude <- c("Private Laws", "Concurrent Resolutions", "Reorganization Plan", "Back Matter", "Constitutional Amendment", "Unknown", "Sub-Volume")

# Filter out rows where partName is in the list of values to exclude
mods_all_a <- mods_all[!mods_all$partName %in% values_to_exclude, ]

#subset mod data
values_to_exclude_2 <- c("Private Laws", "Concurrent Resolutions", "Reorganization Plan", "Back Matter", "Constitutional Amendment", "Unknown", "Sub-Volume")

# Filter out rows where partName is in the list of values to exclude
mods_all_2_a <- mods_all_2[!mods_all_2$partName %in% values_to_exclude_2, ]

# Find the rows in mods_all_2 that have differing "Date" values

differences <- inner_join(mods_all_2_a, mods_all_a, by = "PublicLaw") %>%
  filter(Date.x != Date.y)

differences <- differences %>%
  filter(Date.y < as.Date("2000-01-01"))

differences <- subset(differences, select = -Title.y)
differences <- subset(differences, select = -StatuteCitation.y)
differences <- subset(differences, select = -Date.y)
differences <- subset(differences, select = -BillCitation.y)
differences <- subset(differences, select = -partName.y)
differences <- subset(differences, select = -URL.y)

differences <- differences%>%
  rename(Title = "Title.x")
differences <- differences%>%
  rename(StatuteCitation = "StatuteCitation.x")
differences <- differences%>%
  rename(Date = "Date.x")
differences <- differences%>%
  rename(BillCitation = "BillCitation.x")
differences <- differences%>%
  rename(partName = "partName.x")
differences <- differences%>%
  rename(URL = "URL.x")

mods_all_public <- bind_rows(mods_all_a %>% filter(!Title %in% differences$Title), differences)

#clean all mods
mods_all_public$PublicLaw <- gsub("Public Law ", "", mods_all_public$PublicLaw)
mods_all_public <- mods_all_public %>%
  rename(pl_no = "PublicLaw")
mods_all_public$StatuteCitation <- str_extract(mods_all_public$StatuteCitation, "\\d+ Stat. \\d+")
mods_all_public$StatuteCitation <- ifelse(is.na(mods_all_public$StatuteCitation), "", mods_all_public$StatuteCitation)
split_numbers <- strsplit(mods_all_public$StatuteCitation, " Stat. ")

# Create new columns for the split numbers
mods_all_public$sal_volume <- sapply(split_numbers, `[`, 1)
mods_all_public$sal_page_start <- sapply(split_numbers, `[`, 2)

mods_all_public$sal_volume <- ifelse(is.na(mods_all_public$sal_volume), "", mods_all_public$sal_volume)
mods_all_public$sal_page_start <- ifelse(is.na(mods_all_public$sal_page_start), "", mods_all_public$sal_page_start)

mods_all_public$sal_volume <- as.integer(mods_all_public$sal_volume)
mods_all_public$sal_page_start <- as.integer(mods_all_public$sal_page_start)

mods_all_public <- mods_all_public %>%
  mutate(row_number = row_number())


write.csv(mods_all_public,file="MODS_fixes.csv",row.names=F)



#load data
popnames <- stream_in(file("popnames.jsonl"))
hein_data_raw <- read.csv("hein_data_raw.csv")
Mods <- read.csv("MODS_fixes.csv")


#add source
hein_data_raw$Source <- "Hein Online"
Mods$Source <- "GPO"

hein_data_raw$title[is.na(hein_data_raw$title) | hein_data_raw$title == ""] <- NA

hein_data_raw_1 <- hein_data_raw %>%
  filter(!is.na(title))

hein_data_raw_2 <- hein_data_raw_1 %>%
  filter(sal_volume <=112 & sal_volume > 64)
Mods_2 <- Mods%>%
  filter(sal_volume <=112)
 
filtered_late_hein <- hein_data_raw_2 %>%
  filter(sal_volume >= 73)
filtered_late_mods <- Mods_2 %>%
  filter(sal_volume >=73)

master1 <- merge(filtered_late_hein, filtered_late_mods, by = "pl_no", all.y = TRUE)

master1$sal_page_start.x <- as.integer(master1$sal_page_start.x)

master1 <- master1 %>%
  mutate(sal_volume = coalesce(sal_volume.x, sal_volume.y)) %>%
  mutate(sal_page_start = coalesce(sal_page_start.x, sal_page_start.y)) %>%
  mutate(Source = coalesce(Source.x, Source.y)) %>%
  mutate(Title = coalesce(Title, title)) %>%
  select(pl_no, sal_volume, sal_page_start, date, chapter, congress_number, session_number, Source, action, Title, Date, StatuteCitation, BillCitation, URL)

filtered_mid_hein <- hein_data_raw_2 %>%
  filter(sal_volume < 73)
filtered_mid_mods <- Mods %>%
  filter(sal_volume <73)

master2 <- merge(filtered_mid_hein, filtered_mid_mods, by = c("sal_volume", "sal_page_start"), all.y = TRUE)


master2 <- master2 %>%
  mutate(pl_no = coalesce(pl_no.x, pl_no.y)) %>%
  mutate(Title = coalesce(Title, title)) %>%
  mutate(Source = coalesce(Source.x, Source.y)) %>%
  select(pl_no, sal_volume, sal_page_start, date, chapter, congress_number, session_number, Source, action, Title, StatuteCitation, Date, BillCitation, URL)

  
master3 <- merge(master2, filtered_mid_mods, by = "pl_no", all.y = TRUE)

master3$sal_page_start.x <- as.integer(master3$sal_page_start.x)

master3 <- master3 %>%
  mutate(sal_volume = coalesce(sal_volume.x, sal_volume.y)) %>%
  mutate(sal_page_start = coalesce(sal_page_start.x, sal_page_start.y)) %>%
  mutate(Title = coalesce(Title.x, Title.y)) %>%
  mutate(Source = coalesce(Source.x, Source.y)) %>%
  mutate(StatuteCitation = coalesce(StatuteCitation.x, StatuteCitation.y)) %>%
  mutate(Date = coalesce(Date.x, Date.y)) %>%
  mutate(BillCitation = coalesce(BillCitation.x, BillCitation.y)) %>%
  mutate(URL = coalesce(URL.x, URL.y)) %>%
  select(pl_no, sal_volume, sal_page_start, date, chapter, congress_number, session_number, Source, action, Title, Date, StatuteCitation, BillCitation, URL)

master4 <- rbind(master1, master3)

# Replace empty cells with NA in the entire data frame
master4$date <- ifelse(master4$date == "", NA, master4$date)
master4$DatesEqual <- is.na(master4$Date) | is.na(master4$date) | (master4$Date == master4$date)
master4$DatesEqual <- ifelse(master4$DatesEqual, FALSE, TRUE)

master4 <- master4 %>% rename(secondary_date = date)
master4 <- master4 %>% rename(date_of_passage = Date)
master4 <- master4 %>% rename(dates_conflict = DatesEqual)


master4 <- master4 %>%
  select(action, Title, sal_volume, sal_page_start, StatuteCitation, BillCitation, congress_number, chapter, session_number, pl_no, date_of_passage, secondary_date, dates_conflict, Source, URL )



#add on hein pre 1951
hein_data_raw_old <- hein_data_raw_1 %>%
  filter(sal_volume <= 64)

hein_data_raw_old$URL <- NA
hein_data_raw_old$StatuteCitation <- NA
hein_data_raw_old$BillCitation <- NA
hein_data_raw_old$date_of_passage <- NA
hein_data_raw_old$dates_conflict <- NA
hein_data_raw_old <- hein_data_raw_old %>% rename(secondary_date = date)
hein_data_raw_old <- hein_data_raw_old %>% rename(Title = title)

hein_data_raw_old <- hein_data_raw_old %>%
  select(action, Title, sal_volume, sal_page_start, StatuteCitation, BillCitation, congress_number, chapter, session_number, pl_no, date_of_passage, secondary_date, dates_conflict, Source, URL )

hein_data_raw_old$sal_page_start <- as.integer(hein_data_raw_old$sal_page_start)

master5 <- bind_rows(master4, hein_data_raw_old)



#add on mods post volume 112
Mods_new <- Mods%>%
  filter(sal_volume >=112)

Mods_new$congress_number <- as.integer(gsub("^(\\d+)-.*", "\\1", Mods_new$pl_no))
Mods_new <- Mods_new %>% rename(date_of_passage = Date)
Mods_new$action <- NA
Mods_new$chapter <- NA
Mods_new$session_number <- NA
Mods_new$secondary_date <- NA
Mods_new$dates_conflict <- NA

Mods_new <- Mods_new %>%
  select(action, Title, sal_volume, sal_page_start, StatuteCitation, BillCitation, congress_number, chapter, session_number, pl_no, date_of_passage, secondary_date, dates_conflict, Source, URL )

master6 <- bind_rows(master5, Mods_new)

master6 <- master6 %>%
  arrange(sal_volume, date_of_passage, secondary_date) %>%
  mutate(row_number = row_number())

master6 <- master6 %>%
  select(row_number, action, Title, sal_volume, sal_page_start, StatuteCitation, BillCitation, congress_number, chapter, session_number, pl_no, date_of_passage, secondary_date, dates_conflict, Source, URL )


write.csv(master6, file = "master_set_new.csv", row.names=FALSE)



#POPNAMES MATCHING
#add in ids to popnames
popnames <- popnames %>%
  mutate(id = row_number())

#clean popnames
popnames$citation <- gsub("\\.", "", popnames$citation)
popnames$citation <- str_extract(popnames$citation, "\\d+ Stat \\d+")
popnames$citation <- ifelse(is.na(popnames$citation), "", popnames$citation)
split_numbers <- strsplit(popnames$citation, " Stat ")


# Create new columns for the split numbers
popnames$sal_volume <- sapply(split_numbers, `[`, 1)
popnames$sal_page_start <- sapply(split_numbers, `[`, 2)

popnames$sal_volume <- ifelse(is.na(popnames$sal_volume), "", popnames$sal_volume)
popnames$sal_page_start <- ifelse(is.na(popnames$sal_page_start), "", popnames$sal_page_start)

popnames$sal_volume <- as.integer(popnames$sal_volume)
popnames$sal_page_start <- as.integer(popnames$sal_page_start)

#filter data for late period
master6_late <- master6 %>%
  filter(sal_volume >= 73)
popnames_late <- popnames %>%
  filter(sal_volume >= 73)

#rename
popnames_late <- popnames_late %>%
  rename(pl_no = "public_law")

#merge popnames and hein
late_popnames_master <- left_join(master6_late, popnames_late, by = "pl_no")

late_popnames_master %>%
  group_by(row_number) %>%
  count() %>%
  ungroup() %>%
  summarise(total.number.of.rows=n(),
            number.of.duplicated.masterids=sum(n>1), 
            total.number.of.exact.matches=sum(n==1)) 


late_popnames_master %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()  %>%
  pivot_wider(names_from=records.found,values_from=n)


duplicated_row_numbers = late_popnames_master %>%
  group_by(row_number) %>%
  count()  %>%
  filter(n>1) %>%
  pull(row_number)

late_popnames_master %>%
  filter(!(row_number %in% duplicated_row_numbers)) %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()

succesfully.matched.ids = late_popnames_master %>%
  filter(!is.na(id)) %>%
  pull(id)

unmatched_late_popnames= popnames_late %>%
  filter(!(id %in% succesfully.matched.ids))

#remove unmatched post 2016
unmatched_late_popnames_2 <- unmatched_late_popnames %>%
  filter(date <= 2016-01-01)


overmatched_late_popnames_master = late_popnames_master %>%
  filter(row_number %in% duplicated_row_numbers)





#filter data for early period
master6_early <- master6 %>%
  filter(sal_volume <= 31)
popnames_early <- popnames %>%
  filter(sal_volume <= 31)


#merge popnames and hein
early_popnames_master <- left_join(master6_early, popnames_early, by = c( "sal_volume", "sal_page_start"))

early_popnames_master %>%
  group_by(row_number) %>%
  count() %>%
  ungroup() %>%
  summarise(total.number.of.rows=n(),
            number.of.duplicated.masterids=sum(n>1),
            total.number.of.exact.matches=sum(n==1))


early_popnames_master %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()  %>%
  pivot_wider(names_from=records.found,values_from=n)


duplicated_row_numbers = early_popnames_master %>%
  group_by(row_number) %>%
  count()  %>%
  filter(n>1) %>%
  pull(row_number)

early_popnames_master %>%
  filter(!(row_number %in% duplicated_row_numbers)) %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()

succesfully.matched.ids = early_popnames_master %>%
  filter(!is.na(id)) %>%
  pull(id)

unmatched_early_popnames= popnames_early %>%
  filter(!(id %in% succesfully.matched.ids))

overmatched_early_popnames_master = early_popnames_master %>%
  filter(row_number %in% duplicated_row_numbers)





#mid period

#filter data for mid period
master6_mid <- master6 %>%
  filter(sal_volume > 31 & sal_volume < 73)
popnames_mid <- popnames %>%
  filter(sal_volume > 31 & sal_volume < 73)


#merge popnames and hein
mid_popnames_master <- left_join(master6_mid, popnames_mid, by = c( "sal_volume", "sal_page_start"))

mid_popnames_master %>%
  group_by(row_number) %>%
  count() %>%
  ungroup() %>%
  summarise(total.number.of.rows=n(),
            number.of.duplicated.heinids=sum(n>1), 
            total.number.of.exact.matches=sum(n==1))


mid_popnames_master %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()  %>%
  pivot_wider(names_from=records.found,values_from=n)

duplicated_row_numbers = mid_popnames_master %>%
  group_by(row_number) %>%
  count()  %>%
  filter(n>1) %>%
  pull(row_number)

mid_popnames_master %>%
  filter(!(row_number %in% duplicated_row_numbers)) %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()

succesfully.matched.ids = mid_popnames_master %>%
  filter(!is.na(id)) %>%
  pull(id)

unmatched_mid_popnames= popnames_mid %>%
  filter(!(id %in% succesfully.matched.ids))

overmatched_mid_popnames_master = mid_popnames_master %>%
  filter(row_number %in% duplicated_row_numbers)


#rename
unmatched_mid_popnames <- unmatched_mid_popnames %>%
  rename(pl_no = "public_law")


#match pl from unmatched with hein
mid_popnames_master_2 <- left_join(master6_mid, unmatched_mid_popnames, by = "pl_no")

mid_popnames_master_2 %>%
  group_by(row_number) %>%
  count() %>%
  ungroup() %>%
  summarise(total.number.of.rows=n(),
            number.of.duplicated.masterids=sum(n>1), 
            total.number.of.exact.matches=sum(n==1)) 


mid_popnames_master_2 %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()  %>%
  pivot_wider(names_from=records.found,values_from=n)


duplicated_row_numbers = mid_popnames_master_2 %>%
  group_by(row_number) %>%
  count()  %>%
  filter(n>1) %>%
  pull(row_number)

mid_popnames_master_2 %>%
  filter(!(row_number %in% duplicated_row_numbers)) %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()

succesfully.matched.ids = mid_popnames_master_2 %>%
  filter(!is.na(id)) %>%
  pull(id)

unmatched_mid_popnames_2 = unmatched_mid_popnames %>%
  filter(!(id %in% succesfully.matched.ids))

overmatched_mid_popnames_master_2 = mid_popnames_master_2 %>%
  filter(row_number %in% duplicated_row_numbers)

overmatched_mid_popnames_master_3 <- bind_rows(overmatched_mid_popnames_master, overmatched_mid_popnames_master_2)

# Remove duplicate rows based on row number
overmatched_mid_popnames_master_3 <- overmatched_mid_popnames_master_3 %>%
  distinct()


write.csv(unmatched_early_popnames, file = "unmatched_early_popnames.csv", row.names=FALSE)
write.csv(unmatched_mid_popnames_2, file = "unmatched_mid_popnames.csv", row.names=FALSE)
write.csv(unmatched_late_popnames_2, file = "unmatched_late_popnames.csv", row.names=FALSE)


#LATE PERIOD ADD IN POPNAMES
late_popnames_master <- left_join(master6_late, popnames_late, by = "pl_no")

late_popnames_master %>%
  group_by(row_number) %>%
  count() %>%
  ungroup() %>%
  summarise(total.number.of.rows=n(),
            number.of.duplicated.masterids=sum(n>1), 
            total.number.of.exact.matches=sum(n==1))


late_popnames_master %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()  %>%
  pivot_wider(names_from=records.found,values_from=n)

late_duplicated_row_numbers = late_popnames_master %>%
  group_by(row_number) %>%
  count()  %>%
  filter(n>1) %>%
  pull(row_number)

late_popnames_master %>%
  filter(!(row_number %in% late_duplicated_row_numbers)) %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()

overmatched_late_popnames_master = late_popnames_master %>%
  filter(row_number %in% late_duplicated_row_numbers)

nonduplicated_late = late_popnames_master %>% anti_join(overmatched_late_popnames_master)

overmatched_late_popnames_master = late_popnames_master %>% filter(row_number %in% overmatched_late_popnames_master$row_number)

late_popnames_master = left_join(master6_late, nonduplicated_late, by = "row_number")


late_popnames_master$date <- NULL
late_popnames_master$us_code <- NULL
late_popnames_master$citation <- NULL
late_popnames_master$duplicate <- NULL
late_popnames_master$id <- NULL
late_popnames_master$sal_volume.y <- NULL
late_popnames_master$sal_page_start.y <- NULL
late_popnames_master$action.y <- NULL
late_popnames_master$Title.y <- NULL
late_popnames_master$sal_volume.x <- NULL
late_popnames_master$sal_page_start.x <- NULL
late_popnames_master$StatuteCitation.y <- NULL
late_popnames_master$BillCitation.y <- NULL
late_popnames_master$congress_number.y <- NULL
late_popnames_master$chapter.y <- NULL
late_popnames_master$session_number.y <- NULL
late_popnames_master$pl_no.y <- NULL
late_popnames_master$date_of_passage.y <- NULL
late_popnames_master$secondary_date.y <- NULL
late_popnames_master$dates_conflict.y <- NULL
late_popnames_master$Source.y <- NULL
late_popnames_master$URL.y <- NULL





early_popnames_master <- left_join(master6_early, popnames_early, by = c( "sal_volume", "sal_page_start"))

early_popnames_master %>%
  group_by(row_number) %>%
  count() %>%
  ungroup() %>%
  summarise(total.number.of.rows=n(),
            number.of.duplicated.masterids=sum(n>1),
            total.number.of.exact.matches=sum(n==1))


early_popnames_master %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()  %>%
  pivot_wider(names_from=records.found,values_from=n)

early_duplicated_row_numbers = early_popnames_master %>%
  group_by(row_number) %>%
  count()  %>%
  filter(n>1) %>%
  pull(row_number)

early_popnames_master %>%
  filter(!(row_number %in% early_duplicated_row_numbers)) %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()

overmatched_early_popnames_master = early_popnames_master %>%
  filter(row_number %in% early_duplicated_row_numbers)

nonduplicated_early = early_popnames_master %>% anti_join(overmatched_early_popnames_master)

overmatched_early_popnames_master = early_popnames_master %>% filter(row_number %in% overmatched_early_popnames_master$row_number)

early_popnames_master = left_join(master6_early, nonduplicated_early, by = "row_number")


early_popnames_master$date <- NULL
early_popnames_master$us_code <- NULL
early_popnames_master$citation <- NULL
early_popnames_master$duplicate <- NULL
early_popnames_master$id <- NULL
early_popnames_master$public_law <- NULL
early_popnames_master$action.y <- NULL
early_popnames_master$Title.y <- NULL
early_popnames_master$sal_volume.y <- NULL
early_popnames_master$sal_page_start.y <- NULL
early_popnames_master$StatuteCitation.y <- NULL
early_popnames_master$BillCitation.y <- NULL
early_popnames_master$congress_number.y <- NULL
early_popnames_master$chapter.y <- NULL
early_popnames_master$session_number.y <- NULL
early_popnames_master$pl_no.y <- NULL
early_popnames_master$date_of_passage.y <- NULL
early_popnames_master$secondary_date.y <- NULL
early_popnames_master$dates_conflict.y <- NULL
early_popnames_master$Source.y <- NULL
early_popnames_master$URL.y <- NULL



#MID PERIOD ADD POPNAMES
mid_popnames_master <- left_join(master6_mid, popnames_mid, by = c( "sal_volume", "sal_page_start"))

mid_popnames_master %>%
  group_by(row_number) %>%
  count() %>%
  ungroup() %>%
  summarise(total.number.of.rows=n(),
            number.of.duplicated.masterids=sum(n>1), 
            total.number.of.exact.matches=sum(n==1))


mid_popnames_master %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()  %>%
  pivot_wider(names_from=records.found,values_from=n)

duplicated_row_numbers = mid_popnames_master %>%
  group_by(row_number) %>%
  count()  %>%
  filter(n>1) %>%
  pull(row_number)

mid_popnames_master %>%
  filter(!(row_number %in% duplicated_row_numbers)) %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()

overmatched_mid_popnames_master = mid_popnames_master %>%
  filter(row_number %in% duplicated_row_numbers)

nonduplicated_mid = mid_popnames_master %>% anti_join(overmatched_mid_popnames_master)

overmatched_mid_popnames_master = mid_popnames_master %>% filter(row_number %in% overmatched_mid_popnames_master$row_number)

mid_popnames_master = left_join(master6_mid, nonduplicated_mid, by = "row_number")



popnames_mid <- popnames_mid %>%
  rename(pl_no = "public_law")

mid_popnames_master_2 <- left_join(master6_mid, popnames_mid, by = c("pl_no"))

mid_popnames_master_2 %>%
  group_by(row_number) %>%
  count() %>%
  ungroup() %>%
  summarise(total.number.of.rows=n(),
            number.of.duplicated.masterids=sum(n>1), 
            total.number.of.exact.matches=sum(n==1))


mid_popnames_master_2 %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()  %>%
  pivot_wider(names_from=records.found,values_from=n)

duplicated_row_numbers_2 = mid_popnames_master_2 %>%
  group_by(row_number) %>%
  count()  %>%
  filter(n>1) %>%
  pull(row_number)

mid_popnames_master_2 %>%
  filter(!(row_number %in% duplicated_row_numbers_2)) %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()

overmatched_mid_popnames_master_2 = mid_popnames_master_2 %>%
  filter(row_number %in% duplicated_row_numbers_2)

nonduplicated_mid_2 = mid_popnames_master_2 %>% anti_join(overmatched_mid_popnames_master_2)

overmatched_mid_popnames_master_2 = mid_popnames_master_2 %>% filter(row_number %in% overmatched_mid_popnames_master_2$row_number)

mid_popnames_master_2 = left_join(master6_mid, nonduplicated_mid_2, by = "row_number")



mid_popnames_master$date <- NULL
mid_popnames_master$us_code <- NULL
mid_popnames_master$citation <- NULL
mid_popnames_master$duplicate <- NULL
mid_popnames_master$id <- NULL
mid_popnames_master$pl_no.y <- NULL
mid_popnames_master$action.y <- NULL
mid_popnames_master$Title.y <- NULL
mid_popnames_master$sal_volume.y <- NULL
mid_popnames_master$sal_page_start.y <- NULL
mid_popnames_master$StatuteCitation.y <- NULL
mid_popnames_master$BillCitation.y <- NULL
mid_popnames_master$congress_number.y <- NULL
mid_popnames_master$chapter.y <- NULL
mid_popnames_master$session_number.y <- NULL
mid_popnames_master$date_of_passage.y <- NULL
mid_popnames_master$secondary_date.y <- NULL
mid_popnames_master$dates_conflict.y <- NULL
mid_popnames_master$Source.y <- NULL
mid_popnames_master$URL.y <- NULL


mid_popnames_master_2$date <- NULL
mid_popnames_master_2$us_code <- NULL
mid_popnames_master_2$citation <- NULL
mid_popnames_master_2$duplicate <- NULL
mid_popnames_master_2$id <- NULL
mid_popnames_master_2$sal_volume.y <- NULL
mid_popnames_master_2$sal_page_start.y <- NULL
mid_popnames_master_2$action.y <- NULL
mid_popnames_master_2$Title.y <- NULL
mid_popnames_master_2$sal_volume.x <- NULL
mid_popnames_master_2$sal_page_start.x <- NULL
mid_popnames_master_2$StatuteCitation.x <- NULL
mid_popnames_master_2$BillCitation.x <- NULL
mid_popnames_master_2$congress_number.x <- NULL
mid_popnames_master_2$chapter.x <- NULL
mid_popnames_master_2$session_number.x <- NULL
mid_popnames_master_2$date_of_passage.x <- NULL
mid_popnames_master_2$secondary_date.x <- NULL
mid_popnames_master_2$dates_conflict.x <- NULL
mid_popnames_master_2$Source.x <- NULL
mid_popnames_master_2$URL.x <- NULL
mid_popnames_master_2$sal_volume.y.y <- NULL
mid_popnames_master_2$sal_page_start.y.y <- NULL
mid_popnames_master_2$action.x <- NULL
mid_popnames_master_2$Title.x <- NULL
mid_popnames_master_2$pl_no.y <- NULL
mid_popnames_master_2$pl_no.x <- NULL


mid_popnames_master_3 <- left_join(mid_popnames_master, mid_popnames_master_2, by = c("row_number"))

mid_popnames_master_3$action <- NULL
mid_popnames_master_3$Title <- NULL
mid_popnames_master_3$chapter <- NULL
mid_popnames_master_3$sal_volume.x.x <- NULL
mid_popnames_master_3$sal_page_start.x.x <- NULL
mid_popnames_master_3$StatuteCitation <- NULL
mid_popnames_master_3$BillCitation <- NULL
mid_popnames_master_3$congress_number <- NULL
mid_popnames_master_3$session_number <- NULL
mid_popnames_master_3$pl_no <- NULL
mid_popnames_master_3$date_of_passage <- NULL
mid_popnames_master_3$secondary_date <- NULL
mid_popnames_master_3$dates_conflict <- NULL
mid_popnames_master_3$Source <- NULL
mid_popnames_master_3$URL <- NULL
mid_popnames_master_3$popular_name.y <- NULL



#MERGING ALL
mid_popnames_master_3 <- mid_popnames_master_3 %>%
  rename(popular_name = "popular_name.x")
mid_popnames_master_3 <- mid_popnames_master_3 %>%
 rename(sal_volume = "sal_volume.x")
mid_popnames_master_3 <- mid_popnames_master_3 %>%
rename(sal_page_start = "sal_page_start.x")
early_popnames_master <- early_popnames_master %>%
  rename(sal_volume = "sal_volume.x")
early_popnames_master <- early_popnames_master %>%
  rename(sal_page_start = "sal_page_start.x")

master7 <- rbind(early_popnames_master, mid_popnames_master_3, late_popnames_master)
colnames(master7) <- sub("\\.x$", "", colnames(master7))

write.csv(master7, file = "master_set_7.csv", row.names=FALSE)











#APS EDITING
raw_data <- read.csv("significant_legislation_database.csv")
corrections <- read.csv("significant_legislation_database_2.csv")


differences2 <- inner_join(corrections, raw_data, by = "id") %>%
  filter(pl.x != pl.y)

differences3 <- inner_join(corrections, raw_data, by = "id") %>%
  filter(congress.x != congress.y)

differences2 <- differences2 %>%
  select(-ends_with(".y"))
colnames(differences2) <- sub("\\.x$", "", colnames(differences2))
differences3 <- differences3 %>%
  select(-ends_with(".y"))
colnames(differences3) <- sub("\\.x$", "", colnames(differences3))


APS_almost_fixed <- bind_rows(raw_data %>% filter(!id %in% differences2$id), differences2)
APS_fixed <- bind_rows(APS_almost_fixed %>% filter(!id%in% differences3$id), differences3)

write.csv(APS_fixed,file="APS_fixed.csv",row.names=F)

APS_fixed <- read.csv("APS_fixed.csv")

#clean APS late

APS_fixed$pl <- gsub("PL", "", APS_fixed$pl)
APS_fixed$pl <- gsub(" ", "", APS_fixed$pl)
APS_fixed$pl <- gsub("N/A", "", APS_fixed$pl)
APS_fixed$pl <- gsub("Failed", "", APS_fixed$pl)
APS_fixed$pl <- gsub("NONE", "", APS_fixed$pl)
APS_fixed$pl <- gsub("_", "-", APS_fixed$pl)

APS_fixed <- APS_fixed %>%
  mutate(pl = case_when(
    grepl("^\\d+-\\d+", pl) ~ pl,  # Keep existing format
    TRUE ~ paste(congress, pl, sep = "-")  # Add congress number and dash
  ))

APS_fixed <- APS_fixed %>%
  mutate(pl = ifelse(grepl("^\\d+-$", pl), "", pl))

APS_fixed_late <- APS_fixed %>%
  filter(congress >= 85)
master6_late <- master6 %>%
  filter(congress_number >=85)

APS_fixed_late <- APS_fixed_late %>%
  rename(pl_no = "pl")

#merge sigleg and hein
late_APS_master <- left_join(master6_late, APS_fixed_late, by = "pl_no")

late_APS_master %>%
  group_by(row_number) %>%
  count() %>%
  ungroup() %>%
  summarise(total.number.of.rows=n(),
            number.of.duplicated.heinids=sum(n>1),
            total.number.of.exact.matches=sum(n==1)) 

late_APS_master %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()  %>%
  pivot_wider(names_from=records.found,values_from=n)


duplicated_row_numbers = late_APS_master %>%
  group_by(row_number) %>%
  count()  %>%
  filter(n>1) %>%
  pull(row_number)

late_APS_master %>%
  filter(!(row_number %in% duplicated_row_numbers)) %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()


succesfully.matched.ids = late_APS_master %>%
  filter(!is.na(id)) %>%
  pull(id)

unmatched_late_APS=APS_fixed_late %>%
  filter(!(id %in% succesfully.matched.ids))

unmatched_late_APS <- unmatched_late_APS %>%
  mutate(pl_no = ifelse(pl_no == "", NA, pl_no))

unmatched_late_APS_2 <- unmatched_late_APS %>%
  filter(!is.na(pl_no))

overmatched_late_APS = late_APS_master %>%
  filter(row_number %in% duplicated_row_numbers)




#clean aps early
APS_fixed$pl <- gsub("\\.", "", APS_fixed$pl)
APS_fixed$pl <- str_extract(APS_fixed$pl, "\\d+ Stat \\d+")
APS_fixed$pl <- ifelse(is.na(APS_fixed$pl), "", APS_fixed$pl)
split_numbers <- strsplit(APS_fixed$pl, " Stat ")

# Create new columns for the split numbers
APS_fixed$sal_volume <- sapply(split_numbers, `[`, 1)
APS_fixed$sal_page_start <- sapply(split_numbers, `[`, 2)

APS_fixed$sal_volume <- ifelse(is.na(APS_fixed$sal_volume), "", APS_fixed$sal_volume)
APS_fixed$sal_page_start <- ifelse(is.na(APS_fixed$sal_page_start), "", APS_fixed$sal_page_start)

APS_fixed$sal_volume <- as.integer(APS_fixed$sal_volume)
APS_fixed$sal_page_start <- as.integer(APS_fixed$sal_page_start)

#filtered data for early period

APS_fixed_early <- APS_fixed %>%
  filter(sal_volume <= 31)
APS_fixed_early <- APS_fixed_early %>%
  rename(congress_number = "congress")


#merge sigleg and hein
early_APS_master <- left_join(master6_early, APS_fixed_early, by = c("congress_number", "sal_page_start", "sal_volume"))

early_APS_master %>%
  group_by(row_number) %>%
  count() %>%
  ungroup() %>%
  summarise(total.number.of.rows=n(),
            number.of.duplicated.heinids=sum(n>1), 
            total.number.of.exact.matches=sum(n==1)) 

early_APS_master %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()  %>%
  pivot_wider(names_from=records.found,values_from=n)


duplicated_row_numbers = early_APS_master %>%
  group_by(row_number) %>%
  count()  %>%
  filter(n>1) %>%
  pull(row_number)

early_APS_master %>%
  filter(!(row_number %in% duplicated_row_numbers)) %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()


succesfully.matched.ids = early_APS_master %>%
  filter(!is.na(id)) %>%
  pull(id)

unmatched_early_APS = APS_fixed_early %>%
  filter(!(id %in% succesfully.matched.ids))

overmatched_early_APS_master = early_APS_master %>%
  filter(row_number %in% duplicated_row_numbers)

#remove data without SAL
unmatched_early_APS_2 <- unmatched_early_APS_master[complete.cases(unmatched_early_APS_master$sal_volume), ]










#filtered data for middle period

APS_fixed_mid <- APS_fixed%>%
  filter(congress >= 57 & congress <=85)


#rename
APS_fixed_mid <- APS_fixed_mid%>%
  rename(pl_no = "pl")
APS_fixed_mid <- APS_fixed_mid %>%
  rename(congress_number = "congress")


# Extract numeric values and split when the pattern matches
pattern_matched <- grepl("\\d+ Stat \\d+", APS_fixed_mid$legislation)
APS_fixed_mid$sal_volume <- NA
APS_fixed_mid$sal_page_start <- NA
APS_fixed_mid$sal_volume[pattern_matched] <- str_extract(APS_fixed_mid$legislation[pattern_matched], "\\d+(?= Stat)")
APS_fixed_mid$sal_page_start[pattern_matched] <- str_extract(APS_fixed_mid$legislation[pattern_matched], "(?<=Stat )\\d+")

#clean sigleg PL
APS_fixed_mid$pl_no <- gsub("\\.", "", APS_fixed_mid$pl_no)
APS_fixed_mid$pl_no <- gsub("N/A", "", APS_fixed_mid$pl_no)
APS_fixed_mid$pl_no <- gsub("PL ", "", APS_fixed_mid$pl_no)
APS_fixed_mid$pl_no <- gsub("PR ", "", APS_fixed_mid$pl_no)
APS_fixed_mid$pl_no <- ifelse(grepl("^\\d+$", APS_fixed_mid$pl_no),
                                 paste(APS_fixed_mid$congress, "-", APS_fixed_mid$pl_no),
                           APS_fixed_mid$pl_no)
APS_fixed_mid$pl_no <- gsub(" - ", "-", APS_fixed_mid$pl_no)
APS_fixed_mid$pl_no <- gsub("^(\\d+)-(?![\\d])", "", APS_fixed_mid$pl_no, perl = TRUE)


#merge mid sigleg and hein
mid_APS_master <- left_join(master6_mid, APS_fixed_mid, by = "pl_no")

mid_APS_master %>%
  group_by(row_number) %>%
  count() %>%
  ungroup() %>%
  summarise(total.number.of.rows=n(),
            number.of.duplicated.heinids=sum(n>1),
            total.number.of.exact.matches=sum(n==1))

mid_APS_master %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()  %>%
  pivot_wider(names_from=records.found,values_from=n)


duplicated_row_numbers = mid_APS_master %>%
  group_by(row_number) %>%
  count()  %>%
  filter(n>1) %>%
  pull(row_number)

mid_APS_master %>%
  filter(!(row_number %in% duplicated_row_numbers)) %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()

succesfully.matched.ids = mid_APS_master %>%
  filter(!is.na(id)) %>%
  pull(id)

unmatched_mid_APS= APS_fixed_mid %>%
  filter(!(id %in% succesfully.matched.ids))

unmatched_mid_APS <- unmatched_mid_APS %>%
  mutate(pl_no = ifelse(pl_no == "", NA, pl_no))

unmatched_mid_APS_2 <- unmatched_mid_APS %>%
  filter(!is.na(pl_no))


overmatched_mid_APS = mid_APS_master %>%
  filter(row_number %in% duplicated_row_numbers)




APS_fixed_mid$sal_volume <- as.integer(APS_fixed_mid$sal_volume)
APS_fixed_mid$sal_page_start <- as.integer(APS_fixed_mid$sal_page_start)
mid_APS_master_2 <- left_join(master6_mid, APS_fixed_mid, by = c( "sal_volume", "sal_page_start"))

mid_APS_master_2 %>%
  group_by(row_number) %>%
  count() %>%
  ungroup() %>%
  summarise(total.number.of.rows=n(),
            number.of.duplicated.heinids=sum(n>1),
            total.number.of.exact.matches=sum(n==1))


mid_APS_master_2 %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()  %>%
  pivot_wider(names_from=records.found,values_from=n)


duplicated_row_numbers = mid_APS_master_2 %>%
  group_by(row_number) %>%
  count()  %>%
  filter(n>1) %>%
  pull(row_number)

mid_APS_master_2 %>%
  filter(!(row_number %in% duplicated_row_numbers)) %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()

succesfully.matched.ids = mid_APS_master_2 %>%
  filter(!is.na(id)) %>%
  pull(id)

unmatched_mid_APS_3= APS_fixed_mid %>%
  filter(!(id %in% succesfully.matched.ids))

overmatched_mid_APS_3 = mid_APS_master_2 %>%
  filter(row_number %in% duplicated_row_numbers)


#combined rows from undermatched both by pl and salvolume
unmatched_mid_APS$sal_volume <- as.integer(unmatched_mid_APS$sal_volume)
unmatched_mid_APS$sal_page_start <- as.integer(unmatched_mid_APS$sal_page_start)

unmatched_mid_APS_3 <- unmatched_mid_APS_3 %>%
  mutate(pl_no = ifelse(pl_no == "", NA, pl_no))

unmatched_mid_APS_4 <- inner_join(unmatched_mid_APS, unmatched_mid_APS_3)
unmatched_mid_APS_5 <- unmatched_mid_APS_4 %>%
  filter(!is.na(pl_no))
unmatched_mid_APS_6 <- unmatched_mid_APS_4 %>%
  filter(!is.na(sal_volume))



#USLeg matching
#correcting typos
usleg <- read.csv("US-Legislative-public_laws_20.1_2.csv")
corrections_usleg <- read.csv("US-Legislative-public_laws_20.1_2_copy.csv")

differences_us <- anti_join(corrections_usleg, usleg, by = c("public_law_no", "id"))

usleg_fixed = bind_rows(
  usleg %>% anti_join(corrections_usleg, by="id"),
  corrections_usleg)

write.csv(usleg_fixed,file="USLEG_fixes.csv",row.names=F)




#late period

#filter data for late period
late_usleg <- usleg_fixed %>%
  filter(congress >= 85)


#clean usleg
late_usleg <- late_usleg %>%
  rename(pl_no = "public_law_no")


# Convert columns to character type
late_usleg$pl_no <- as.character(late_usleg$pl_no)
late_usleg$congress <- as.character(late_usleg$congress)

# Function to add dash to pl_no
add_dash <- function(pl_no, congress) {
  if (startsWith(pl_no, congress)) {
    new_pl_no <- paste(substring(pl_no, 1, nchar(congress)), 
                       substring(pl_no, nchar(congress) + 1), 
                       sep = "-")
    return(new_pl_no)
  }
  
  return(pl_no)
}

# Apply add_dash function to modify pl_no column
late_usleg$pl_no <- mapply(add_dash, late_usleg$pl_no, late_usleg$congress)


#remove all zeros after the dash
remove_zeros <- function(x) {
  gsub("(?<=-)0+(?=[1-9])", "", x, perl = TRUE)
}

# Apply the function to the pl_no column
late_usleg$pl_no <- remove_zeros(late_usleg$pl_no)


#merge by PL number
late_usleg_master <- left_join(master6_late, late_usleg, by = "pl_no")

#matching process
late_usleg_master %>%
  group_by(row_number) %>%
  count() %>%
  ungroup() %>%
  summarise(total.number.of.rows=n(),
            number.of.duplicated.heinids=sum(n>1), 
            total.number.of.exact.matches=sum(n==1)) 

late_usleg_master %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()  %>%
  pivot_wider(names_from=records.found,values_from=n)

duplicated_row_numbers = late_usleg_master %>%
  group_by(row_number) %>%
  count()  %>%
  filter(n>1) %>%
  pull(row_number)

late_usleg_master %>%
  filter(!(row_number %in% duplicated_row_numbers)) %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()


succesfully.matched.ids = late_usleg_master %>%
  filter(!is.na(id)) %>%
  pull(id)

unmatched_late_usleg= late_usleg %>%
  filter(!(id %in% succesfully.matched.ids))

unmatched_late_usleg_2 = unmatched_late_usleg %>%
  filter(year < 2016) 

overmatched_late_usleg = late_usleg_master %>%
  filter(row_number %in% duplicated_row_numbers)




#match mid period (starting 1948)
usleg_fixed <- usleg_fixed %>%
  rename(pl_no = "public_law_no")

#filter data for mid period
master6_mid_2 <- master6_mid %>%
  filter(congress_number < 85 & congress_number >=80)
mid_usleg <- usleg_fixed %>%
  filter(congress < 85)


# Convert columns to character type
mid_usleg$pl_no <- as.character(mid_usleg$pl_no)
mid_usleg$congress <- as.character(mid_usleg$congress)

# Function to add dash to pl_no
add_dash <- function(pl_no, congress) {
  if (startsWith(pl_no, congress)) {
    new_pl_no <- paste(substring(pl_no, 1, nchar(congress)), 
                       substring(pl_no, nchar(congress) + 1), 
                       sep = "-")
    return(new_pl_no)
  }
  
  return(pl_no)
}

# Apply add_dash function to modify pl_no column
mid_usleg$pl_no <- mapply(add_dash, mid_usleg$pl_no, mid_usleg$congress)


#remove all zeros after the dash
remove_zeros <- function(x) {
  gsub("(?<=-)0+(?=[1-9])", "", x, perl = TRUE)
}

# Apply the function to the pl_no column
mid_usleg$pl_no <- remove_zeros(mid_usleg$pl_no)

#merge by PL number
mid_usleg_master <- left_join(master6_mid_2, mid_usleg, by = "pl_no")

#matching process
mid_usleg_master %>%
  group_by(row_number) %>%
  count() %>%
  ungroup() %>%
  summarise(total.number.of.rows=n(),
            number.of.duplicated.heinids=sum(n>1), 
            total.number.of.exact.matches=sum(n==1)) 

mid_usleg_master %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()  %>%
  pivot_wider(names_from=records.found,values_from=n)


duplicated_row_numbers = mid_usleg_master %>%
  group_by(row_number) %>%
  count()  %>%
  filter(n>1) %>%
  pull(row_number)

mid_usleg_master %>%
  filter(!(row_number %in% duplicated_row_numbers)) %>%
  group_by(records.found=ifelse(is.na(id),'Missed','Matched')) %>%
  count()


succesfully.matched.ids = mid_usleg_master %>%
  filter(!is.na(id)) %>%
  pull(id)

unmatched_mid_usleg= mid_usleg %>%
  filter(!(id %in% succesfully.matched.ids))

overmatched_mid_usleg = mid_usleg_master %>%
  filter(row_number %in% duplicated_row_numbers)








