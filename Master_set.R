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
values_to_exclude <- c("Private Laws", "Concurrent Resolutions", "Reorganization Plan", "Back Matter", "Constitutional Amendment", "Unknown", "Sub-Volume")

# Filter out rows where partName is in the list of values to exclude
mods_all_2_a <- mods_all_2[!mods_all_2$partName %in% values_to_exclude, ]

# Find the rows in mods_all_2 that have differing "Date" values
differences <- mods_all_2_a %>%
  anti_join(mods_all_a, by = "Title") %>%
  select(Title, Date)  # Select only "Title" and "Date" columns

# Combine the differing "Date" values with mods_all_2
mods_all_public <- mods_all_2_a %>%
  left_join(differences, by = "Title", suffix = c("_2", "_1")) %>%
  mutate(Date = ifelse(!is.na(Date_1), Date_1, Date_2)) %>%
  select(-Date_1, -Date_2)  # Remove the temporary "Date_1" and "Date_2" columns


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
