#libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(jsonlite)
library(stringr)
library(XML)
library(xml2)
library(data.table)
library(here)
library(progress)

options(pillar.width=Inf)
# Specify a directory where you want to save the downloaded files
download_dir <- here("build/mods/")

# Create the download directory if it doesn't exist
if (!file.exists(download_dir)) {
  dir.create(download_dir)
}

# Loop through the URLs and download the XML files
for (volume in 65:130) {
  #set the url
  url = sprintf("https://www.govinfo.gov/metadata/pkg/STATUTE-%s/mods.xml",
                volume)
  
  # Extract the file name from the URL
  file_name <- basename(url)

  # Generate a unique file name by adding a counter
  unique_file_name <- paste0(volume, "_", file_name)
  
  # Set the complete path to save the downloaded file
  save_path <- file.path(download_dir, unique_file_name)
  
  # Download the file from the URL
  if (!file.exists(save_path)) {
    download.file(url, save_path, mode = "wb")
  }
  cat("Downloaded:", unique_file_name, "\n")
}

# Optional: List the downloaded files in the directory
downloaded_files <- list.files(download_dir)
cat("Downloaded files:", downloaded_files, "\n")


# Define a function to extract data from an XML file
extract_data_from_xml <- function(xml_file_path) {
  # Convert file path into xml object
  mods_parsed <- read_xml(xml_file_path)
  # Remove namespace information from object
  mods_parsed <- xml_ns_strip(mods_parsed)
  # This pulls the set of nodes
  nodes <- xml_find_all(mods_parsed, 'relatedItem[@type="constituent"]')
  
 

  
  # Create an empty data table to store the results
  mods <- data.table(
    Title = character(0),
    StatuteCitation = character(0),
    Date = character(0),
    PublicLaw = character(0),
    BillCitation = character(0),
    partName = character(0),
   URL = character(0)
  )
  
  # Iterate through each 'relatedItem' node and extract information
  for (node in nodes) {
    title <- xml_text(xml_find_first(node, 'titleInfo/title'))
    statute_citation <- xml_text(xml_find_first(node, 'identifier[@type="Statute citation"]'))
    public_law <- xml_text(xml_find_first(node, 'identifier[@type="public law citation"]'))
    date <- xml_text(xml_find_first(node, 'extension/granuleDate'))
    bill <- xml_text(xml_find_first(node, 'relatedItem[@type="isReferencedBy"]/identifier[@type="Congressional Bill citation"]'))
    part <- xml_text(xml_find_first(node, 'titleInfo/partName'))
    url <- xml_text(xml_find_first(node, './relatedItem[@type="otherFormat"]/@xlink:href'))

    
    # Add the extracted information to the data table
    mods <- rbind(mods, data.table(
      Title = title,
      StatuteCitation = statute_citation,
      PublicLaw = public_law,
      Date = date,
      BillCitation = bill,
      partName = part,
      URL = url
    ))
  }
  
  return(mods)
}


# Create an empty data table to store the combined results
combined_data <- data.table(
  Title = character(0),
  StatuteCitation = character(0),
  Date = character(0),
  PublicLaw = character(0),
  BillCitation = character(0),
  partName = character(0),
  URL = character(0)
)

pb <- progress_bar$new(
  format = "[:bar] :percent ETA: :eta",
  total = length(downloaded_files)
)

# Iterate through downloaded files and extract data
for (file_path in here(download_dir, downloaded_files)) {
  extracted_data <- extract_data_from_xml(file_path)
  combined_data <- rbind(combined_data, extracted_data)
  pb$tick()
}

# Print the combined data table
print(combined_data)

write.csv(combined_data, file = here("build","mods_raw.csv"), row.names = FALSE)

mods_raw = read_csv( here("build","mods_raw.csv"))


#  Undergrad RA Jamie Gall originally made a file called mods_all_2.csv 
#  where the hard fixes were coded. This is the code that extracts her hard fixes
#  Eventually I moved to what I view as a more maintainable system.

# mods_fixed <- read.csv("mods_all_2.csv")
# fixed_rows = mods_raw %>% inner_join(mods_fixed,by="URL") %>%
#   filter(Date.x!=Date.y | 
#          StatuteCitation.x != StatuteCitation.y | 
#          Title.x!=Title.y | 
#          PublicLaw.x!=PublicLaw.y | 
#          StatuteCitation.x !=StatuteCitation.y | 
#          partName.x!=partName.y) %>%
#   pull(URL)
# 
# 
# mods_fixed %>%
#   filter(URL %in% fixed_rows) %>%
#   write.csv(here("build","mods_hard_fixes.csv"),row.names=F)

# Update with hard fixes
mods_hard_fixes = read_csv(here("build","mods_hard_fixes.csv"))
mods_raw$provenance = "Raw"
mods = bind_rows(mods_raw %>% filter(!(URL %in% mods_hard_fixes$URL)),
                 mods_hard_fixes %>% mutate(provenance="Fixed"))

#subset mod data
excluded_parts <- c("Private Laws", 
                    "Concurrent Resolutions",
                    "Reorganization Plan", 
                    "Back Matter",
                    "Constitutional Amendment", 
                    "Unknown", 
                    "Sub-Volume")

mods = mods %>%
  filter(!(partName %in% excluded_parts))

# tidying the mods file to make it more like the heinonline file
mods$PublicLaw <- gsub("Public Law ", "", mods$PublicLaw)
mods <- mods %>%
  rename(pl_no = "PublicLaw")

mods$StatuteCitation <- str_extract(mods$StatuteCitation, "\\d+ Stat. \\d+")
mods$StatuteCitation <- ifelse(is.na(mods$StatuteCitation), "", mods$StatuteCitation)

mods$sal_volume = str_extract(mods$StatuteCitation, "(\\d+) Stat. (\\d+)",group=1)
mods$sal_page_start <- str_extract(mods$StatuteCitation, 
                                   "(\\d+) Stat. (\\d+)",group=2)

mods = mods %>% mutate(sal_volume=as.integer(sal_volume),
                sal_page_start=as.integer(sal_page_start))

write.csv(mods,file=here("build","mods.csv"))

