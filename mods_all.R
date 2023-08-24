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
library(XML)
library(xml2)
library(data.table)




# Define a vector of URLs for the XML files
urls <- c(
  "https://www.govinfo.gov/metadata/pkg/STATUTE-65/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-66/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-67/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-68/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-69/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-70/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-71/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-72/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-73/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-74/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-75/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-76/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-77/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-78/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-79/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-80/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-81/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-82/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-83/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-84/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-85/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-86/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-87/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-88/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-89/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-90/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-91/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-92/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-93/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-94/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-95/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-96/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-97/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-98/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-99/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-100/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-101/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-102/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-103/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-104/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-105/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-106/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-107/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-108/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-109/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-110/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-111/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-112/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-113/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-114/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-115/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-116/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-117/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-118/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-119/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-120/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-121/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-122/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-123/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-124/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-125/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-126/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-127/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-128/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-129/mods.xml",
  "https://www.govinfo.gov/metadata/pkg/STATUTE-130/mods.xml"
  # Add more URLs as needed
)

# Specify a directory where you want to save the downloaded files
download_dir <- "my_download_directory"

# Create the download directory if it doesn't exist
if (!file.exists(download_dir)) {
  dir.create(download_dir)
}

# Initialize a counter for generating unique file names
counter <- 65

# Loop through the URLs and download the XML files
for (url in urls) {
  # Extract the file name from the URL
  file_name <- basename(url)
  
  # Generate a unique file name by adding a counter
  unique_file_name <- paste0(counter, "_", file_name)
  
  # Set the complete path to save the downloaded file
  save_path <- file.path(download_dir, unique_file_name)
  
  # Download the file from the URL
  download.file(url, save_path, mode = "wb")
  
  cat("Downloaded:", unique_file_name, "\n")
  
  # Increment the counter for the next file
  counter <- counter + 1
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

# Define a vector of downloaded file paths
downloaded_files <- c(
  "my_download_directory/65_mods.xml",
  "my_download_directory/66_mods.xml",
  "my_download_directory/67_mods.xml",
  "my_download_directory/68_mods.xml",
  "my_download_directory/69_mods.xml",
  "my_download_directory/70_mods.xml",
  "my_download_directory/71_mods.xml",
  "my_download_directory/72_mods.xml",
  "my_download_directory/73_mods.xml",
  "my_download_directory/74_mods.xml",
  "my_download_directory/75_mods.xml",
  "my_download_directory/76_mods.xml",
  "my_download_directory/77_mods.xml",
  "my_download_directory/78_mods.xml",
  "my_download_directory/79_mods.xml",
  "my_download_directory/80_mods.xml",
  "my_download_directory/81_mods.xml",
  "my_download_directory/82_mods.xml",
  "my_download_directory/83_mods.xml",
  "my_download_directory/84_mods.xml",
  "my_download_directory/85_mods.xml",
  "my_download_directory/86_mods.xml",
  "my_download_directory/87_mods.xml",
  "my_download_directory/88_mods.xml",
  "my_download_directory/89_mods.xml",
  "my_download_directory/90_mods.xml",
  "my_download_directory/91_mods.xml",
  "my_download_directory/92_mods.xml",
  "my_download_directory/93_mods.xml",
  "my_download_directory/94_mods.xml",
  "my_download_directory/95_mods.xml",
  "my_download_directory/96_mods.xml",
  "my_download_directory/97_mods.xml",
  "my_download_directory/98_mods.xml",
  "my_download_directory/99_mods.xml",
  "my_download_directory/100_mods.xml",
  "my_download_directory/101_mods.xml",
  "my_download_directory/102_mods.xml",
  "my_download_directory/103_mods.xml",
  "my_download_directory/104_mods.xml",
  "my_download_directory/105_mods.xml",
  "my_download_directory/106_mods.xml",
  "my_download_directory/107_mods.xml",
  "my_download_directory/108_mods.xml",
  "my_download_directory/109_mods.xml",
  "my_download_directory/110_mods.xml",
  "my_download_directory/111_mods.xml",
  "my_download_directory/112_mods.xml",
  "my_download_directory/113_mods.xml",
  "my_download_directory/114_mods.xml",
  "my_download_directory/115_mods.xml",
  "my_download_directory/116_mods.xml",
  "my_download_directory/117_mods.xml",
  "my_download_directory/118_mods.xml",
  "my_download_directory/119_mods.xml",
  "my_download_directory/120_mods.xml",
  "my_download_directory/121_mods.xml",
  "my_download_directory/122_mods.xml",
  "my_download_directory/123_mods.xml",
  "my_download_directory/124_mods.xml",
  "my_download_directory/125_mods.xml",
  "my_download_directory/126_mods.xml",
  "my_download_directory/127_mods.xml",
  "my_download_directory/128_mods.xml",
  "my_download_directory/129_mods.xml",
  "my_download_directory/130_mods.xml"
)

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

# Iterate through downloaded files and extract data
for (file_path in downloaded_files) {
  extracted_data <- extract_data_from_xml(file_path)
  combined_data <- rbind(combined_data, extracted_data)
}

# Print the combined data table
print(combined_data)

write.csv(combined_data, file = "mods_all.csv", row.names = FALSE)






