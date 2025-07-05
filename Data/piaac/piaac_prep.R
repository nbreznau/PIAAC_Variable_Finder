# First wave data appending 


# Load required libraries
library(rvest)
library(dplyr)
library(purrr)
library(readr)
library(haven)

# Step 1: Scrape the webpage to get all .csv file links
url <- "https://webfs.oecd.org/piaac/puf-data/CSV"
webpage <- read_html(url)

# Get the file links from the webpage
csv_links <- webpage %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  grep("\\.csv$", ., value = TRUE) # Filter only .csv links

# Combine the links with the base URL
csv_links <- paste0("https://webfs.oecd.org", csv_links)

# Step 2: Create a function to download and read each CSV file
read_and_convert_to_char <- function(link) {
  # Download the file and read it
  df <- read_csv(link, col_types = cols(.default = "c")) # Read all as character
  return(df)
}

# Step 3: Download all CSV files and store them in a list of dataframes
all_data <- map(csv_links, read_and_convert_to_char)

# Step 4: Use bind_rows to append data together
final_data <- bind_rows(all_data, .id = "source")

# This file is age for the countries with missing in the PUF for DE, SE and GB 
# It needs to be taken from PIAAC SUFs with permission from PIAAC. Therefore we cannot share it.
# ZA5845_v2-0-0.dta

df_age <- read_dta(here::here("Data", "piaac", "DEGBSE.2.dta")) %>%
  select(CNTRYID, SEQID, AGE_R) %>%
  mutate(age_x = AGE_R,
         CNTRYID = as.character(CNTRYID),
         SEQID = as.character(SEQID)) %>%
  select(-AGE_R)

final_data <- final_data %>%
  left_join(df_age, by = c("CNTRYID", "SEQID"))

# for some reason a column gets created at the end of the file that has a variable label that is all variables, remove it
final_data <- final_data[ , - (ncol(final_data) - 1)]

# Step 5: Write final output to a file
write_csv(final_data, here::here("Data", "piaac_combined.csv"))

rm(all_data, final_data)
