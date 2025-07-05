# First wave data appending 


# Load required libraries
library(rvest)
library(dplyr)
library(purrr)
library(readr)
library(haven)

# Step 1: Downloaded manually all of the 1st wave .csv files (17.12.2024)
# You will need to get permission from PIAAC, just have to enter basic info
# https://webfs.oecd.org/piaac/index.html


# Step 2: Identify and combine ...2.csv files
file_list <- list.files(path = here::here("Data", "piaac", "cycle1csvs"), pattern = "1\\.csv$", full.names = TRUE)

combined_data <- file_list %>%
  lapply(read_delim, 
         delim = ";", 
         col_types = cols(.default = col_character())) %>%  
  bind_rows()


# Step 3: Write final output to a file
write_csv(combined_data, here::here("Data", "piaac_combined.csv"))

rm(file_list, combined_data)
