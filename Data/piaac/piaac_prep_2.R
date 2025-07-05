# Second wave data appending

# Load required libraries
library(tidyverse)

# Step 1: Downloaded manually all of the 2nd wave .csv files (17.12.2024)
# https://www.oecd.org/en/data/datasets/piaac-2nd-cycle-database.html#data


# Step 2: Identify and combine ...2.csv files
file_list <- list.files(path = here::here("Data", "piaac", "cycle2csvs"), pattern = "2\\.csv$", full.names = TRUE)

combined_data <- file_list %>%
  lapply(read_delim, 
         delim = ";", 
         col_types = cols(.default = col_character())) %>%  
  bind_rows()


# Step 3: Write final output to a file
write_csv(combined_data, here::here("Data", "piaac_combined_2.csv"))

rm(file_list, combined_data)
