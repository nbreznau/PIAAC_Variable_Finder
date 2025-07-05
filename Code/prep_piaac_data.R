# auto checker/loader of packages

packages <- c('rvest', 
              'tidyverse',
              'purrr', 
              'here')

pacman::p_load(packages, character.only = T)


## Data

# The piaac individual country files are scraped and appended from https://webfs.oecd.org/piaac/puf-data/CSV/ using the /Data/piaac/piaac_prep.R routine.

# This results in the file /Data/piaac_combined.csv which is too large to share on Github


source(here::here("Data", "piaac", "piaac_prep.R"))

source(here::here("Data", "piaac", "piaac_prep_2.R"))

