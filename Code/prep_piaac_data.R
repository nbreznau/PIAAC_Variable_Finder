# auto checker/loader of packages

packages <- c('rvest', 
              'tidyverse',
              'purrr', 
              'here')

pacman::p_load(packages, character.only = T)


## Data

# The piaac individual country files are downloaded from https://webfs.oecd.org/piaac/puf-data/CSV/ 
# The user needs to fill in a small survey to access these PUFs
# THIS WILL NOT RUN WITHOUT DOING THIS
# This results in the file /Data/piaac_combined.csv and /Data/piaac_combined2.csv which are too large to share on Github


source(here("Data", "piaac", "piaac_prep.R"))

source(here("Data", "piaac", "piaac_prep_2.R"))

