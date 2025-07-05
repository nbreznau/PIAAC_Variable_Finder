# from PIAAC 2nd Cycle R documentation
df_label_2 <- readRDS(here("Data", "label_data_PIAAC.RDS")) %>%
  mutate(generic_label_en = label,
         responses_en = value_labels,
         cycle = "Cycle 2") %>%
  select(variable, generic_label_en, responses_en, cycle)


start_vars <- c("PVLIT1", "A2_N02")
end_vars <- c("PVAPS10", "WRITWORKC2_WLE_CA")
start_indices <- match(start_vars, df_label_2$variable)
end_indices <- match(end_vars, df_label_2$variable)

# Create a filter to keep all variables in the ranges
keep_indices <- c(
  which(df_label_2$variable == "CNTRYID"),  # Always keep CNTRYID
  unlist(mapply(`:`, start_indices, end_indices)) # Include ranges
)

# Subset the dataframe
df_label_2 <- df_label_2[keep_indices, ]


# from PIAAC 1st Cycle Stata code saved as .csv
df_label_1 <- read_csv(here("Data", "label_data_PIAAC_Cy1.csv"))
colnames(df_label_1) <- "variable"

df_label_1 <- df_label_1 %>%
  mutate(
    generic_label_en = str_extract(variable, '".*?"') %>% str_replace_all('"', ''), # Extract text within quotes and remove quotes
    variable = str_extract(variable, '^[^ ]+') %>% toupper() # Extract first word and convert to uppercase
  )

df_label_1 <- df_label_1 %>%
  mutate(cycle = "Cycle 1")


stata_labels <- read_csv(here("Data", "stata_labels_cy1.csv"), col_names = F)

df_resp <- stata_labels %>%
  rename(line = X1) %>%
  mutate(
    variable = str_extract(line, "label define \\S+"),  # Extract "label define VAR"
    variable = str_remove(variable, "label define "),  # Remove "label define" to keep only the variable name
    response_num = str_extract(line, " \\d+"),  # Extract the response number
    response_num = str_trim(response_num),  # Remove any leading/trailing spaces
    response_text = str_extract(line, '".*?"') %>% str_replace_all('"', ""),  # Extract response text
    response_combined = paste(response_num, response_text)  # Format as "number response"
  ) %>%
  select(variable, response_combined) %>%
  filter(!is.na(variable))

# Combine all responses per variable into a single row
df_resp_combined <- df_resp %>%
  group_by(variable) %>%
  summarise(
    responses_en = paste(response_combined, collapse = ", "),  # Join responses
    .groups = "drop"
  )

df_label_1 <- df_label_1 %>%
  left_join(df_resp_combined, by = "variable")

df_labels <- rbind(df_label_1, df_label_2) %>%
  subset(!(variable == "EDLEVEL3" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVLIT1" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVLIT2" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVLIT3" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVLIT4" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVLIT5" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVLIT6" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVLIT7" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVLIT8" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVLIT9" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVLIT10" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVNUM1" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVNUM2" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVNUM3" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVNUM4" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVNUM5" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVNUM6" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVNUM7" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVNUM8" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVNUM9" & cycle == "Cycle 1")) %>%
  subset(!(variable == "PVNUM10" & cycle == "Cycle 1")) %>%
  mutate(cycle = ifelse(variable == "EDLEVEL3", "both", cycle),
         cycle = ifelse(variable == "PVLIT1", "both", cycle),
         cycle = ifelse(variable == "PVLIT2", "both", cycle),
         cycle = ifelse(variable == "PVLIT3", "both", cycle),
         cycle = ifelse(variable == "PVLIT4", "both", cycle),
         cycle = ifelse(variable == "PVLIT5", "both", cycle),
         cycle = ifelse(variable == "PVLIT6", "both", cycle),
         cycle = ifelse(variable == "PVLIT7", "both", cycle),
         cycle = ifelse(variable == "PVLIT8", "both", cycle),
         cycle = ifelse(variable == "PVLIT9", "both", cycle),
         cycle = ifelse(variable == "PVLIT10", "both", cycle),
         cycle = ifelse(variable == "PVNUM1", "both", cycle),
         cycle = ifelse(variable == "PVNUM2", "both", cycle),
         cycle = ifelse(variable == "PVNUM3", "both", cycle),
         cycle = ifelse(variable == "PVNUM4", "both", cycle),
         cycle = ifelse(variable == "PVNUM5", "both", cycle),
         cycle = ifelse(variable == "PVNUM6", "both", cycle),
         cycle = ifelse(variable == "PVNUM7", "both", cycle),
         cycle = ifelse(variable == "PVNUM8", "both", cycle),
         cycle = ifelse(variable == "PVNUM9", "both", cycle),
         cycle = ifelse(variable == "PVNUM10", "both", cycle)
         )

dups <- which(duplicated(df_labels$variable))  # Get row numbers of duplicates
dups_info <- data.frame(
  row_number = dups,  
  variable = df_labels$variable[dups]  #  variable names
)

# drop duplicates
df_labels <- df_labels[-dups, ]

# update cycle
df_labels <- df_labels %>%
  mutate(cycle = ifelse(variable %in% dups_info$variable, "both", cycle))



# make a variable2 because of the all caps problem
df_labels <- df_labels %>%
  mutate(variable2 = toupper(variable)) %>%
  select(-variable)

rm(df_resp, df_resp_combined, stata_labels, end_indices, end_vars, keep_indices, start_indices, start_vars)
rm(df_label_1, df_label_2, dups, dups_info)
