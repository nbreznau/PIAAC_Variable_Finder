df_trend <- read_csv(here("Data", "trend_vars.csv"))

# create cy1 df for trends
df_cy1_trend <- df_trend %>%
  subset(!is.na(var_c2) & !is.na(var_c1)) %>%
  mutate(variable = var_c1,
         trend_var = var_c2,
         cycle_m = "Cycle 1") %>%
  select(-c(var_c1, var_c2))

# create cy1 df for cancelled items
df_cy1_cancelled <- df_trend %>%
  subset(!is.na(var_c1) & is.na(var_c2)) %>%
  mutate(variable = var_c1,
         cycle_m = "Cycle 1",
         trend_var = NA) %>%
  select(-c(var_c1, var_c2))

# create cy2 df for new items
df_cy2_new <- df_trend %>%
  subset(!is.na(var_c2) & is.na(var_c1)) %>%
  subset(var_c2 != "F2_Q05a" & var_c2 != "G2_Q05a") %>%
  mutate(variable = var_c2,
         cycle_m = "Cycle 2",
         trend_var = NA) %>%
  select(-c(var_c1, var_c2))

# create cy2 df for trends
df_cy2_trend <- df_trend %>%
  subset(!is.na(var_c2) & !is.na(var_c1)) %>%
  mutate(variable = var_c2,
         trend_var = var_c1,
         cycle_m = "Cycle 2") %>%
  select(-c(var_c1, var_c2))

# fix some duplicates
df_cy2_trend <- df_cy2_trend %>%
  subset(!(variable == "B2_Q21" & trend_var == "B_Q19")) %>%
  subset(!(variable == "B2_Q21" & trend_var == "B_Q18")) %>%
  subset(!(variable == "B2_Q21" & trend_var == "B_Q20a")) %>%
  subset(!(variable == "F2_Q05a" & trend_var == "G_Q05h")) %>%
  subset(!(variable == "G2_Q05a" & trend_var == "H_Q05h")) %>%
  mutate(trend_var = ifelse(variable == "B2_Q21", "B_Q17, B_Q18, B_Q19, B_Q20a", trend_var),
         trend_var = ifelse(variable == "F2_Q05a", "G_Q05a, G_Q05h", trend_var),
         trend_var = ifelse(variable == "G2_Q05a", "H_Q05a, H_Q05h", trend_var),
         generic_label_en = ifelse(variable == "F2_Q05a", "Use ICT to communicate with others", generic_label_en),
         generic_label_de = ifelse(variable == "F2_Q05a", "Nutzen Sie IKT, um mit anderen zu kommunizieren", generic_label_de),
         generic_label_en = ifelse(variable == "G2_Q05a", "How often use smartphone", generic_label_en),
         generic_label_de = ifelse(variable == "G2_Q05a", "Wie oft Smartphone nutzen?", generic_label_de))
         
         


# put all back together
df_cy_trend <- rbind(df_cy1_trend, df_cy2_trend, df_cy1_cancelled, df_cy2_new)
df_cy_trend[df_cy_trend == ""] <- NA

rm(df_cy1_cancelled, df_cy1_trend, df_cy2_new, df_cy2_trend, df_trend)
