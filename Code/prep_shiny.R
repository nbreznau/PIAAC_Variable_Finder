library(rvest)
library(dplyr)
library(stringr)
library(here)
library(tidyr)
library(readr)

# Run subroutines parsing questionnaires
# Original data from https://www.oecd.org/en/about/programmes/piaac/piaac-data.html
# Note, UK used for English version

source(here("Shiny", "sub", "parse_questionnaires_cycle1.R"))
source(here("Shiny", "sub", "parse_questionnaires_cycle2.R"))

# Labels taken from R (Cy2) and Stata (Cy1) code
source(here("Shiny", "sub", "parse_labels.R"))

# taken from "Codebook-for-derived-Variables-16March2015"
df_derived <- read_csv(here("Data", "derived_vars.csv"), locale = locale(encoding = "latin1")) %>%
  mutate(cycle = "both",
         generic_label_en = label_en,
         generic_label_de = label_de,
         constructed_vars = NA) %>%
  select(-c(label_en, label_de)) %>%
  subset(!is.na(variable))

# 2nd cycle derived variables added at the end of routine
# https://www.oecd.org/content/dam/oecd/en/about/programmes/edu/piaac/data-materials/cycle-2/piaac-cy2-codebook-and-sql-codes-for-derived-variables.pdf

# trend variables manually extracted from the document "PIAAC_CY2(2018_11)BQ_Draft_Conceptual_Framework"
source(here("Shiny", "sub", "trend_var.R"))

df_cy_trend <- df_cy_trend %>%
  mutate(generic_label_en_extra = generic_label_en,
         generic_label_de_extra = generic_label_de,
         variable2 = toupper(variable),
         variable3 = variable) %>%
  select(-c(generic_label_de, generic_label_en, variable))


df <- rbind(df_piaac_c1, df_piaac_c2) %>%
  mutate(ref_variables = NA,
         generic_label_en = NA,
         generic_label_de = NA)

df <- df %>%
  mutate(generic_label_en_extra = generic_label_en,
         cycle_extra = cycle,
         responses_en_extra = responses_en,
         variable2 = toupper(variable)) %>% # necessary because cy1 has only all caps
  select(-c(generic_label_en, cycle, responses_en))

df2 <- df %>%
  full_join(df_labels, by = "variable2") %>%
  as.data.frame

df3 <- df2 %>%
  mutate(cycle = ifelse(is.na(cycle_extra), cycle, cycle_extra),
         responses_en = ifelse(responses_en == "NULL", NA, responses_en),
         responses_en = ifelse(!is.na(responses_en), responses_en, responses_en_extra),
         generic_label_en = ifelse(is.na(generic_label_en), generic_label_en_extra, generic_label_en),
         variable = ifelse(is.na(variable), variable2, variable)) %>%
  select(-c(generic_label_en_extra, cycle_extra, responses_en_extra))


df4 <- df3 %>%
  full_join(df_cy_trend, by = "variable2") %>%
  mutate(cycle = ifelse(!is.na(cycle), cycle, cycle_m),
         generic_label_en = ifelse(is.na(generic_label_en), generic_label_en_extra, generic_label_en),
         generic_label_de = ifelse(is.na(generic_label_de), generic_label_de_extra, generic_label_de)) %>%
  select(-c(cycle_m, generic_label_en_extra, generic_label_de_extra))

# remove NA for variable name and edit lower to upper case manually
#fix <- subset(df4, is.na(variable))

#write_csv(fix, here("Data", "fix.csv"))

fix_a <- read_csv(here("Data", "fix_a.csv"))

# remove problematics
df5 <- subset(df4, !is.na(variable))

df6 <- df5 %>%
  full_join(fix_a, by = "variable")

# fix variable names
df7 <- df6 %>%
  mutate(generic_label_en = ifelse(is.na(generic_label_en), generic_label_en_a, generic_label_en),
         generic_label_de = ifelse(is.na(generic_label_de), generic_label_de_a, generic_label_de),
         cycle = ifelse(is.na(cycle), cycle_a, cycle),
         soft_trend_explanation = ifelse(is.na(soft_trend_explanation), soft_trend_explanation_a, soft_trend_explanation),
         trend_var = ifelse(is.na(trend_var), trend_var_a, trend_var),
         trend = ifelse(is.na(trend), trend_a, trend),
         variable = ifelse(variable == variable2 & !is.na(variable3), variable3, variable)) %>%
  select(-c(generic_label_en_a, generic_label_de_a, cycle_a, trend_var_a, trend_a, soft_trend_explanation_a, variable2, variable3))


# remove variables with no information
df7 <- df7 %>%
  subset(!(is.na(generic_label_en) & is.na(generic_label_de) & 
           is.na(question_text_en) & is.na(question_text_de)))

df_derived <- df_derived %>%
  mutate(question_text_en = "[derived var]",
         question_text_de = NA)


# Convert all columns in df7 and df_derived to character
df7 <- df7 %>% mutate(across(everything(), as.character))
df_derived <- df_derived %>% mutate(across(everything(), as.character))

# Merge the two dataframes, keeping all cases
df8 <- full_join(df7, df_derived, by = "variable", suffix = c("_df7", "_df_derived"))

# Replace NA values with available data from either dataframe
df9 <- df8 %>%
  mutate(across(ends_with("_df7"), 
                ~ coalesce(as.character(.x), as.character(get(str_replace(cur_column(), "_df7", "_df_derived")))))) %>%
  rename_with(~ str_replace(.x, "_df7", ""), ends_with("_df7")) %>%
  select(-ends_with("_df_derived")) 


df9 <- df9 %>%
  subset(!(variable == "NUMWORK_WLE_CA" & is.na(responses_de)))

df9[df9 == ""] <- NA

# set up related variables from matches in the variable construction coding

# Store all variable names in a character vector
variable_list <- unique(df9$variable)

# Process df9 to find all matches in constructed_vars
df10 <- df9 %>%
  rowwise() %>%
  mutate(
    # Find all words from variable_list that appear at least once in constructed_vars
    matched_words = list(variable_list[sapply(variable_list, function(word) str_detect(constructed_vars, fixed(word)))]),
    
    # Handle cases where matched_words is empty
    c_vars = ifelse(length(matched_words) == 0 || all(matched_words == ""), 
                    NA, 
                    paste(unique(unlist(matched_words)), collapse = ", "))
  ) %>%
  select(-matched_words) %>%  # Remove temporary column
  ungroup()

# combine ref_vars and c_vars
df11 <- df10 %>%
  mutate(ref_variables = ifelse(is.na(ref_variables), "", ref_variables),
         c_vars = ifelse(is.na(c_vars), "", c_vars),
         ref_variables = paste(ref_variables, c_vars))

#df11 <- read_rds(here("Data", "df.RDS"))

# debugging from app testing
df11 <- df11 %>%
  mutate(trend_var = ifelse(variable == "EDCAT6", "EDCAT6_TC1",
                     ifelse(variable == "EDCAT7", "EDCAT7_TC1",
                     ifelse(variable == "EDCAT8", "EDCAT8_TC1",
                     ifelse(variable == "EDCAT6_TC1", "EDCAT6",
                     ifelse(variable == "EDCAT7_TC1", "EDCAT7",
                     ifelse(variable == "EDCAT8_TC1", "EDCAT8",
                     ifelse(variable == "LEAVER1624", "LEAVER1624C2",
                     ifelse(variable == "LEAVER1624C2", "LEAVER1624",
                     ifelse(variable == "EDWORK", "EDWORKC2",
                     ifelse(variable == "EDWORKC2", "EDWORK",
                     ifelse(variable == "TASKDISC", "TASKDISCC2_T1",
                     ifelse(variable == "TASKDISCC2_T1", "TASKDISC",
                     ifelse(variable == "FE12", "FE12C2",
                     ifelse(variable == "FE12C2", "FE12",
                     ifelse(variable == "ICTHOME", "ICTHOMEC2",
                     ifelse(variable == "ICTHOMEC2", "ICTHOME",
                     ifelse(variable == "ICTWORK", "ICTWORKC2",
                     ifelse(variable == "ICTWORKC2", "ICTWORK",
                     ifelse(variable == "IMGEN", "IMGENC2",
                     ifelse(variable == "IMGENC2", "IMGEN",
                     ifelse(variable == "IMPAR", "IMPARC2",
                     ifelse(variable == "IMPARC2", "IMPAR", trend_var)))))))))))))))))))))),
         trend_var = ifelse(variable == "LEARNATWORK", "LEARNATWORKC2",
                     ifelse(variable == "LEARNATWORKC2", "LEARNATWORK",
                     ifelse(variable == "NEET", "NEETC2",
                     ifelse(variable == "NEETC2", "NEET",
                     ifelse(variable == "NFE12", "NFE12C2",
                     ifelse(variable == "NFE12C2", "NFE12",
                     ifelse(variable == "NFE12JR", "NFE12JRC2",
                     ifelse(variable == "NFE12JRC2", "NFE12JR",
                     ifelse(variable == "NFE12NJR", "NFE12NJRC2",
                     ifelse(variable == "NFE12NJRC2", "NFE12NJR",
                     ifelse(variable == "NUMHOME", "NUMHOMEC2",
                     ifelse(variable == "NUMHOMEC2", "NUMHOME",
                     ifelse(variable == "NUMWORK", "NUMWORKC2",
                     ifelse(variable == "NUMWORKC2", "NUMWORK",
                     ifelse(variable == "PARED", "PAREDC2",
                     ifelse(variable == "PAREDC2", "PARED",
                     ifelse(variable == "WRITHOME", "WRITHOMEC2",
                     ifelse(variable == "WRITHOMEC2", "WRITHOME",
                     ifelse(variable == "WRITWORK", "WRITWORKC2",
                     ifelse(variable == "WRITWORKC2", "WRITWORK",
                     ifelse(variable == "YRSGET", "YRSGETC2",
                     ifelse(variable == "YRSGETC2", "YRSGET",
                     ifelse(variable == "YRSQUAL", "YRSQUALC2", trend_var))))))))))))))))))))))),
         trend_var = ifelse(variable == "YRSQUALC2", "YRSQUAL",
                     ifelse(variable == "READWORK", "READWORKC2_T1",
                     ifelse(variable == "READWORKC2_T1", "READWORK",
                     ifelse(variable == "READHOME", "READHOMEC2_T1",
                     ifelse(variable == "READHOMEC2_T1", "READHOME",
                     ifelse(variable == "AETPOP", "AETPOPC2",
                     ifelse(variable == "AETPOPC2", "AETPOP",
                      ifelse(variable == "E_Q08", "E2_Q08",
                      ifelse(variable == "E2_Q08", "E_Q08",
                     ifelse(variable == "EARNFLAG", "EARNFLAGC2",
                     ifelse(variable == "EARNFLAGC2", "EARNFLAG", trend_var)
                     )))))))))),
         trend = ifelse(variable %in% c("EDCAT6", "EDCAT6_TC1", "EDCAT7", "EDCAT7_TC1",
                        "EDCAT8", "EDCAT8_TC1", "LEAVER1624", "LEAVER1624C2",
                        "EDWORK", "EDWORKC2", "TASKDISC", "TASKDISCC2_T1",
                        "FE12", "FE12C2", "ICTHOME", "ICTHOMEC2",
                        "ICTWORK", "ICTWORKC2", "IMGEN", "IMGENC2",
                        "IMPAR", "IMPARC2", "LEARNATWORK", "LEARNATWORKC2",
                        "NEET", "NEETC2", "NFE12", "NFE12C2",
                        "NFE12JR", "NFE12JRC2", "NFE12NJR", "NFE12NJRC2",
                        "NUMHOME", "NUMHOMEC2", "NUMWORK", "NUMWORKC2",
                        "PARED", "PAREDC2", "WRITHOME", "WRITHOMEC2",
                        "WRITWORK", "WRITWORKC2", "YRSGET", "YRSGETC2",
                        "YRSQUAL", "YRSQUALC2", "READWORK", "READWORKC2_T1",
                        "READHOME", "READHOMEC2_T1", "AETPOPC2", "AETPOP",
                        "EARNFLAG", "EARNFLAGC2", "E2_Q08", "E_Q08"), "Derived", trend),
         cycle = ifelse(variable == "GENDER_R", "both", cycle),
         trend = ifelse(is.na(trend) & cycle == "both", "Strict", trend),
         generic_label_en = ifelse(variable == "I2_Q05", 
                                   "About yourself - Satisfaction with life (life satisfaction)", generic_label_en),
         question_text_en = ifelse(variable == "GENDER_R", "[derived]", question_text_en),
         responses_en = ifelse(variable == "GENDER_R", "01: Male | 02: Female", responses_en),
         ref_variables = ifelse(variable == "GENDER_R", "A_N01, A2_N02, A_N01_T", ref_variables),
         variable = ifelse(variable == "B2_Q05B_1CT", "B2_Q05b_1CT",
                    ifelse(variable == "E2_Q05A1", "E2_Q05a1",
                    ifelse(variable == "E2_Q05B1", "E2_Q05b1", variable))),
         question_text_en = ifelse(variable == "B2_Q20", "There can be different costs associated with participating in a training activity, for example tuition fees, expenses for books, or travel costs. Who paid for this training activity? Please name all that apply.",
                                   question_text_en),
         responses_en = ifelse(variable == "B2_Q20", "01	You personally | 02	Your employer | 03	Federal, state, or local employment agency (e.g. one-stop center, American Job Center, or career resource center) | 04	Trade unions or associations | 05	Other public sources | 06	Other private sources (e.g. family members) | 07	Others | 08	There were no such costs",
                               responses_en)
  ) %>%
  subset(!(variable %in% c("CI_GENDER", "H2_Q03", "H2_Q04", "H2_Q05", "H2_Q07", 
                           "F2_Q05", "F2_I05", "EP_VET", "EP_ISC4", "EP_ACAD")))

df_derived_2 <- read_csv(here("Data", "derived_2_vars.csv"))

df12 <- rbind(df11, df_derived_2)


# more updates
df12 <- df12 %>%
  mutate(c_vars = ifelse(variable == "READYTOLEARN", 
                         "I_Q04b, I_Q04d, I_Q04h, I_Q04j, I_Q04l, I_Q04m",
                         c_vars),
         question_text_en = ifelse(variable == "I_Q04b", "When I hear or read about new ideas, I try to relate them to real life situations to 
which they might apply",
                            ifelse(variable == "I_Q04d", "I like learning new things",
                            ifelse(variable == "I_Q04h", "When I come across something new, I try to relate it to what I already know",
                            ifelse(variable == "I_Q04j", "I like to get to the bottom of difficult things",
                            ifelse(variable == "I_Q04l", "I like to figure out how different ideas fit together",
                            ifelse(variable == "I_Q04m", "If I don't understand something, I look for additional information to make it 
clearer", question_text_en)))))))

#write_rds(df12, here("Data", "df.RDS"))

#df12 <- read_rds(here("Data", "df.RDS"))

df13 <- df12 %>%
  mutate(question_text_en = ifelse(variable == "B2_Q04a", 
                                   df12$question_text_en[df12$variable == "B2_Q04aUK1"],
                                   question_text_en),
         variable = ifelse(variable == "I_Q06A", "I_Q06a",
                     ifelse(variable == "I_Q07A", "I_Q07a", variable)),
         question_text_en = ifelse(variable == "I_Q06a",
                                   "To what extent do you agree or disagree with the following statements? People like me don't have any say about what the government does",
                            ifelse(variable == "I_Q07a", 
                                    "To what extent do you agree or disagree with the following statements? There are only a few people you can trust completely",
                            ifelse(variable == "I_Q07b",
                                   "To what extent do you agree or disagree with the following statements? If you are not careful, other people will take advantage of you",
                                   question_text_en))),
         question_text_de = ifelse(variable == "I_Q06a",
                                   "Sagen Sie mir bitte, inwieweit Sie den folgenden Aussagen zustimmen oder nicht zustimmen. Menschen wie ich haben keinerlei Einfluss darauf, was die Regierung macht.",
                            ifelse(variable == "I_Q07a", 
                                   "Sagen Sie mir bitte, inwieweit Sie den folgenden Aussagen zustimmen oder nicht zustimmen. Es gibt nur wenige Menschen, denen man voll vertrauen kann.",
                            ifelse(variable == "I_Q07b",
                                   "Sagen Sie mir bitte, inwieweit Sie den folgenden Aussagen zustimmen oder nicht zustimmen. Wenn man nicht aufpasst, wird man von anderen ausgenutzt.",
                                   question_text_de))),
         generic_label_de = ifelse(variable == "I_Q06a", "Politische Wirksamkeit, Politik, Politics", 
                            ifelse(variable == "I2_Q01a", "Politische Wirksamkeit, Politik, Politics",
                                   generic_label_de)),
         trend = ifelse(variable == "I_Q06a", "Cy1 only", trend),
         ref_variables = ifelse(variable == "I_Q06a", "I2_Q01a", 
                       ifelse(variable == "I2_Q01a", "I_Q06a", ref_variables)),
         responses_de = ifelse(variable %in% c("I_Q06a", "I_Q07a", "I_Q07b"), "01: Stimme voll und ganz zu | 02: Stimme zu | 03: Stimme teils zu, stimme teils nicht zu | 04: Stimme nicht zu | 05: Stimme überhaupt nicht zu",
                               responses_de),
         ref_variables = ifelse(is.na(ref_variables) | ref_variables == "", c_vars, ref_variables)
         ) %>% subset(!variable %in% c("I_Q06a_lead", "I_Q06b"))

#write_rds(df13, here("Data", "df.RDS"))

#df13 <- read_rds(here("Data", "df.RDS"))

df14 <- df13 %>%
  mutate(generic_label_de = ifelse(str_detect(variable, "^K2_"), "Big Five, Personality, Persönlichkeit", 
                                    generic_label_de),
         generic_label_de = ifelse(variable %in% c("OPEM", "OPEM_6", "AGRE", "AGRE_6", "EMOS", "EMOS_6", "EXTR",
                                                    "EXTR_6", "CONS", "CONS_6"), 
                                    "Big Five, Personality, Persönlichkeit",
                                    generic_label_de),
         responses_en = ifelse(variable %in% c("OPEM", "OPEM_6", "AGRE", "AGRE_6", "EMOS", "EMOS_6", "EXTR",
                                                "EXTR_6", "CONS", "CONS_6"),
                                "standardized z-score", responses_en),
         trend = ifelse(variable %in% c("OPEM", "OPEM_6", "AGRE", "AGRE_6", "EMOS", "EMOS_6", "EXTR",
                                        "EXTR_6", "CONS", "CONS_6"), "Cy2 only", trend),
         ref_variables = ifelse(variable %in% c("OPEM", "OPEM_6", "AGRE", "AGRE_6", "EMOS", "EMOS_6", "EXTR",
                                                "EXTR_6", "CONS", "CONS_6"), "Variables with K2_", ref_variables))

write_rds(df14, here("Data", "df14.RDS"))

write_rds(df14, here("Data", "df.RDS"))

#df <- read_rds(here("Data", "df14.RDS"))

# find out which variables have non-missing data for at least one of DE or UK
# for the PUF

df <- readRDS(here("Data", "df.RDS"))
df_piaac <- read_csv(here("Data", "piaac_combined.csv"), show_col_types = FALSE) %>%
  filter(CNTRYID == 276 | CNTRYID == 826)
# playing around with filter v subset (doesn't seem to matter)
df_piaac2 <- subset(read_csv(here("Data", "piaac_combined_2.csv"), show_col_types = F), CNTRYID == 276 | CNTRYID == 826)

vars <- df$variable

# find variables with non-missings for at least one of the cycle-country cases
df$notin <- vapply(vars, function(v) {
  if (v %in% names(df_piaac)) {
    all(is.na(df_piaac[[v]]))  # 1 if column exists and all NA
  } else {
    TRUE                       # 1 if column absent
  }
}, logical(1L)) * 1L           # convert TRUE/FALSE → 1/0

df$notin2 <- vapply(vars, function(v) {
  if (v %in% names(df_piaac2)) {
    all(is.na(df_piaac2[[v]]))  # 1 if column exists and all NA
  } else {
    TRUE                       # 1 if column absent
  }
}, logical(1L)) * 1L           # convert TRUE/FALSE → 1/0

df$none = ifelse(df$notin + df$notin2 == 2, "NO", "YES")

write_rds(df, here("Data", "df15.RDS"))

write_rds(df, here("Data", "df.RDS"))

# fix German language symbol addition

df <- df %>%
  mutate(responses_de = str_remove_all(responses_de, "[\u0084\u0093]"),
         generic_label_en = ifelse(variable == "AETPOP", "Adult education/training population – excludes youths 16-24", generic_label_en),
         generic_label_en = ifelse(variable == "AETPOPC2", "Adult education/training population – excludes youths 16-24", generic_label_en),
         generic_label_en = str_replace_all(generic_label_en, regex("\\bCBA\\b"), "CBA (Computer-based assessment)"),
         ref_variables = ifelse(variable == "D2_Q08B_C", "D_Q07B_C", 
                         ifelse(variable == "D_Q07B_C", "D2_Q08B_C", ref_variables)),
         generic_label_en = ifelse(variable == "LNG_L1", "First language learned at home in childhood and still understood",
                            ifelse(variable == "LNG_L2", "Second language learned at home in childhood and still understood",
                                   generic_label_en)),
         responses_en = ifelse(variable == "LNG_L1" | variable == "LNG_L2", "Listed as ISO3c country codes", responses_en),
         ref_variables = ifelse(variable == "LNG_L1", "LNG_L2, LNG_HOME, LNG_BQ",
                         ifelse(variable == "LNG_L2", "LNG_L1, LNG_HOME, LNG_BQ", ref_variables)),
         variable = ifelse(variable == "NativeSpeaker", "NATIVESPEAKER", variable),
         ref_variables = ifelse(variable == "NATIVESPEAKER", "J_Q05a1DE, J_Q05a1, LNG_L1, LNG_L2, NATIVELANG, BORNLANG, FORBORNLANG", ref_variables),
         ref_variables = ifelse(variable == "BORNLANG", "J_Q04a,  LNG_L1, LNG_L2, NATIVELANG, NATIVESPEAKER, FORBORNLANG", ref_variables),
         ref_variables = ifelse(variable == "FORBORNLANG", "LNG_L1, LNG_L2, BORNLANG, NATIVELANG, NATIVESPEAKER, FORBORNLANG", ref_variables),
         ref_variables = ifelse(variable == "NATIVELANG", "LNG_CI, LNG_L1, LNG_L2, BORNLANG, NATIVELANG, NATIVESPEAKER, FORBORNLANG", ref_variables),
         generic_label_en = ifelse(variable == "H2_Q08", "Work autonomy", generic_label_en),
         question_text_en = ifelse(variable == "H2_Q09a", "How often does your current job involve learning new things? (or last if no current)", question_text_en),
         variable = ifelse(variable == "H2_Q19B01", "H2_Q19b01",
                    ifelse(variable == "H2_Q19B02", "H2_Q19b02",
                    ifelse(variable == "H2_Q19B03", "H2_Q19b03",
                    ifelse(variable == "H2_Q19B04", "H2_Q19b04",
                    ifelse(variable == "H2_Q19B05", "H2_Q19b05",
                    ifelse(variable == "H2_Q19B06", "H2_Q19b06",
                    ifelse(variable == "H2_Q19B07", "H2_Q19b07", variable)))))))
                    
  )

write_rds(df, here("Data", "df16.RDS"))

write_rds(df, here("Data", "df.RDS"))

# Redundancy between "Cy2 only" and "New in Cy2"

#df <- read_rds(here("Data", "df16.RDS"))

df <- df %>%
  mutate(trend = ifelse(trend == "New in Cy2", "Cy2 only", trend),
         responses_en = responses_en %>%
                    str_replace_all("c\\(", "") %>%
                    str_replace_all("numeric\\(0\\)", "Numeric") %>%
                    str_replace_all("=\\s?1\\)", "=1") %>%
                    str_replace_all("=\\s?2\\)", "=2") %>%
                    str_replace_all("=\\s?3\\)", "=3") %>%
                    str_replace_all("=\\s?4\\)", "=4") %>%
                    str_replace_all("=\\s?5\\)", "=5") %>%
                    str_replace_all("=\\s?5\\)", "=6") %>%
                    str_replace_all("=\\s?5\\)", "=7") %>%
                    str_replace_all("=\\s?10\\)", "=10"),
         responses_de = ifelse(responses_de == "#VALUE!", NA, responses_de)
           )

df <- df %>%
  mutate(responses_en = ifelse(responses_en == "numeri0)", "Numeric", responses_en))


