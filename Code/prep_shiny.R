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

#write_rds(df, here("Data", "df.RDS"))

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
  mutate(responses_en = ifelse(responses_en == "numeri0)", "Numeric", responses_en),
         question_text_en = ifelse(variable %in% c("F2_Q01a", "F2_Q01b", "F2_Q01c", "F2_Q01d", "F2_Q01e", "F2_Q01f", "F2_Q01A_T", "F2_Q01A_T1", "F2_Q01B_T", "F2_Q01B_T1", "F2_Q01C_T", "F2_Q01C_T1", "F2_Q01E_T", "F2_Q01E_T1"),
                                   "IF (^C2_D10 = 1) THEN =^F2_D01a = The following questions are about reading activities that you undertake as part of your current job. Please only report reading that is part of your current job, not reading you do in your non-work time. Include all forms of reading, including any reading you might do on computer screens, tablets or other electronic displays. ELSE =^F2_D01a = The following questions are about reading activities that you undertook as part of your last job. Please only report 
                                   reading that was part of your last job, not reading you did in your non-work time. Include all forms of reading, including any reading you did on computer screens, tablets or other electronic displays.", 
                                   question_text_en),
         question_text_de = ifelse(variable %in% c("F2_Q01a", "F2_Q01b", "F2_Q01c", "F2_Q01d", "F2_Q01e", "F2_Q01f", "F2_Q01A_T", "F2_Q01A_T1", "F2_Q01B_T", "F2_Q01B_T1", "F2_Q01C_T", "F2_Q01C_T1", "F2_Q01E_T", "F2_Q01E_T1"),
                                   "IF (^C2_D10 = 1) 
                                   THEN =^F2_D01a = In den folgenden Fragen geht es darum, inwieweit Sie im Rahmen Ihrer derzeitigen Arbeit lesen. Bitte berücksichtigen Sie dabei auch das Lesen an Computerbildschirmen, Tablets oder anderen elektronischen Geräten. Bitte denken Sie jedoch nicht an das Lesen in Ihrer Freizeit.
                                   ELSE =^F2_D01a = In den folgenden Fragen geht es darum, inwieweit Sie im Rahmen Ihrer letzten Arbeit gelesen haben. Bitte berücksichtigen Sie dabei auch das Lesen an Computerbildschirmen, Tablets oder anderen elektronischen Geräten. Bitte denken Sie jedoch nicht an das Lesen in Ihrer Freizeit.", 
                                   question_text_de))

# Fix time skill use at work questions, specific to each variable

df <- df %>%
  mutate(question_text_de = if_else(variable == "F2_Q01a", paste("wie oft haben Sie normalerweise: Anleitungen oder Anweisungen gelesen? ...", question_text_de, sep = ": "), question_text_de),
         question_text_en = if_else(variable == "F2_Q01a", paste("How often do you usually: read directions or instructions? ...", question_text_en, sep = ": "), question_text_en),
         question_text_de = if_else(variable == "F2_Q01b", paste("wie oft haben Sie normalerweise: Briefe, Notizen oder E-Mails gelesen? ...", question_text_de, sep = ": "), question_text_de),
         question_text_en = if_else(variable == "F2_Q01b", paste("How often do you usually: read letters, memos or e-mails? ...", question_text_en, sep = ": "), question_text_en),
         question_text_de = if_else(variable == "F2_Q01c", paste("wie oft haben Sie normalerweise: Artikel in Zeitungen, Zeitschriften oder Newsletter gelesen? ...", question_text_de, sep = ": "), question_text_de),
         question_text_en = if_else(variable == "F2_Q01c", paste("How often do you usually: read articles in newspapers, magazines or newsletters? ...", question_text_en, sep = ": "), question_text_en),
         question_text_de = if_else(variable == "F2_Q01d", paste("wie oft haben Sie normalerweise: Bücher, wissenschaftliche Veröffentlichungen oder Artikel in Fachzeitschriften gelesen? ...", question_text_de, sep = ": "), question_text_de),
         question_text_en = if_else(variable == "F2_Q01d", paste("How often do you usually: read books, scholarly publications, or articles in professional journals? ...", question_text_en, sep = ": "), question_text_en),
         question_text_de = if_else(variable == "F2_Q01e", paste("wie oft haben Sie normalerweise: Handbücher oder Nachschlagewerke gelesen? ...", question_text_de, sep = ": "), question_text_de),
         question_text_en = if_else(variable == "F2_Q01e", paste("How often do you usually: read manuals or reference materials? ...", question_text_en, sep = ": "), question_text_en),
         question_text_de = if_else(variable == "F2_Q01f", paste("wie oft haben Sie normalerweise: Rechnungen, Bankauszüge oder Ähnliches gelesen? ...", question_text_de, sep = ": "), question_text_de),
         question_text_en = if_else(variable == "F2_Q01f", paste("How often do you usually: read bills, invoices, bank statements or other financial statements? ...", question_text_en, sep = ": "), question_text_en),
  )


F2_Q02prompt_en <- "[The following questions are about writing activities that you undertake as part of your current job (last job). Include any writing you might do (you did) on computers, tablets or other electronic devices.] In your current job (last job), how often do you usually" 
F2_Q02prompt_de <- "[In den folgenden Fragen geht es darum, inwieweit Sie im Rahmen Ihrer derzeitigen Arbeit (letzten Arbeit) schreiben (geschrieben haben). Bitte berücksichtigen Sie dabei auch das Schreiben am Computer, auf Tablets oder anderen elektronischen Geräten.] Bei Ihrer derzeitigen Arbeit (letzten Arbeit), wie oft haben Sie normalerweise"
F2_Q03prompt_en <- "[The following questions are about activities that you undertake as part of your current job (last job) and that involve numbers, quantities, numerical information, statistics or mathematics.] In your current job (last job), how often do you usually" 
F2_Q03prompt_de <- "[In den folgenden Fragen geht es um Tätigkeiten im Rahmen Ihrer derzeitigen Arbeit (letzten Arbeit), die mit Zahlen, Mengenangaben, Statistik oder Mathematik zu tun haben.] Bei Ihrer derzeitigen Arbeit (letzten Arbeit), wie oft haben Sie normalerweise"




df <- df %>%
  mutate(question_text_en = if_else(variable == "F2_Q02a", paste(F2_Q02prompt_en, "write letters, memos or e-mails?", sep = " "), question_text_en),
         question_text_de = if_else(variable == "F2_Q02a", paste(F2_Q02prompt_de, "Briefe, Notizen oder E-Mails geschrieben?", sep = " "), question_text_de),
         question_text_en = if_else(variable == "G_Q02a", paste(F2_Q02prompt_en, "write letters, memos or e-mails?", sep = " "), question_text_en),
         question_text_de = if_else(variable == "G_Q02a", paste(F2_Q02prompt_de, "Briefe, Notizen oder E-Mails geschrieben?", sep = " "), question_text_de),
         question_text_en = if_else(variable == "F2_Q02b", paste(F2_Q02prompt_en, "write reports or articles?", sep = " "), question_text_en),
         question_text_de = if_else(variable == "F2_Q02b", paste(F2_Q02prompt_de, "Berichte oder Artikel geschrieben?", sep = " "), question_text_de),
         c_vars = if_else(variable %in% c("G_Q02b", "G_Q02c"), "F2_Q02b", c_vars),
         question_text_en = if_else(variable == "G_Q02b", paste(F2_Q02prompt_en, "write articles?", sep = " "), question_text_en),
         question_text_de = if_else(variable == "G_Q02b", paste(F2_Q02prompt_de, "Artikel geschrieben?", sep = " "), question_text_de),
         question_text_en = if_else(variable == "G_Q02c", paste(F2_Q02prompt_en, "write reports?", sep = " "), question_text_en),
         question_text_de = if_else(variable == "G_Q02c", paste(F2_Q02prompt_de, "Berichte geschrieben?", sep = " "), question_text_de),
         question_text_en = if_else(variable == "F2_Q02c", paste(F2_Q02prompt_en, "Fill in forms?", sep = " "), question_text_en),
         question_text_de = if_else(variable == "F2_Q02c", paste(F2_Q02prompt_de, "Formulare ausgefüllt?", sep = " "), question_text_de),
         question_text_en = if_else(variable == "G_Q02d", paste(F2_Q02prompt_en, "Fill in forms?", sep = " "), question_text_en),
         question_text_de = if_else(variable == "G_Q02d", paste(F2_Q02prompt_de, "Formulare ausgefüllt?", sep = " "), question_text_de))

df <- df %>%
  mutate(question_text_en = if_else(variable == "F2_Q03a", paste(F2_Q03prompt_en, "undertake calculations, such as calculating prices, costs or quantities?", sep = " "), question_text_en),
         question_text_de = if_else(variable == "F2_Q03a", paste(F2_Q03prompt_de, "Berechnungen durchgeführt, z.B. von Preisen, Kosten oder Mengen?", sep = " "), question_text_de),
         question_text_en = if_else(variable == "F2_Q03b", paste(F2_Q03prompt_en, "use maps, plans or GPS for finding directions and locations?", sep = " "), question_text_en),
         question_text_de = if_else(variable == "F2_Q03b", paste(F2_Q03prompt_de, "Karten, Pläne oder Navigationssysteme benutzt, um Wegbeschreibungen oder Orte zu finden?", sep = " "), question_text_de),
         question_text_en = if_else(variable == "F2_Q03c", paste(F2_Q03prompt_en, "undertake measurements such as lengths, weights, temperatures, dosages, areas or volumes?", sep = " "), question_text_en),
         question_text_de = if_else(variable == "F2_Q03c", paste(F2_Q03prompt_de, "Messungen durchgeführt, z.B. von Längen, Gewicht, Temperaturen, Dosierungen, Flächen oder Volumen?", sep = " "), question_text_de),
         question_text_en = if_else(variable == "F2_Q03d", paste(F2_Q03prompt_en, "read and prepare charts, graphs or tables?", sep = " "), question_text_en),
         question_text_de = if_else(variable == "F2_Q03d", paste(F2_Q03prompt_de, "Diagramme, Schaubilder oder Tabellen gelesen und erstellt", sep = " "), question_text_de),
         question_text_en = if_else(variable == "F2_Q03e", paste(F2_Q03prompt_en, "use advanced mathematics or statistics?", sep = " "), question_text_en),
         question_text_de = if_else(variable == "F2_Q03e", paste(F2_Q03prompt_de, "Höhere Mathematik oder Statistik verwendet", sep = " "), question_text_de),
         question_text_en = if_else(variable == "F2_Q04", "Do you use a computer in your current job (last job)? (By computer we mean a mainframe, desktop, laptop computer, tablets or any other device that can be used to do such things as sending or receiving e-mail messages, processing data or text, or finding things on the internet)", question_text_en),
         question_text_de = if_else(variable == "F2_Q04", "Benutzen Sie bei Ihrer derzeitigen Arbeit (letzten Arbeit) einen Computer? (Hier sind auch Smartphones, Tablets sowie andere tragbare elektronische Geräte gemeint, die zur Internetnutzung und zum Lesen von E-Mails usw. verwendet werden.)", question_text_de))

F2_Q05prompt_en <- "The following questions are about the use of a computer or digital device such as a tablet or smartphone as part of your current job (last job). They do not refer to the use of computers or digital devices in any jobs you may have held prior to your current job."
F2_Q05prompt_de <- "In den folgenden Fragen geht es um die Nutzung von Computern oder digitalen Geräten wie beispielsweise Tablets oder Smartphones bei Ihrer derzeitigen Arbeit (letzten Arbeit). Sie beziehen sich nicht auf die Nutzung von Computern oder digitalen Geräten bei früheren beruflichen Tätigkeiten."
F2_Q05resp_de <- "01	Nie	02	Seltener als einmal im Monat	03	Seltener als einmal pro Woche, aber mindestens einmal im Monat	04	Mindestens einmal pro Woche, aber nicht täglich	05	Täglich"

df <- df %>%
  mutate(question_text_en = if_else(variable == "F2_Q05a", paste(F2_Q05prompt_en, "To communicate with others (e.g. via emails, social networking sites, or internet calls). Exclude normal phone calls using a mobile phone.", sep = " "), question_text_en),
         question_text_de = if_else(variable == "F2_Q05a", paste(F2_Q05prompt_de, "Um mit anderen zu kommunizieren, z.B. über E-Mail, soziale Netzwerke oder Internettelefonate. Nicht gemeint sind normale Telefonate mit dem Handy oder Smartphone.", sep = " "), question_text_en),
         responses_de = if_else(variable == "F2_Q05a", F2_Q05resp_de, responses_de),
         question_text_en = if_else(variable == "F2_Q05c", paste(F2_Q05prompt_en, "To access information (e.g. use a search engine, find information, or read documents).", sep = " "), question_text_en),
         question_text_de = if_else(variable == "F2_Q05c", paste(F2_Q05prompt_de, "Um Informationen zu bekommen, z.B. eine Suchmaschine nutzen, Informationen finden oder Dokumente lesen.", sep = " "), question_text_en),
         responses_de = if_else(variable == "F2_Q05c", F2_Q05resp_de, responses_de),
         question_text_en = if_else(variable == "F2_Q05d", paste(F2_Q05prompt_en, "To create or edit electronic documents, spreadsheets or presentations (using Microsoft Word, Excel, PowerPoint, or similar software).", sep = " "), question_text_en),
         question_text_de = if_else(variable == "F2_Q05d", paste(F2_Q05prompt_de, "Um elektronische Dokumente, Tabellenkalkulationen oder Präsentationen zu erstellen oder zu bearbeiten, z.B. mit Word, Excel, PowerPoint oder ähnlicher Software.", sep = " "), question_text_en),
         responses_de = if_else(variable == "F2_Q05d", F2_Q05resp_de, responses_de),
         question_text_en = if_else(variable == "F2_Q05e", paste(F2_Q05prompt_en, "To use specialized software (e.g. for computer-aided design, the processing or analysis of data, sound and images, or quality control).", sep = " "), question_text_en),
         question_text_de = if_else(variable == "F2_Q05e", paste(F2_Q05prompt_de, "Um Spezialsoftware zu nutzen, z.B. für computergestütztes Design, für die Verarbeitung oder Analyse von Daten, für Ton- und Bildbearbeitung oder zur Qualitätskontrolle.", sep = " "), question_text_en),
         responses_de = if_else(variable == "F2_Q05e", F2_Q05resp_de, responses_de),
         question_text_en = if_else(variable == "F2_Q05f", paste(F2_Q05prompt_en, "To use a programming language to program software (e.g. applications) or websites.", sep = " "), question_text_en),
         question_text_de = if_else(variable == "F2_Q05f", paste(F2_Q05prompt_de, "Um eine Programmiersprache zu nutzen, um Software, wie z.B. Apps, oder Webseiten zu programmieren.", sep = " "), question_text_en),
         responses_de = if_else(variable == "F2_Q05f", F2_Q05resp_de, responses_de))

df <- df %>%
  mutate(constructed_vars = if_else(variable == "EDLEVEL3", "derived variable", constructed_vars),
         question_text_en = if_else(variable == "EDLEVEL3", "PIAAC does not offer documentation, but this variable is derived from ISCED so that educational/education attainment is Low = 1-2, Mid = 3-4, and High = 5+. Essentially primary, secondary and tertiary", question_text_en),
         trend_var = if_else(variable == "EDLEVEL3", "EDLEVEL3", trend_var),
         c_vars = if_else(variable == "EDLEVEL3", "EDCAT6, EDCAT6_TC1, EDCAT7, EDCAT7_TC1, EDCAT8, EDCAT8_TC1", c_vars))

df <- df %>%
  mutate(responses_de = if_else(variable == "C_D05", "1 Erwerbstätig, 2 Arbeitslos, 3 Nicht erwerbstätig, 4 Unbekannt", responses_de),
         responses_de = if_else(variable == "C2_D05", "1 Erwerbstätig, 2 Arbeitslos, 3 Nicht erwerbstätig, 4 Unbekannt", responses_de),
         question_text_en = if_else(variable == "J_Q03a", "[Question on parent/parenthood] Do you have children? Please include stepchildren and children not living in your household.", question_text_en),
         question_text_de = if_else(variable == "J_Q03a", "[Frage zu Elternschaft] Haben Sie Kinder? Hiermit sind auch Stiefkinder und eigene Kinder, die nicht in Ihrem Haushalt leben, gemeint.", question_text_de),
         question_text_en = if_else(variable == "J2_Q03a", "[Question on parent/parenthood] Do you have children? Please include stepchildren and children not living in your household.", question_text_en),
         question_text_de = if_else(variable == "J2_Q03a", "[Frage zu Elternschaft] Haben Sie Kinder? Hiermit sind auch Stiefkinder und eigene Kinder, die nicht in Ihrem Haushalt leben, gemeint.", question_text_de),
         question_text_en = if_else(variable == "C2_Q11", "In the last 12 months, that is since ^MonthYear, did you receive unemployment benefits, disability benefits, sickness benefits, housing benefits or state pension benefits [welfare state]?", question_text_en),
         question_text_de = if_else(variable == "C2_Q11", "Haben Sie in den letzten 12 Monaten, d.h. seit ^MonthYear, irgendwann einmal Arbeitslosengeld, Erwerbsminderungsrente, Krankengeld, Rente oder Pension erhalten [Sozialstaat / Wohlfahrtstaat]?", question_text_de),
         c_vars = if_else(variable %in% c("A_N01", "A_N01_T", "A2_N02", "A2_N02T"), "A_N01, A_N01_T, A2_N02, A2_N02T", c_vars),
         question_text_de = if_else(variable %in% c("A_N01", "A_N01_T", "A2_N02", "A2_N02T"), "Geschlecht: männlich oder weiblich", question_text_de))

#write_rds(df, here::here("Data", "df17.RDS"))

write_rds(df, here("Data", "df.RDS"))



write_rds(df, here("Shiny", "df.RDS"))
