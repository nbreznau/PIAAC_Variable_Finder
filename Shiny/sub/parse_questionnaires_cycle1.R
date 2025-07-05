
# Function to extract questionnaire data from an HTML file
parse_piaac_html <- function(file_path, language, cycle) {
  html_doc <- read_html(file_path)
  
  # Extract all question items
  items <- html_doc %>% html_nodes(".itemGroup")
  
  data_list <- list()
  
  for (item in items) {
    # Extract variable name
    var_name <- item %>% html_node(".questionCode") %>% html_text(trim = TRUE)
    
    # Extract question text
    question_text <- item %>% html_node(".questionText") %>% html_text(trim = TRUE)
    
    # Extract response choices (if available)
    response_nodes <- item %>% html_nodes(".responseTable tr")
    responses_list <- list()
    
    for (row in response_nodes) {
      code <- row %>% html_node(".label_answer") %>% html_text(trim = TRUE)
      text <- row %>% html_node("td:nth-child(2)") %>% html_text(trim = TRUE)
      
      if (!is.na(code) && code != "" && !is.na(text) && text != "") {
        responses_list[[code]] <- text
      }
    }
    
    responses_str <- ifelse(length(responses_list) > 0, 
                            paste(names(responses_list), responses_list, sep = ": ", collapse = " | "), NA)
    
    # Extract constructed variables
    constructed_vars <- item %>% html_nodes(".routingID, .routing") %>% html_text(trim = TRUE)
    constructed_vars <- paste(constructed_vars, collapse = " | ")  # Concatenate if multiple
    
    if (!is.na(var_name) && var_name != "") {
      data_list[[var_name]] <- data.frame(
        variable = var_name,
        cycle = cycle,
        question_text_de = ifelse(language == "DE", question_text, NA),
        question_text_en = ifelse(language == "EN", question_text, NA),
        responses_de = ifelse(language == "DE", responses_str, NA),
        responses_en = ifelse(language == "EN", responses_str, NA),
        constructed_vars = ifelse(constructed_vars != "", constructed_vars, NA),
        stringsAsFactors = FALSE
      )
    }
  }
  
  return(bind_rows(data_list))
}

# here one could add more questionnaires/languages
# note that I had to ask PIAAC FDZ directly for the US_Cycle1 file,
# the OECD website had only the Spanish version under "United States" (09.03.2025)
# but the file is different, does not parse as well

files <- list(
  list(here("Data", "DE_Cycle1.htm"), "DE", "Cycle 1"),
  list(here("Data", "UK_Cycle1.htm"), "EN", "Cycle 1")
  #list(here("Data", "US_Cycle1.html"), "EN", "Cycle 1")
)

# Parse files
df_list <- lapply(files, function(f) parse_piaac_html(f[[1]], f[[2]], f[[3]]))

# Combine all extracted data
df_combined <- bind_rows(df_list)

# merge German and English 
df_piaac_c1 <- df_combined %>%
  group_by(variable, cycle) %>%
  summarise(
    question_text_de = first(na.omit(question_text_de)),
    question_text_en = first(na.omit(question_text_en)),
    responses_de = first(na.omit(responses_de)),
    responses_en = first(na.omit(responses_en)),
    constructed_vars = first(na.omit(constructed_vars)),
    .groups = "drop"
  )


df_piaac_c1$variable <- gsub("\\[|\\]", "", df_piaac_c1$variable)


