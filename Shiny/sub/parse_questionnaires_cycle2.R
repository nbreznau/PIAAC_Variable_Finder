

# Function to parse a single .html file
parse_piaac_html_new <- function(file_path, language, cycle) {
  html_doc <- read_html(file_path)
  
  # Extract all question items
  items <- html_doc %>% html_nodes(".itemGroup")
  
  if (length(items) == 0) {
    message("No items found in ", file_path)
    return(data.frame())  # Return an empty dataframe if no items are found
  }
  
  data_list <- list()
  
  for (item in items) {
    # Extract variable name **only from `.itemGroup` ID**
    var_name <- item %>% html_attr("id")  # Extracts "A2_Q01", "B2_D03", etc.
    
    question_text <- item %>%
      html_nodes(xpath = ".//tr[td[contains(text(), 'Question')]]/td[2]") %>%
      html_text(trim = TRUE)
    
    question_text <- ifelse(length(question_text) > 0, first(question_text), NA)
    

    response_nodes <- item %>% html_nodes(".responseTable tr")
    responses_list <- list()
    
    for (row in response_nodes) {
      code <- row %>% html_node(".label_answer") %>% html_text(trim = TRUE)
      text <- row %>% html_nodes("td") %>% html_text(trim = TRUE)
      text <- text[length(text)]  # Get last element if multiple td's exist
      
      if (!is.na(code) && code != "" && !is.na(text) && text != "") {
        responses_list[[code]] <- text
      }
    }
    
    responses_str <- ifelse(length(responses_list) > 0, 
                            paste(names(responses_list), responses_list, sep = ": ", collapse = " | "), NA)
    
    constructed_vars <- item %>%
      html_nodes(".routing") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = " | ")
    
    # Store cleaned results
    if (!is.na(var_name) && var_name != "") {
      data_list[[var_name]] <- data.frame(
        variable = var_name,  # ✅ Now correctly capturing variable names
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


files_html <- list(
  list(here("Data", "DE_Cycle2.html"), "DE", "Cycle 2"),
  list(here("Data", "UK_Cycle2.html"), "EN", "Cycle 2")
)


df_list_html <- lapply(files_html, function(f) parse_piaac_html_new(f[[1]], f[[2]], f[[3]]))


df_piaac_c2 <- bind_rows(df_list_html)


df_piaac_c2 <- df_piaac_c2 %>%
  group_by(variable, cycle) %>%
  summarise(
    question_text_de = first(na.omit(question_text_de)),
    question_text_en = first(na.omit(question_text_en)),
    responses_de = first(na.omit(responses_de)),
    responses_en = first(na.omit(responses_en)),
    constructed_vars = first(na.omit(constructed_vars)),
    .groups = "drop"
  )

rm(df_combined, df_list, df_list_html, files, files_html, parse_piaac_html, parse_piaac_html_new)
