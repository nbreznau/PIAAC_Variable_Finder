library(shiny)
library(dplyr)
library(DT)
library(stringr)
library(here)

df <- readRDS("df.RDS")  

# this identifies if the system is web-based or local
is_local <- Sys.getenv("SHINY_PORT") == ""

guide_link <- if (is_local) {
  "The PIAAC Variable Finder Users Guide.pdf"
} else {
  "https://github.com/nbreznau/PIAAC_Variable_Finder/blob/main/Shiny/www/The%20PIAAC%20Variable%20Finder%20Users%20Guide.pdf"
}

ui <- fluidPage(
  titlePanel("PIAAC Variable Finder"),  
  
  tags$head(
    tags$style(HTML("
      #selected_row {
        white-space: pre-wrap;
        word-wrap: break-word;
        max-height: 300px;
        overflow-y: auto;
        border: 1px solid #ddd;
        padding: 10px;
        background-color: #f9f9f9;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,  
      textInput("search_term", "Main Search", value = ""),
      textInput("search_within", "Search Within Results", value = ""),
      
      checkboxInput("show_trend_vars", "Show Trend Variables", value = FALSE),
      checkboxInput("show_none_vars", "Show Only PUF Non-Missings", value = FALSE),
      
      tags$div(
        style = "font-size: smaller;",
        helpText("Search in German or English (British) through the questionnaires from Cycle 1 and Cycle 2. 
            You can also search variable names. Click on result to see more info.")
      ),
      
      tags$a(href = guide_link, strong("User's Guide"), target = "_blank", download = NA),
      
      br(), br(),
      
      tags$div(
        style = "font-size: smaller;",
        div(
          strong("License CCBY4.0"), br(),
          "Please cite as:", br(),
          strong("Breznau, Nate. 2025. PIAAC Variable Finder."), br(),
          tags$small(
            "German Institute for Adult Education - Leibniz Center for Lifelong Learning.", br(),
            tags$a(
              href = "https://doi.org/10.5281/zenodo.15817634",
              "https://doi.org/10.5281/zenodo.15817634",
              target = "_blank"
            )
          )
        ),
        
        
        br(),
        
        div(
          strong("Please report bugs or ideas:"), br(),
          tags$a(href = "mailto:breznau.nate@gmail.com", "Email")
        )
      ),
      
      br(),
      
      tags$img(src = "Logo_RGB_blau_englisch.jpg", width = "100%", alt = "DIE Logo")  
    ),
    
    mainPanel(
      width = 9,
      fluidRow(
        column(12, DTOutput("search_results"))
      ),
      fluidRow(
        column(12, uiOutput("selected_row"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    search_term <- input$search_term
    search_within <- input$search_within
    show_trend_vars <- input$show_trend_vars
    show_none_vars <- input$show_none_vars
    
    df_filtered <- df
    
    if (search_term != "") {
      df_filtered <- df_filtered %>%
        filter(
          str_detect(variable, regex(search_term, ignore_case = TRUE)) |
            str_detect(question_text_en, regex(search_term, ignore_case = TRUE)) |
            str_detect(question_text_de, regex(search_term, ignore_case = TRUE)) |
            str_detect(responses_en, regex(search_term, ignore_case = TRUE)) |
            str_detect(responses_de, regex(search_term, ignore_case = TRUE)) |
            str_detect(generic_label_en, regex(search_term, ignore_case = TRUE)) |
            str_detect(generic_label_de, regex(search_term, ignore_case = TRUE)) |
            str_detect(trend_var, regex(search_term, ignore_case = TRUE)) 
        )
    }
    
    if (search_within != "") {
      df_filtered <- df_filtered %>%
        filter(
          str_detect(variable, regex(search_within, ignore_case = TRUE)) |
            str_detect(question_text_en, regex(search_within, ignore_case = TRUE)) |
            str_detect(question_text_de, regex(search_within, ignore_case = TRUE)) |
            str_detect(responses_en, regex(search_within, ignore_case = TRUE)) |
            str_detect(responses_de, regex(search_within, ignore_case = TRUE)) |
            str_detect(generic_label_en, regex(search_within, ignore_case = TRUE)) |
            str_detect(generic_label_de, regex(search_within, ignore_case = TRUE)) |
            str_detect(trend_var, regex(search_within, ignore_case = TRUE))  
        )
    }
    
    if (show_trend_vars) {
      df_filtered <- df_filtered %>%
        filter(
          cycle == "both" | trend %in% c("Strict", "Soft") | !is.na(trend_var)
        )
    }
    
    if (show_none_vars) {
      df_filtered <- df_filtered %>%
        filter(
          none == "YES"
        )
    }
    
    df_filtered %>%
      mutate(
        display_label = coalesce(generic_label_en, question_text_en, question_text_de),
        underscore_present = str_detect(variable, "_"),
      ) %>%
      arrange(underscore_present, variable)  
  })
  
  output$search_results <- renderDT({
    filtered_data() %>%
      select(variable, display_label, cycle) %>%
      datatable(selection = "single", options = list(pageLength = 7))
  })
  
  selected_row_data <- reactive({
    req(input$search_results_rows_selected)
    df_selected <- filtered_data()  
    selected_index <- input$search_results_rows_selected
    
    df_selected[selected_index, , drop = FALSE]  
  })
  
  output$selected_row <- renderUI({
    row <- selected_row_data()
    
    if (nrow(row) == 0) return("No results.")
    
    explanation <- if (!is.na(row$soft_trend_explanation) && row$trend == "Soft") {
      paste0(" <strong>Explanation:</strong> ", row$soft_trend_explanation)
    } else {
      ""
    }
    
    HTML(paste0(
      "<strong>VARIABLE:</strong> ", row$variable, "<br>",
      "<strong>LABEL:</strong> ", row$generic_label_en, "<br>",
      "<strong>CYCLE:</strong> ", row$cycle, "<br>",
      "<strong>NON-MISSING in PUF:</strong> ", row$none, "<br>",
      "<strong>TREND STATUS:</strong> ", row$trend, explanation, "<br>", 
      "<strong>TREND VAR:</strong> ", row$trend_var, "<br>",
      "<br>",
      "<strong>QUESTION TEXT EN:</strong> ", row$question_text_en, "<br>",
      "<strong>RESPONSES EN:</strong> ", row$responses_en, "<br>",
      "<br>",
      "<strong>QUESTION TEXT DE:</strong> ", row$question_text_de, "<br>",
      "<strong>RESPONSES DE:</strong> ", row$responses_de, "<br>",
      "<br>",
      "<strong>Related variables:</strong> ",
      paste(na.omit(c(row$trend_var, row$ref_variables)), collapse = ", ")
    ))
  })
}

shinyApp(ui = ui, server = server)