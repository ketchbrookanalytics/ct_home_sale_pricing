# Shiny app that allows users to enter their home info and 

library(shiny)
library(tidyverse)

modl_data_fnl <- readRDS("modl_data_fnl.RDS")
lm_modl <- readRDS("lm_modl.RDS")
xgb_modl <- readRDS("xgb_modl.RDS")
source("funs.R")


# Get CPI Data ------------------------------------------------------------

macro.env <- new.env()
quantmod::getSymbols.FRED("CPIAUCSL", env = macro.env)
cpi_data <- macro.env$CPIAUCSL %>% 
  data.table::as.data.table() %>% 
  dplyr::rename(date = index) %>% 
  dplyr::mutate(cpipctchange = (CPIAUCSL - dplyr::lag(CPIAUCSL, 12)) / dplyr::lag(CPIAUCSL, 12)) %>% 
  dplyr::filter(lubridate::year(date) >= 1987) %>% 
  dplyr::select(-CPIAUCSL) %>% 
  dplyr::slice(n())



# Build App ---------------------------------------------------------------

ui <- fluidPage(
  shiny::column(
    width = 4, 
    shiny::wellPanel(
      shiny::sliderInput(
        inputId = "bedrooms_slider", 
        label = "Number of Bedrooms", 
        min = 1, 
        max = 6, 
        value = 3, 
        step = 1
      ), 
      shiny::sliderInput(
        inputId = "bathrooms_slider", 
        label = "Number of Bathrooms", 
        min = 1, 
        max = 7, 
        value = 2, 
        step = 0.5
      ), 
      shinyWidgets::pickerInput(
        inputId = "city_picker", 
        label = "Select City", 
        choices = c(unique(modl_data_fnl$city))
      ), 
      shiny::numericInput(
        inputId = "sqft_input", 
        label = "Enter Square Footage", 
        value = 1800, 
        min = 400, 
        max = 10000
      ), 
      shiny::numericInput(
        inputId = "year_built_input", 
        label = "Enter Year Built", 
        value = 2000, 
        min = 400, 
        max = 10000
      ), 
      # shiny::dateInput(
      #   inputId = "date_sold_input", 
      #   label = "Enter Date of Sale \n (or leave unchanged if you would like to simulate sale as of today's date)", 
      #   value = lubridate::today()
      # ), 
      shiny::actionButton(
        inputId = "predict_button", 
        label = "Generate Prediction", 
        icon = shiny::icon("download")
      )#,
      # shiny::verbatimTextOutput(
      #   "season"
      # )
    )
  ), 
  shiny::column(
    width = 8, 
    DT::DTOutput(
      "temp_tbl"
    )
  )
)


server <- function(input, output, session) {
  
  output$season <- renderText({
    dplyr::case_when(
      lubridate::month(lubridate::today()) %in% 3:5 ~ "Spring", 
      lubridate::month(lubridate::today()) %in% 6:8 ~ "Summer", 
      lubridate::month(lubridate::today()) %in% 9:11 ~ "Fall", 
      TRUE ~ "Winter"
    )
    
  })
  
  df_eval <- shiny::reactive({
    
    data.frame(
      city = input$city_picker, 
      bathrooms = input$bathrooms_slider, 
      finishedSqFt = input$sqft_input, 
      yearBuilt = input$year_built_input, 
      season = dplyr::case_when(
        lubridate::month(lubridate::today()) %in% 3:5 ~ "Spring", 
        lubridate::month(lubridate::today()) %in% 6:8 ~ "Summer", 
        lubridate::month(lubridate::today()) %in% 9:11 ~ "Fall", 
        TRUE ~ "Winter"
      ), 
      cpipctchange = cpi_data$cpipctchange
    )
    
  })
  
  
  output$temp_tbl <- DT::renderDataTable(
    df_eval()
  )
  
  # output$predicted_value <- shiny::renderPrint({
  # 
  #   generate_model_pred(model = lm_modl,
  #                       data = df_eval)
  # 
  # })
  
  
}

shinyApp(ui, server)