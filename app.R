# Shiny app that allows users to enter their home info and 

library(shiny)
library(tidyverse)

# readRDS("xgb_modl.RDS")
readRDS("modl_data_fnl.RDS")

ui <- fluidPage(
  
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
  shiny::verbatimTextOutput(
    "season"
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
    
}

shinyApp(ui, server)