# Shiny app that allows users to enter their home info and 

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(glue)
library(parsnip)
library(xgboost)

modl_data_fnl <- readRDS("modl_data_fnl.RDS")
lm_modl <- readRDS("lm_modl.RDS")
lm_train_data <- readRDS("lm_train_data.RDS")
xgb_modl <- readRDS("xgb_modl.RDS")
xgb_train_data <- readRDS("xgb_train_data.RDS")
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
  
  shinyWidgets::setBackgroundColor(
    color = c("#F7FBFF", 
              "#2171B5"), 
    gradient = "linear", 
    direction = c("bottom")
  ), 
  
  title = "Home Sale Price Estimator for Tolland County, Connecticut", 
  
  # shiny::HTML("<a href=\"https://www.ketchbrookanalytics.com\"><img border=\"0\" alt=\"test\" src=\"Ketchbrook_Logo_nobackground.png\" width=\"100\" height=\"150\"></a>"), 
  
  shiny::HTML("<div id=\"ketchbrook_banner\" class=\"shiny-image-output\" style=\"width: 100% ; height: 150px\"></div>"),
  
  shiny::div(
    shiny::h1("Tolland County, Connecticut Home Sale Price Estimator"), 
    shiny::p("Developed by Ketchbrook Analytics")
  ), 
  
  shiny::div(
    shiny::column(
      width = 4, 
      shiny::wellPanel(
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
    )
  ), 
  
  shiny::div(
    shiny::column(
      width = 8, 
      shiny::textOutput(
        "pred_text"
      ), 
      shinydashboard::valueBoxOutput(
        "pred"
      ), 
      shiny::div(
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Linear Regression", 
            shiny::plotOutput(
              "lime_plot_lm"
            )
          ), 
          shiny::tabPanel(
            "XGBoost", 
            shiny::plotOutput(
              "lime_plot_xgb"
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  output$ketchbrook_banner <- shiny::renderImage({
    
    list(
      src = "img/Ketchbrook_Logo_nobackground.png", 
      contentType = "image/png", 
      Width = 1000, 
      height = (1000 * 58) / 368,
      alt = "KetchbrookLogo"
    )
    
  }, deleteFile = F)
  
  output$season <- renderText({
    dplyr::case_when(
      lubridate::month(lubridate::today()) %in% 3:5 ~ "Spring", 
      lubridate::month(lubridate::today()) %in% 6:8 ~ "Summer", 
      lubridate::month(lubridate::today()) %in% 9:11 ~ "Fall", 
      TRUE ~ "Winter"
    )
    
  })
  
  df_eval <- shiny::eventReactive( input$predict_button, {
    
    tibble::tibble(
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
    
  }, ignoreNULL = FALSE)
  
  pred_value <- shiny::reactive({
    
    generate_ensemble_pred(
      model1 = lm_modl, 
      model2 = xgb_modl, 
      data = df_eval()
    )
    
  })
  
  output$pred_text <- shiny::renderText({
    
    pred_value()
    
  })
  
  output$pred <- shinydashboard::renderValueBox({
    
    shinydashboard::valueBox(
      "Title", 
      glue::glue(
        "Today's Estimated Sale Price: ${format(generate_ensemble_pred(model1 = lm_modl, model2 = xgb_modl, data = df_eval()), big.mark = \",\")}"
      ), 
      icon = shiny::icon("credit-card")
    )
    
  })
  
  
  # output$pred <- shiny::renderText({
  #   
  #   glue::glue(
  #     "Today's Estimated Sale Price: ${format(generate_ensemble_pred(model1 = lm_modl, model2 = xgb_modl, data = df_eval()), big.mark = \",\")}"
  #   )
  #   
  # })
  
  output$lime_plot_lm <- shiny::renderPlot({
    
    generate_lime_chart(
      model = lm_modl, 
      train_data = lm_train_data, 
      new_data = df_eval()
    )
    
  })
  
  output$lime_plot_xgb <- shiny::renderPlot({
    
    generate_lime_chart(
      model = xgb_modl, 
      train_data = xgb_train_data, 
      new_data = df_eval()
    )
    
  })
  
  
  
  
  
  
  # output$predicted_value <- shiny::renderPrint({
  # 
  #   generate_model_pred(model = lm_modl,
  #                       data = df_eval)
  # 
  # })
  
  
}

shinyApp(ui, server)