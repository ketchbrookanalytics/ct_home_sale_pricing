# Shiny app that allows users to enter their home info and 

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(glue)
library(parsnip)
library(xgboost)

modl_data_fnl <- readRDS("data/modl_data_fnl.RDS")
lm_modl <- readRDS("models/lm_modl.RDS")
lm_train_data <- readRDS("data/lm_train_data.RDS")
xgb_modl <- readRDS("models/xgb_modl.RDS")
xgb_train_data <- readRDS("data/xgb_train_data.RDS")
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



# App UI ---------------------------------------------------------------

ui <- fluidPage(
  
  # Page Bootstrap theme ----
  # theme = shinythemes::shinytheme(theme = "slate"), 
  shinythemes::themeSelector(), 
  
  
  # App Overall Styling ----
  # shinyWidgets::setBackgroundColor(
  #   color = c("#F7FBFF", 
  #             "#627D9F"), 
  #   gradient = "linear", 
  #   direction = c("bottom")
  # ), 
  
  # Title of fluidPage (no appearance)
  title = "Home Sale Price Estimator for Tolland County, Connecticut", 
  
  # Add Ketchbrook logo banner
  # shiny::HTML("<div id=\"ketchbrook_banner\" class=\"shiny-image-output\" style=\"width: 100% ; height: 150px\"></div>"), 
  shiny::div(id = "ketchbrook_logo_link"), 
  
  # shiny::div(
  #   class = "jumbotron", 
  #   style = "background:url('Ketchbrook_Logo_nobackground.png'); background-size:cover;"
  # ),
  
  shiny::div(
    class = "container", 
    shiny::column(
      width = 12, 
      shiny::tags$img(
        class = "img img-responsive", 
        src = "Ketchbrook_Logo_nobackground.png", 
        align = "center", 
        style = "width:800px;"   # image size is responsive up to 800px
      ) %>%
        shiny::a(
          href = "https://www.ketchbrookanalytics.com/",
          target = "_blank"
        )
    )
  ),
  
  # Add title and subtitle
  shiny::div(
    shiny::h1(
      "Home Sale Price Estimator" 
    ), 
    shiny::h3(
      shiny::span(
        "Tolland County, Connecticut", 
        style = "font-family: Impact"
      )
    )
  ), 
  
  # Insert line break
  shiny::hr(), 
  
  
  shiny::div(
    shiny::column(
      width = 4, 
      
      # Add wellPanel that contains user-input objects
      shiny::wellPanel(
        shiny::sliderInput(
          inputId = "bathrooms_slider", 
          label = "Number of Bathrooms", 
          min = 1, 
          max = 7, 
          value = 2, 
          step = 0.5
        ), 
        
        # City drop-down menu ----
        shinyWidgets::pickerInput(
          inputId = "city_picker", 
          label = "Select City", 
          choices = c(unique(modl_data_fnl$city))
        ), 
        
        # Square Footage numeric input ----
        shiny::numericInput(
          inputId = "sqft_input", 
          label = "Enter Square Footage", 
          value = 1800, 
          min = 400, 
          max = 10000
        ), 
        
        # Year Built numeric input ----
        shiny::numericInput(
          inputId = "year_built_input", 
          label = "Enter Year Built", 
          value = 2000, 
          min = 1500, 
          max = as.numeric(lubridate::year(lubridate::today()))
        ), 
        
        # Action Button to generate predictions ----
        shiny::actionButton(
          inputId = "predict_button", 
          label = "Generate Prediction", 
          icon = shiny::icon("download")
        )
      )
    )
  ), 
  
  shiny::div(
    shiny::column(
      width = 8, 
      shiny::wellPanel(
        
        # Estimated Home Sale Price message ----
        shiny::span(
          "Today's Estimated Sale Price: ", 
          style = "font-size: 150%"
        ), 
        shiny::br(), 
        shiny::span(
          shiny::textOutput(
            "pred_text"
          ), 
          style = "font-size: 200%"
        ), 
        style = "background: #F0F0F0"
      ), 
      
      shiny::div(
        
        # Panel for LIME charts ----
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Linear Regression", 
            shiny::plotOutput(
              "lime_plot_lm"
            )
          ), 
          shiny::tabPanel(
            title = "XGBoost", 
            shiny::plotOutput(
              "lime_plot_xgb"
            )
          )
        )
      )
    )
  )
)



# App Server --------------------------------------------------------------

server <- function(input, output, session) {
  
  # Render Ketchbrook banner image ----
  output$ketchbrook_banner <- shiny::renderImage({
    
    list(
      src = "img/Ketchbrook_Logo_nobackground.png", 
      contentType = "image/png", 
      Width = 1000, 
      height = (1000 * 58) / 368,
      alt = "KetchbrookLogo"
    )
    
  }, deleteFile = F)
  
  
  # Render season text ----
  # output$season <- renderText({
  #   dplyr::case_when(
  #     lubridate::month(lubridate::today()) %in% 3:5 ~ "Spring", 
  #     lubridate::month(lubridate::today()) %in% 6:8 ~ "Summer", 
  #     lubridate::month(lubridate::today()) %in% 9:11 ~ "Fall", 
  #     TRUE ~ "Winter"
  #   )
  #   
  # })
  
  
  # Reactive dataframe from user inputs ----
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
  
  
  # Reactive value for home sale price estimate
  pred_value <- shiny::reactive({
    
    generate_ensemble_pred(
      model1 = lm_modl, 
      model2 = xgb_modl, 
      data = df_eval()
    )
    
  })
  
  
  # Render estimated home sale price as text ----
  output$pred_text <- shiny::renderText({
    
    glue::glue(
      "${format(generate_ensemble_pred(model1 = lm_modl, model2 = xgb_modl, data = df_eval()), big.mark = \",\")}"
    )
    
  })
  
  
  # Render LIME plot for lm model ----
  output$lime_plot_lm <- shiny::renderPlot({
    
    generate_lime_chart(
      model = lm_modl, 
      train_data = lm_train_data, 
      new_data = df_eval()
    )
    
  })
  
  
  # Render LIME plot for xgboost model ----
  output$lime_plot_xgb <- shiny::renderPlot({
    
    generate_lime_chart(
      model = xgb_modl, 
      train_data = xgb_train_data, 
      new_data = df_eval()
    )
    
  })
  
}

shinyApp(ui, server)