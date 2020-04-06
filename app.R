# Shiny app that allows users to enter their home info and 

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(data.table)
library(glue)
library(parsnip)
library(xgboost)
library(shinyjs)

modl_data_fnl <- readRDS("data/modl_data_fnl.RDS")
lm_modl <- readRDS("models/lm_modl.RDS")
lm_train_data <- readRDS("data/lm_train_data.RDS")
xgb_modl <- readRDS("models/xgb_modl.RDS")
xgb_train_data <- readRDS("data/xgb_train_data.RDS")
source("funs.R")


# App UI ---------------------------------------------------------------

ui <- shiny::fluidPage(
  
  # Page Bootstrap theme ----
  theme = shinythemes::shinytheme(theme = "spacelab"),
  
  # Title of fluidPage (no appearance)
  title = "Home Sale Price Estimator for Tolland County, Connecticut", 
  
  # Navbar  ----
  shiny::navbarPage(
    
    # Set the navbar title, embedded with hyperlink to Ketchbrook website
    title = "Ketchbrook Analytics" %>% 
      shiny::a(
        href = "https://www.ketchbrookanalytics.com/", 
        target = "_blank"
      ), 
    
    collapsible = T, 
    
    # "Model" tab on the navbar ----
    shiny::tabPanel(
      title = "Model", 
      value = "nav_page_1", 
      
      # Add title and subtitle
      shiny::div(
        shiny::h1(
          "Home Sale Price Estimator", 
          shiny::tags$small("for Tolland County, Connecticut")
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
      
    ), 
    # "About" tab on the navbar ----
    shiny::tabPanel(
      title = "About", 
      value = "nav_page_2", 
      
      shiny::div(
        id = "about"
      ), 
      
      # JumboTron Ad for Ketchbrook ----
      shiny::fluidRow(
        shiny::div(
          class = "jumbotron", 
          shiny::h1("Enjoying This App?"), 
          shiny::p(
            class = "lead", 
            "Check out what else Ketchbrook Analytics can do"
          ), 
          shiny::a(
            class = "btn btn-primary btn-lg", 
            href = "https://www.ketchbrookanalytics.com/", 
            target = "_blank", 
            "Visit Us"
          )
        )
      ), 
      
      # Header for model information ----
      shiny::fluidRow(
        shiny::p(
          class = "lead", 
          "Models are only as good as their assumptions & the data they were trained with."
        ), 
        shiny::p("Here's some more information on the models used in this app:")
      ), 
      
      shinyjs::useShinyjs(), 
      
      shiny::fluidRow(
        
        shiny::column(
          width = 12, 
          class = "well", 
          
          shiny::p(
            class = "lead", 
            "Data"
          ), 
          
          shiny::actionButton(
            inputId = "data_toggle_button", 
            label = "See More"
          ), 
          
          shiny::div(
            
            id = "data_toggle_info", 
            
            shiny::br(), 
            
            shiny::textInput(
              inputId = "first_name", 
              label = "First Name", 
              placeholder = "Enter your first name"
            )
            
          ) %>% shinyjs::hidden()
          
        )
        
        )
      
    )
    
  )
  
)


# App Server --------------------------------------------------------------

server <- function(input, output, session) {
  
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
      cpipctchange = get_cpipctchange()
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
  
  # Toggle Data Info
  shinyjs::onclick(id = "data_toggle_button", {
    
    shinyjs::toggle(
      id = "data_toggle_info", 
      anim = T 
    )
    
  })
  
}

shinyApp(ui, server)