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

options(scipen = 999)

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
            "Check out what else Ketchbrook Analytics can do for you."
          ), 
          shiny::a(
            class = "btn btn-primary btn-lg", 
            href = "https://www.ketchbrookanalytics.com/", 
            target = "_blank", 
            "Visit Us"
          )
        )
      ), 
      
      shiny::hr(), 
      
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
          
          # Data toggle button ----
          shiny::actionButton(
            inputId = "data_toggle_button", 
            label = "Show/Hide"
          ), 
          
          shiny::div(
            
            id = "data_toggle_info", 
            
            shiny::br(), 
            
            shiny::uiOutput(
              outputId = "data_toggle_text"
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
  
  output$data_toggle_text <- shiny::renderUI({
    
    shiny::div(
      shiny::p(
        glue::glue(
          "There are multiple sources of data that were used in developing the models.", 
          "The final models were trained using",
          "{format(nrow(lm_train_data), big.mark = \",\")} observations across {ncol(lm_train_data)} column variables,",
          "including the dependent variable, sale price.", 
          .sep = " "
        )
      ), 
      
      shiny::p("Data was gathered from the following sources:"), 
      
      shiny::tags$ul(
        
        # Data Sources - CT Open Data ----
        shiny::tags$li(
          shiny::strong(
            shiny::a(
              href = "https://data.ct.gov/Housing-and-Development/Real-Estate-Sales-2001-2017/5mzw-sjtu", 
              target = "_blank", 
              "Connecticut Open Data"
            ) 
          ), 
          ": includes data about historical home sales in Connecticut, accessed via the ", 
          shiny::a(
            href = "https://github.com/Chicago/RSocrata", 
            target = "_blank", 
            "RSocrata API"
        )
      ), 
      # Data Sources - Zillow ----
      shiny::tags$li(
        shiny::strong(
          shiny::a(
            href = "https://www.zillow.com", 
            target = "_blank", 
            "Zillow"
          ) 
        ), 
        ": retrieved data about home details (number of bedrooms, number of bathrooms, etc.), accessed via the ", 
        shiny::a(
          href = "https://github.com/stharms/realEstAnalytics.r", 
          target = "_blank", 
          "realEstAnalytics interface to the Zillow API"
        )
      )
      ), 
      
      shiny::p(
        glue::glue(
          "Here are the final variables chosen & engineered for these models, along with their descriptions", 
          "(Note: you may be surprised that the number of bedrooms was not used.", 
          "This omission was due to high collinearity between the number of bathrooms, and the number of bathrooms", 
          "were found to be more predictive of the sale price than the number of bedrooms.):",
          .sep = " "
        )
      ), 
      
      shiny::tags$ul(
        
        # Data - Bullet Point - City ----
        shiny::tags$li(
          shiny::strong("City: "), 
          glue::glue(
            "a categorical independent variable representing which one of the",
            "{length(unique(lm_train_data$city))}", 
            "unique towns/cities in Tolland County, Connecticut, that the sale occurred", 
            "In the app, the user has the ability to choose the value (city)",
            "that they want the model to use in its prediction calculation.", 
            .sep = " "
          )
        ), 
        
        # Data - Bullet Point - Bathrooms ----
        shiny::tags$li(
          shiny::strong("Bathrooms: "), 
          glue::glue(
            "a continuous independent variable representing the number of bathrooms in the", 
            "home at the time of the sale. This variable had values between", 
            "{min(lm_train_data$bathrooms)} and {max(lm_train_data$bathrooms)}.", 
            "In the app, the user has the ability to choose the value (number of bathrooms)",
            "that they want the model to use in its prediction calculation.",
            .sep = " "
          )
        ), 
        
        # Data - Bullet Point - Square Footage ----
        shiny::tags$li(
          shiny::strong("Square Footage: "), 
          glue::glue(
            "a continuous independent variable representing the finished square footage of the", 
            "home at the time of the sale. This variable had values between", 
            "{min(lm_train_data$finishedSqFt)} and {format(max(lm_train_data$finishedSqFt), big.mark = \",\")}.", 
            "In the app, the user has the ability to choose the value (square footage)",
            "that they want the model to use in its prediction calculation.",
            .sep = " "
          )
        ), 
        
        # Data - Bullet Point - Year Built ----
        shiny::tags$li(
          shiny::strong("Year Built: "), 
          glue::glue(
            "a continuous independent variable representing the year the home was built.", 
            "This variable had values between", 
            "{min(lm_train_data$yearBuilt)} and {max(lm_train_data$yearBuilt)}.", 
            "In the app, the user has the ability to choose the value (year built)",
            "that they want the model to use in its prediction calculation.",
            .sep = " "
          )
        ), 
        
        # Data - Bullet Point - CPI ----
        shiny::tags$li(
          shiny::strong("Consumer Price Index (% Change from 12 Months Prior): "), 
          glue::glue(
            "a continuous independent variable representing the percent change in the", 
            "Consumer Price Index between the time of the sale and the Consumer Price Index", 
            "12 months prior. The purpose of this variable was to serve as a proxy variable", 
            "for the year of the sale, and to help account for some of the time series elements", 
            "seen in the raw data (e.g., increasing trend in home sale prices over time).", 
            "In the app, this value is generated behind the scenes. When a user opens the app,", 
            "the most recent data for the Consumer Price Index is pulled in via the FRED API", 
            "that they want the model to use in its prediction calculation.",
            .sep = " "
          )
        ), 
        
        # Data - Bullet Point - Season ----
        shiny::tags$li(
          shiny::strong("Season: "), 
          glue::glue(
            "a categorical independent variable representing the season during which the sale occurred.", 
            "This variable was engineered based upon the date of the sale. Sales in December, January,", 
            "or February were coded to a value of \"Winter\". Sales that took place in March, April, or May", 
            "were coded to a value of \"Spring\". Sales that took place in June, July, or August were coded", 
            "to a value of \"Summer\". Sales that took place in Septebmer, October, or November were coded", 
            "to a value of \"Fall\". In the app, this value is generated behind the scenes. When a user", 
            "opens the app, the system date is capture and translated into the season based upon the month", 
            "of the system date for the model to use in its prediction calculation.",
            .sep = " "
          )
        ), 
        
        # Data - Bullet Point - Sale Price ----
        shiny::tags$li(
          shiny::strong("Sale Price: "), 
          glue::glue(
            "the numeric idependent variable representing the sale price. For data preparation purposes", 
            "(specifically for joining to the Zillow data, which represents only current home data),", 
            "only the most recent sale price for a given address was used. Additionally, a log transformation", 
            "was applied to this variable for helping to satisfy statistical normality & stationarity assumptions.", 
            "Consequently, the model output gets transformed via the exponential function prior to consumption by", 
            "the end user.", 
            "In the training data, this dependent variable had values between", 
            "{format(exp(min(lm_train_data$logLastSoldPrice)), big.mark = \",\")} and",
            "{format(exp(max(lm_train_data$logLastSoldPrice)), big.mark = \",\")}.", 
            "In the app, this is the output \"prediction\" of the sale price returned to the user.",
            .sep = " "
          )
        )
        
      ), 
      
      # Data Preparation Steps ----
      shiny::p("There were some additional data preparation steps that were taken in the training data:"), 
      
      shiny::tags$ul(
        
        shiny::tags$li(
          "Only residential properties were included."
        ), 
        shiny::tags$li(
          "Sales with a non-use code were excluded. For more information, visit the Connecticut Open Data link above."
        ), 
        shiny::tags$li(
          "Only sales between $100,000 and $1,000,000 were included, to ensure stabality against outliers."
        ), 
        shiny::tags$li(
          "Only single- or multi-family homes were included."
        ), 
        shiny::tags$li(
          "Only residential properties were included."
        ), 
        shiny::tags$li(
          "Towns with less than 100 observations (sales) in the dataset were excluded."
        ), 
        shiny::tags$li(
          "Only properties with between 1 and 7 bathrooms were included, to ensure stabality against outliers."
        ), 
        shiny::tags$li(
          "Only properties with finished square footage betwen 400 and 10,000 were included, to ensure stabality against outliers."
        ), 
        shiny::tags$li(
          "Only properties with finished square footage betwen 400 and 10,000 were included, to ensure stabality against outliers."
        )
        
      )
      
    )
    
  })
  

  
}

shinyApp(ui, server)