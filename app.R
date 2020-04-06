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
    
    # "App" tab on the navbar ----
    shiny::tabPanel(
      title = "App", 
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
    
    # "Help" page on the navbar ----
    shiny::tabPanel(
      title = "Help", 
      value = "nav_page2", 
      
      shiny::div(
        id = "help_page", 
        class = "well", 
        
        shiny::p(
          class = "lead", 
          "How Do I Interpret the Chart?"
        ), 
        
        shiny::p(
          "The chart on the", 
          shiny::strong("App"), 
          "page is a", 
          shiny::strong(
            shiny::em("LIME Explainer Chart")
          ), 
          ". This chart displays:"
        ), 
        
        shiny::tags$ol(
          shiny::tags$li(
            "The top three most influential variables in determining the model's output score for that observation"
          ), 
          shiny::tags$li(
            "The criteria that made that variable influential to the model output for that observation"
          ), 
          shiny::tags$li(
            "Whether or not that variable increased or decreased the model output value (sale price) for that observation"
          ), 
          shiny::tags$li(
            "The relative importance of each variable for determining the model's output score for that observation"
          )
        ), 
        
        shiny::h3("Example"), 
        
        shiny::p("Let's walk through an example of what we can interpret from the chart below using the linear regression model"), 
        
        shiny::tags$ol(
          shiny::tags$li(
            glue::glue(
              "The most important feature was the \"yearBuilt\" variable, specifically the fact", 
              "that the home was built after 1988. In the case of this observation, this variable had a positive impact on the", 
              "model output value (it increased the predicted sale price).", 
              .sep = " "
            )
          ), 
          shiny::tags$li(
            glue::glue(
              "The second-most important feature was the \"finishedSqFt\" variable, specifically the fact", 
              "that the home had square footage between 1,748 and 2,256. In the case of this observation, this variable had a negative impact on the", 
              "model output value (it decreased the predicted sale price).", 
              .sep = " "
            )
          ), 
          shiny::tags$li(
            glue::glue(
              "The third-most important feature was the \"city\" variable, specifically the fact", 
              "that the home was located in Stafford Springs, Connecticut. In the case of this observation, this variable had a negative impact on the", 
              "model output value (it decreased the predicted sale price).", 
              .sep = " "
            )
          ), 
          shiny::tags$li(
            "The linear regression model is predicting a sale price of $244,535."
          )
        ), 
        
        shiny::tags$img(
          class = "img img-responsive", 
          src = "chart_explanations_full.png", 
          style = "width:1200px;"   # image size is responsive up to 200px
        )
        
      )
    ), 
    
    # "About" page on the navbar ----
    shiny::tabPanel(
      title = "About", 
      value = "nav_page_3", 
      
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
          "Models are only as good as their assumptions and the data they were trained with."
        ), 
        shiny::p("Here's some more information on the models and data used in this app:")
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
        
      ), 
      
      shiny::fluidRow(
        
        shiny::column(
          width = 12, 
          class = "well", 
          
          shiny::p(
            class = "lead", 
            "Models"
          ), 
          
          # Model toggle button ----
          shiny::actionButton(
            inputId = "model_toggle_button", 
            label = "Show/Hide"
          ), 
          
          shiny::div(
            
            id = "model_toggle_info", 
            
            shiny::br(), 
            
            shiny::uiOutput(
              outputId = "model_toggle_text"
            )
            
          ) %>% shinyjs::hidden()
          
        )
        
      ), 
      
      shiny::fluidRow(
        
        shiny::column(
          width = 12, 
          class = "well", 
          
          shiny::p(
            class = "lead", 
            "Chart"
          ), 
          
          # Chart toggle button ----
          shiny::actionButton(
            inputId = "chart_toggle_button", 
            label = "Show/Hide"
          ), 
          
          shiny::div(
            
            id = "chart_toggle_info", 
            
            shiny::br(), 
            
            shiny::span(
              glue::glue(
              "The chart seen in the application is produced via a technique called \"LIME\",", 
              "which stands for \"Local Interpretable Model-Agnostic Explanations\". The original", 
              "paper citing the methodology can be found", 
              .sep = " "
              ), 
              shiny::a(
                href = "https://arxiv.org/abs/1602.04938", 
                target = "_blank", 
                "here"
              ), 
              glue::glue(
                ". This techique fits a linear model through the given observation, then looks at", 
                "the nearest (hence, \"Local\") data points around that particular observation to gauge", 
                "how the model performs at those other points in the vicinity compared to the observation", 
                "at hand.", 
                .sep = " "
              )
              
            )
            
          ) %>% shinyjs::hidden()
          
        )
        
      )
      
    ), 
    
    shiny::tabPanel(
      title = "Use Cases", 
      value = "nav_page_4", 
      
      shiny::div(
        id = "using_app"
      ), 
      
      shiny::h1("Possible Use Cases"), 
      
      shiny::p(
        glue::glue(
        "Why might you need an application from Ketchbrook Analytics to help you build user-friendly", 
        "model interpretation capabilities around your predictive models? Here are some reasons why", 
        "your business might benefit from a solution like this:", 
        .sep = " "
        )
      ), 
      
      shiny::div(
        class = "well", 
        
        shiny::h3("Auditing Your Internal Models"),
        
        shiny::p(
          glue::glue(
            "This type of application can serve as a tool for your internal audit team to interface with", 
            "and help them gain an understanding about your model, the variables it uses, the", 
            "effect each variable has on the model output, and to back-test a particular observation.", 
            "This can be extremely useful to hand your external auditors as well, allowing them to get", 
            "up to speed quicker with your model as opposed to strictly reading documentation.", 
            .sep = " "
          )
        )
      ), 
      
      shiny::div(
        class = "well", 
        
        shiny::h3("Presentation to Management & Board of Directors"), 
        
        shiny::p(
          glue::glue(
            "If you have a model in place in your organization, it can be difficult to provide something", 
            "tangible to non-technical stakeholders who need to gain comfort from an organizational risk", 
            "perspective prior to relying on that model to impact business decisions and outcomes.", 
            "Providing them with a user interface where they can see the model score an observation", 
            "can yield incredible dividends in terms of bridging the gap between the modeling team", 
            "and the managerial suite.", 
            .sep = " "
          )
        )
      ), 
      
      shiny::div(
        class = "well", 
        
        shiny::h3("Validation within the Modeling Team"), 
        
        shiny::p(
          glue::glue(
            "This tool can be useful as a gut-check for the model developer to ensure that the model they", 
            "are developing is stable across all variables. Furthermore, the team member validating the", 
            "model developer's work can use this tool to provide a sensitivity analysis of the model.", 
            .sep = " "
          )
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
  
  
  # Toggle Model Info
  shinyjs::onclick(id = "model_toggle_button", {
    
    shinyjs::toggle(
      id = "model_toggle_info", 
      anim = T 
    )
    
  })
  
  # Render section in "About" navbar page that details the models
  output$model_toggle_text <- shiny::renderUI({
    
    shiny::div(
      shiny::p(
        glue::glue(
          "Two models were developed for use in this application. The final predicted value", 
          "seen in the user interface is the result of taking the average of each model's", 
          "predicted value for the sale price. This ensemble approach was found to yield the", 
          "lowest error (RMSE and MAE).", 
          .sep = " "
        )
      ), 
      
      # Linear Regression detail ----
      shiny::span(
        class = "lead", 
        shiny::a(
          href = "https://en.wikipedia.org/wiki/Linear_regression", 
          target = "_blank", 
          "Linear Regression"
        )
      ), 
      
      shiny::p(
        glue::glue(
          "The first model uses simple linear regression, which allows for a straightforward", 
          "interpretation of the model coefficients, and each variable's impact on the output", 
          "predicted value.", 
          .sep = " "
        )
      ), 
      
      # Linear Regression detail ----
      shiny::span(
        class = "lead", 
        shiny::a(
          href = "https://xgboost.readthedocs.io/en/latest/", 
          target = "_blank", 
          "XGBoost"
        )
      ), 
      
      shiny::p(
        glue::glue(
          "The second model uses a specific gradient boosting algorithm, called \"XGBoost\".", 
          "There are both classification and regression interpretations of this algorithm,", 
          "and the regression implementation was used for our home sale price prediction model.", 
          "This algorithm produces a non-linear \"black box\" model, so the coefficients are not", 
          "easily interpretable. This is where the power of the LIME technique shines, allowing us", 
          "to gain some insight into how the model is making its predictions.", 
          .sep = " "
        )
      )
      
    )
    
  })
  
  # Toggle Chart Info
  shinyjs::onclick(id = "chart_toggle_button", {
    
    shinyjs::toggle(
      id = "chart_toggle_info", 
      anim = T 
    )
    
  })
  
  
  
}

shinyApp(ui, server)