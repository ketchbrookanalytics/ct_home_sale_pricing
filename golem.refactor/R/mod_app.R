#' app UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_app_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header("Home Sale Price Estimator (for Tolland County, Connecticut)"),
      bslib::layout_sidebar(

        sidebar = bslib::sidebar(
          open = "always",
          width = 400,
          
          shiny::sliderInput(
            inputId = ns("n_bathrooms"),
            label = "Number of Bathrooms",
            min = 1,
            max = 7,
            value = 2,
            step = 0.5
          ),

          shiny::selectInput(
            inputId = ns("city"), 
            label = "Select City", 
            choices = c(unique(modl_data_fnl$city))
          ),

          shiny::numericInput(
            inputId = ns("square_footage"), 
            label = "Enter Square Footage", 
            value = 1800, 
            min = 400, 
            max = 10000
          ), 
            
          shiny::numericInput(
            inputId = ns("year_built"), 
            label = "Enter Year Built", 
            value = 2000, 
            min = 1500, 
            max = as.numeric(lubridate::year(lubridate::today()))
          ),

          bslib::input_task_button(
            id = ns("generate_prediction"),
            label = "Generate Prediction"
          )

        ),

        bslib::value_box(
          title = "Today's Estimated Sale Price:",
          value = shiny::textOutput(ns("estimated_sale_price")),
          showcase = bsicons::bs_icon("coin")
        ),

        bslib::navset_card_pill(
          bslib::nav_panel(
            title = "Linear Regression",
            echarts4r::echarts4rOutput(outputId = ns("lime_plot_lm"))
          ),
          bslib::nav_panel(
            title = "XGBoost",
            echarts4r::echarts4rOutput(outputId = ns("lime_plot_xgb"))
          )
        ) |> 
          shiny::tagAppendAttributes(class = "nav-justified")

      )
    )
 
  )
}
    
#' app Server Functions
#'
#' @noRd 
mod_app_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Reactive dataframe from user inputs ----
    df_eval <- shiny::eventReactive(input$generate_prediction, {
      tibble::tibble(
        city = input$city, 
        bathrooms = input$n_bathrooms, 
        finishedSqFt = input$square_footage, 
        yearBuilt = input$year_built, 
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
    pred_value <- shiny::eventReactive(df_eval(), {
      generate_ensemble_pred(
        model1 = lm_modl, 
        model2 = xgb_modl, 
        data = df_eval()
      )
    })

    # Render estimated home sale price as text ----
    output$estimated_sale_price <- shiny::renderText({
      scales::label_currency()(pred_value())
    })

    output$lime_plot_lm <- echarts4r::renderEcharts4r({
      generate_lime_chart(
        model = lm_modl,
        train_data = modl_data_fnl,
        new_data = df_eval()
      )
    })

    output$lime_plot_xgb <- echarts4r::renderEcharts4r({
      generate_lime_chart(
        model = xgb_modl,
        train_data = modl_data_fnl,
        new_data = df_eval()
      )
    })

  })
}
    
## To be copied in the UI
# mod_app_ui("app_1")
    
## To be copied in the server
# mod_app_server("app_1")
