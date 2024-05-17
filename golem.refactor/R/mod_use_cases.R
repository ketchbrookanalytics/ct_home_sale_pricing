#' use_cases UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_use_cases_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      col_widths = c(-3, 6, -3),
      bslib::card(
        bslib::card_header("Possible Use Cases"),
        shiny::p("Why might you need an application from Ketchbrook Analytics to help you build user-friendly model interpretation capabilities around your predictive models?"),
        shiny::p("Here are some reasons why your business might benefit from a solution like this:"),
        bslib::accordion(
          open = FALSE,
          bslib::accordion_panel(
            title = "Auditing Your Internal Models",
            shiny::p("This type of application can serve as a tool for your internal audit team to interface with and help them gain an understanding about your model, the variables it uses, the effect each variable has on the model output, and to back-test a particular observation. This can be extremely useful to hand your external auditors as well, allowing them to get up to speed quicker with your model as opposed to strictly reading documentation.")
          ),
          bslib::accordion_panel(
            title = "Presentation to Management & Board of Directors",
            shiny::p("If you have a model in place in your organization, it can be difficult to provide something tangible to non-technical stakeholders who need to gain comfort from an organizational risk perspective prior to relying on that model to impact business decisions and outcomes. Providing them with a user interface where they can see the model score an observation can yield incredible dividends in terms of bridging the gap between the modeling team and the managerial suite.")
          ),
          bslib::accordion_panel(
            title = "Validation within the Modeling Team",
            shiny::p("This tool can be useful as a gut-check for the model developer to ensure that the model they are developing is stable across all variables. Furthermore, the team member validating the model developer's work can use this tool to provide a sensitivity analysis of the model.")
          )
        )
      )
    )



 
  )
}
    
#' use_cases Server Functions
#'
#' @noRd 
mod_use_cases_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_use_cases_ui("use_cases_1")
    
## To be copied in the server
# mod_use_cases_server("use_cases_1")
