#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      bslib::card(
        bslib::card_header(shiny::em("Models are only as good as their assumptions and the data they were trained with")),
        shiny::span("Here's some more information on the", shiny::strong("data"), "and", shiny::strong("models"), "used in this app:"),
        bslib::navset_card_pill(
          bslib::nav_panel(
            title = "Data",
            shiny::includeMarkdown(path = app_sys("md_files/data.md"))
          ),
          bslib::nav_panel(
            title = "Models",
            shiny::includeMarkdown(path = app_sys("md_files/models.md"))
          )
        ) |> 
          shiny::tagAppendAttributes(class = "nav-justified")
      ),

      bslib::card(
        bslib::card_header(shiny::em("Prediction Explanation")),
        shiny::includeMarkdown(path = app_sys("md_files/chart.md"))
      )
    )
  )
}
    
#' about Server Functions
#'
#' @noRd 
mod_about_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_about_ui("about_1")
    
## To be copied in the server
# mod_about_server("about_1")
