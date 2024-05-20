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
    bslib::card(
      bslib::card_header(shiny::em("Models are only as good as their assumptions and the data they were trained with")),
      "Here's some more information on the models and data used in this app:",
      bslib::navset_card_pill(
        bslib::nav_panel(
          title = "Data",
          shiny::includeMarkdown(path = app_sys("md_files/data.md"))
        ),
        bslib::nav_panel(
          title = "Models",
          shiny::includeMarkdown(path = app_sys("md_files/models.md"))
        ),
        bslib::nav_panel(
          title = "Chart",
          shiny::includeMarkdown(path = app_sys("md_files/chart.md"))
        )
      ) |> 
        shiny::tagAppendAttributes(class = "nav-justified")
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
