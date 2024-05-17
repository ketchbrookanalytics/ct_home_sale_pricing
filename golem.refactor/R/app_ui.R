#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = shiny::a(
        "Ketchbrook Analytics",
        href = "https://www.ketchbrookanalytics.com/", 
        target = "_blank"
      ),
      bslib::nav_panel(
        title = "App"
      ),
      bslib::nav_panel(
        title = "Help"
      ),
      bslib::nav_panel(
        title = "About"
      ),
      bslib::nav_panel(
        title = "Use Cases"
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "golem.refactor"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
