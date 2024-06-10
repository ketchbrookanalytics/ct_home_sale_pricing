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
      id = "app_navbar",
      theme = bslib::bs_theme(
        bootswatch = "sandstone",
        fg = "#000000",
        bg = "#ffffff",
        primary = "#2f70c8",
        secondary = "#2f70c8",
        success = "#0f294d"
      ),
      bslib::nav_item(
        shiny::a(
          shiny::img(
            src = "www/ketchbrook_logo.png",
            width = 150
          ),
          href = "https://www.ketchbrookanalytics.com/", 
          target = "_blank"
        )
      ),
      bslib::nav_panel(
        title = "App",
        mod_app_ui("app_1")
      ),
      bslib::nav_panel(
        title = "About",
        mod_about_ui("about_1")
      ),
      bslib::nav_panel(
        title = "Use Cases",
        mod_use_cases_ui("use_cases_1")
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
      app_title = "ct.home.sale.pricing"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
