#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  years <- get_session_years()

  bslib::page_navbar(
    title = tags$span(
      tags$img(src = "www/logo.svg", alt = "", class = "navbar-logo"),
      "SD Legviz"
    ),
    window_title = "SD Legviz",
    theme = app_theme(),
    header = golem_add_external_resources(),
    bslib::nav_panel("House", value = "house", mod_network_ui("house", years)),
    bslib::nav_panel("Senate", value = "senate", mod_network_ui("senate", years)),
    bslib::nav_panel("Sponsorships", value = "sponsorship", mod_network_ui("sponsorship", years))
  )
}

#' The Bootstrap theme: plain, with the party colors as the only color
#' @noRd
app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#fafaf9",
    fg = "#1c1917",
    primary = "#1c1917",
    "border-radius" = "6px",
    base_font = bslib::font_collection(
      "system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "sans-serif"
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "www/favicon.ico"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "sdlegviz"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
