#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  years <- get_session_years()

  tagList(
    golem_add_external_resources(),
    navbarPage(
      title = "SD Legviz",
      network_tab("House", "house", years),
      network_tab("Senate", "senate", years),
      network_tab("Sponsorships", "sponsorship", years)
    )
  )
}

#' One navbar tab wrapping a network module
#'
#' @param title Tab title.
#' @param backbone Backbone shown by the tab, also the module id.
#' @param years Session years to offer in the picker.
#' @noRd
network_tab <- function(title, backbone, years) {
  tabPanel(
    title = title,
    value = backbone,
    mod_network_ui(backbone, years)
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
      app_title = "sdlegviz"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
