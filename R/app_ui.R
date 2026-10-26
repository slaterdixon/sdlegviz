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
      network_tab("Sponsorships", "sponsor", years)
    )
  )
}

#' One tab with a session year picker beside a network plot
#'
#' @param title Tab title.
#' @param prefix Prefix for the input/output ids, e.g. "house" gives
#'     `house_select_year` and `house_network_viz`.
#' @param years Session years to offer in the picker.
#' @noRd
network_tab <- function(title, prefix, years) {
  tabPanel(
    title = title,
    value = prefix,
    fluidRow(
      column(
        width = 3,
        selectInput(
          inputId = paste0(prefix, "_select_year"),
          label = "Select a session year",
          choices = years
        )
      ),
      column(
        width = 9,
        plotOutput(outputId = paste0(prefix, "_network_viz"))
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
      app_title = "sdlegviz"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
