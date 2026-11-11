#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  for (backbone in c("house", "senate", "sponsorship")) {
    mod_network_server(backbone, backbone = backbone)
  }
}
