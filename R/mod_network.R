#' network UI Function
#'
#' @description A shiny Module showing a session year picker, a backbone
#'     network and the legislator picked in it.
#'
#' @param id Module id.
#' @param years Session years to offer in the picker.
#' @param backbone The backbone shown, which the about section describes. The
#'     tabs name their module after it.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_network_ui <- function(id, years, backbone = id) {
  ns <- NS(id)
  tagList(tags$div(
    class = "legviz",
    tags$aside(
      class = "legviz-controls",
      selectInput(
        inputId = ns("select_year"),
        label = "Session year",
        choices = years
      ),
      selectizeInput(
        inputId = ns("legislator"),
        label = "Find a legislator",
        choices = NULL,
        options = list(placeholder = "Search by name")
      )
    ),
    tags$aside(class = "legviz-about", about_panel(backbone)),
    tags$main(
      class = "legviz-graph",
      `data-view` = "graph",
      tags$div(
        class = "graph-toolbar",
        tags$div(
          class = "view-toggle",
          role = "group",
          `aria-label` = "View",
          tags$button(type = "button", `data-view` = "graph", `aria-pressed` = "true", "Graph"),
          tags$button(type = "button", `data-view` = "table", `aria-pressed` = "false", "Table")
        ),
        tags$div(
          class = "graph-zoom",
          role = "group",
          `aria-label` = "Zoom",
          zoom_button("in", "Zoom in", "M12 5v14M5 12h14"),
          zoom_button("out", "Zoom out", "M5 12h14"),
          zoom_button("fit", "Reset zoom", "M3 12a9 9 0 1 0 9-9 9.75 9.75 0 0 0-6.74 2.74L3 8M3 3v5h5")
        )
      ),
      tags$div(
        class = "graph-canvas",
        role = "region",
        `aria-label` = "Network of legislators. The Table view lists the same legislators.",
        visNetwork::visNetworkOutput(ns("network_viz"), height = "100%")
      ),
      tags$div(class = "graph-table", uiOutput(ns("members"))),
      uiOutput(ns("graph_key"))
    ),
    tags$aside(
      class = "legviz-detail",
      `aria-live` = "polite",
      uiOutput(ns("legislator_card"))
    )
  ))
}

#' A button that zooms the graph, drawn as an icon with a name for screen readers
#' @noRd
zoom_button <- function(action, label, path) {
  tags$button(
    type = "button",
    `data-zoom` = action,
    `aria-label` = label,
    title = label,
    tags$svg(
      viewBox = "0 0 24 24",
      `aria-hidden` = "true",
      tags$path(d = path)
    )
  )
}

#' Placeholder shown when a legislator's image fails to load
#' @noRd
broken_image_uri <- function() {
  svg <- paste0(
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'>",
    "<rect width='100' height='100' fill='lightgray'/>",
    "<circle cx='50' cy='38' r='18' fill='white'/>",
    "<ellipse cx='50' cy='90' rx='32' ry='28' fill='white'/></svg>"
  )
  paste0("data:image/svg+xml,", utils::URLencode(svg, reserved = TRUE))
}

#' JavaScript that sets a Shiny input to the node selected, or NULL when none is
#' @noRd
select_event <- function(input_id) {
  if (is.null(input_id)) return(NULL)
  sprintf(
    "function(properties) { Shiny.setInputValue('%s', properties.nodes.length ? properties.nodes[0] : null, {priority: 'event'}); }",
    input_id
  )
}

#' The network widget for one backbone
#'
#' @param g A list of `nodes` and `edges`, from `read_backbone()`.
#' @param clicked_input The id of the Shiny input to set to the node clicked, or
#'     NULL when the widget isn't in an app.
#' @noRd
network_widget <- function(g, clicked_input = NULL) {
  visNetwork::visNetwork(g$nodes, g$edges) |>
    visNetwork::visNodes(
      size = NODE_SIZE,
      borderWidth = 4,
      borderWidthSelected = 6,
      # Zoomed out, the default font is too small to be drawn
      font = list(size = 0.6 * NODE_SIZE),
      # A name is drawn only once it is big enough to read
      scaling = list(label = list(drawThreshold = 9)),
      # The node picked, or under the pointer, gets an ink ring and grows
      chosen = list(node = htmlwidgets::JS(
        "function(values, id, selected, hovering) {
           if (selected || hovering) {
             values.borderColor = '#1c1917';
             values.borderWidth = 6;
             values.size = values.size * 1.3;
           }
         }"
      )),
      brokenImage = broken_image_uri()
    ) |>
    # The layout is computed here rather than by physics in the browser, so
    # nothing moves. Of the igraph layouts, this one overlaps the fewest nodes.
    visNetwork::visIgraphLayout(layout = "layout_with_kk", randomSeed = 42) |>
    # The layout is as tall as it is wide, so the browser turns it to the panel
    visNetwork::visEvents(
      type = "once",
      afterDrawing = "function() { if (window.legviz) window.legviz.arrange(this); this.fit(); }"
    ) |>
    visNetwork::visEvents(
      select = select_event(clicked_input),
      resize = "function() { this.fit(); }"
    ) |>
    visNetwork::visInteraction(hover = TRUE, keyboard = TRUE) |>
    visNetwork::visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE)) |>
    visNetwork::visEdges(color = list(opacity = 0.3))
}

#' network Server Functions
#'
#' @param id Module id.
#' @param backbone One of "house", "senate" or "sponsorship".
#'
#' @noRd
mod_network_server <- function(id, backbone) {
  # Evaluate now: a lazy `backbone` would see a loop variable's last value
  force(backbone)

  moduleServer(id, function(input, output, session) {
    graph <- reactive({
      req(input$select_year)
      read_backbone(backbone, input$select_year)
    })

    # Offer the legislators in the current graph, searchable by name
    observeEvent(graph(), {
      nodes <- graph()$nodes
      nodes <- nodes[order(nodes$label), ]
      updateSelectizeInput(
        session,
        "legislator",
        choices = stats::setNames(nodes$id, paste0(nodes$label, " (", nodes$group, ")")),
        selected = character(0),
        server = TRUE
      )
    })

    # The legislator shown on the card, from a click on the graph or the search
    selected <- reactiveVal(NULL)

    observeEvent(input$legislator, {
      id <- if (nzchar(input$legislator)) input$legislator else NULL
      if (identical(id, selected())) return()

      proxy <- visNetwork::visNetworkProxy(session$ns("network_viz"))
      if (is.null(id)) {
        visNetwork::visUnselectAll(proxy)
      } else {
        visNetwork::visSelectNodes(proxy, id = id)
        visNetwork::visFocus(proxy, id = id, scale = 1.2)
      }
      selected(id)
    })

    observeEvent(input$clicked, {
      selected(input$clicked)
      updateSelectizeInput(session, "legislator", selected = input$clicked %||% character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # A pick in the table goes through the search, which draws it in the graph
    observeEvent(input$picked, {
      updateSelectizeInput(session, "legislator", selected = input$picked)
    })

    output$legislator_card <- renderUI({
      legislators <- graph()$legislators
      legislator <- legislators[match(selected(), legislators$id), ]
      if (is.null(selected()) || is.na(legislator$id)) {
        legislator_card_placeholder()
      } else {
        sponsorship <- backbone == "sponsorship"
        legislator_card(
          legislator,
          votes = if (!sponsorship) read_legislator_votes(selected(), input$select_year),
          peers = legislators,
          linked = !selected() %in% unlinked_ids(graph()),
          alignment = !sponsorship,
          sponsored = if (sponsorship) read_legislator_sponsorships(selected(), input$select_year)
        )
      }
    })

    output$graph_key <- renderUI(graph_key(graph()))

    output$members <- renderUI({
      members_table(graph(), votes = backbone != "sponsorship", input_id = session$ns("picked"))
    })

    output$network_viz <- visNetwork::renderVisNetwork(
      network_widget(graph(), clicked_input = session$ns("clicked"))
    )
  })
}

## To be copied in the UI
# mod_network_ui("house", years)

## To be copied in the server
# mod_network_server("house", backbone = "house")
