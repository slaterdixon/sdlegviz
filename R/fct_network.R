# A node's radius in pixels
NODE_SIZE <- 25

# Legislators with a leadership position are drawn larger
LEADER_NODE_SIZE <- 40

#' Read one published backbone as visNetwork nodes and edges
#'
#' @param backbone One of "house", "senate" or "sponsorship".
#' @param year Session year.
#' @param data_path Folder holding the published data.
#'
#' @return A list with data frames `nodes` (id, label, group, shape, image, color.border,
#'     color.background), where
#'     `image` is the node picture, as a data URI where the data has the photo so
#'     the browser doesn't request it (NA for a plain dot);
#'     `edges` (from, to, value), where a legislator in the chamber with no edges
#'     is still a node; and
#'     `legislators`, one row per node of what the legislator card shows (id,
#'     name, last_name, party, office, district, position, occupation, inactive_date, image,
#'     and how often they vote with their party and its leadership: n_party,
#'     pct_party, n_leadership, pct_leadership, min_minority).
#' @noRd
read_backbone <- function(backbone, year, data_path = app_data_path()) {
  edges <- arrow::read_parquet(
    file.path(data_path, year, paste0(backbone, "_backbone.parquet"))
  )

  # Each OpenStates id belongs to at most one member in a session
  legislators <- arrow::read_parquet(file.path(data_path, "legislators_sd.parquet"))
  legislators <- legislators[legislators$session_year == as.integer(year) & !is.na(legislators$id), ]

  # A legislator who isn't linked to anyone is not in the backbone's edges, but
  # is still a member of the chamber, so they are drawn alone
  chambers <- switch(backbone, house = "lower", senate = "upper", c("lower", "upper"))
  members <- legislators$id[legislators$chamber %in% chambers]
  ids <- unique(c(edges$from, edges$to, members))
  info <- legislators[match(ids, legislators$id), ]

  alignment <- arrow::read_parquet(file.path(data_path, "vote_alignment.parquet"))
  alignment <- alignment[alignment$session_year == as.integer(year), ]
  alignment <- alignment[match(ids, alignment$id), ]

  party <- ifelse(is.na(info$party), "Unknown", info$party)
  label <- ifelse(is.na(info$name), ids, info$name)

  # The browser draws these as they come, with no request for each photo. A
  # photo that isn't in the data keeps its link.
  photos <- arrow::read_parquet(file.path(data_path, "legislator_photos.parquet"))
  photo_uri <- photos$data_uri[match(info$photo, photos$url)]
  image <- ifelse(is.na(photo_uri), info$photo, photo_uri)
  has_image <- !is.na(image)

  list(
    nodes = data.frame(
      id = ids,
      label = label,
      group = party,
      shape = ifelse(has_image, "circularImage", "dot"),
      image = image,
      size = ifelse(is.na(info$position), NODE_SIZE, LEADER_NODE_SIZE),
      color.border = party_color(party),
      color.background = party_color(party),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = edges$from,
      to = edges$to,
      value = edges$oldweight,
      stringsAsFactors = FALSE
    ),
    legislators = data.frame(
      id = ids,
      name = label,
      last_name = info$last_name,
      party = party,
      office = unname(c(lower = "House", upper = "Senate")[info$chamber]),
      district = info$district,
      position = info$position,
      occupation = info$occupation,
      inactive_date = info$inactive_date,
      image = image,
      n_party = alignment$n_party,
      pct_party = alignment$pct_party,
      n_leadership = alignment$n_leadership,
      pct_leadership = alignment$pct_leadership,
      min_minority = alignment$min_minority,
      stringsAsFactors = FALSE
    )
  )
}

#' Node color for each party; anyone else is gray
#' @noRd
party_color <- function(party) {
  colors <- c(Republican = "#cd2626", Democratic = "#00AEF3")
  unname(ifelse(party %in% names(colors), colors[party], "gray"))
}

#' Card with what is known of one legislator
#'
#' @param legislator One row of the `legislators` from `read_backbone()`.
#' @param votes The closest bills they voted on, from `read_legislator_votes()`.
#' @param peers The `legislators` from `read_backbone()` the legislator is
#'     compared against, normally the whole chamber. NULL shows no average.
#'
#' @param sponsored The bills they sponsor with the most cosponsors, from
#'     `read_legislator_sponsorships()`, for the sponsorship graph, which has them
#'     in place of the votes.
#' @param linked Whether the legislator is linked to anyone in the graph.
#' @param alignment Whether to show how often they vote with their party and its
#'     leadership, which only applies to the vote graphs.
#'
#' @return Shiny tags.
#' @noRd
legislator_card <- function(legislator, votes = NULL, peers = NULL, linked = TRUE, alignment = TRUE, sponsored = NULL) {
  field <- function(name, value) {
    if (is.na(value) || !nzchar(value)) return(NULL)
    tagList(tags$dt(name), tags$dd(value))
  }
  party_peers <- peers[!is.na(peers$party) & peers$party %in% legislator$party, ]
  if (is.null(peers)) party_peers <- data.frame(pct_party = numeric(), pct_leadership = numeric())
  has_votes <- alignment && (
    (!is.na(legislator$n_party) && legislator$n_party > 0) ||
      (!is.na(legislator$n_leadership) && legislator$n_leadership > 0)
  )
  # Who the card is about in its sentences, by last name where it is known
  who <- legislator$last_name
  if (is.null(who) || is.na(who) || !nzchar(who)) who <- legislator$name
  members <- c(Republican = "Republicans", Democratic = "Democrats")[legislator$party]
  party_help <- if (is.na(members)) "most of their party" else paste("most other", members)
  leadership_help <- if (is.na(members)) "most of their party's leaders" else paste0("most of the ", members, "' leaders")

  tags$article(
    class = "legislator-card",
    tags$header(
      if (!is.na(legislator$image)) {
        tags$img(
          src = legislator$image,
          alt = "",
          class = "legislator-photo",
          onerror = "this.style.display='none'"
        )
      },
      tags$div(
        tags$h2(legislator$name),
        tags$p(
          class = "legislator-party",
          style = paste0("--party: ", party_color(legislator$party), ";"),
          legislator$party
        )
      )
    ),
    if (!linked) tags$p(class = "note", "No links in this graph."),
    tags$dl(
      field("Office", legislator$office),
      field("District", legislator$district),
      field("Position", legislator$position),
      field("Occupation", legislator$occupation),
      field("Left office", as.character(legislator$inactive_date))
    ),
    if (has_votes) {
      tags$section(
        tags$h3("Compared with their party"),
        comparison_bar(
          "Votes with party",
          legislator$pct_party, legislator$n_party, party_peers$pct_party, legislator$party,
          help = paste0("How often did ", who, " vote the same as ", party_help, "?")
        ),
        comparison_bar(
          "Votes with leadership",
          legislator$pct_leadership, legislator$n_leadership, party_peers$pct_leadership, legislator$party,
          help = paste0("How often did ", who, " vote the same as ", leadership_help, "?")
        ),
        tags$p(
          class = "note",
          paste0(
            "Of the floor votes where at least ", round(legislator$min_minority * 100),
            "% of the chamber voted the other way, and not counting their own vote."
          )
        )
      )
    },
    legislator_votes_list(votes, "majority", "Most contentious votes, with majority", who),
    legislator_votes_list(votes, "minority", "Most contentious votes, against majority", who),
    legislator_sponsorships_list(sponsored)
  )
}

#' A bar for a legislator's share of votes, with a tick at their party's average
#'
#' @param label What the share measures.
#' @param pct,n The legislator's share (0 to 1) and the votes it is out of.
#' @param peer_pct Everyone in the party's share, averaged for the tick.
#' @param party Their party, which colors the bar.
#' @param help Optional explanation of the label, shown in a tooltip on a `?`
#'   button so it opens on tap and focus as well as hover.
#'
#' @return Shiny tags, or NULL when they have no votes to count.
#' @noRd
comparison_bar <- function(label, pct, n, peer_pct, party, help = NULL) {
  if (is.na(pct) || is.na(n) || n == 0) return(NULL)
  avg <- mean(peer_pct, na.rm = TRUE)
  pos <- function(x) paste0(round(x * 100, 1), "%")
  tags$div(
    class = "vote-prop",
    style = paste0("--party: ", party_color(party), ";"),
    tags$div(
      class = "vote-prop-label",
      tags$strong(
        label,
        if (!is.null(help)) {
          bslib::tooltip(
            tags$button(
              type = "button",
              class = "vote-prop-why",
              `aria-label` = paste0("About \"", tolower(label), "\""),
              "?"
            ),
            help
          )
        }
      ),
      tags$span(paste0(round(pct * 100), "% of ", n, " votes"))
    ),
    tags$div(
      class = "vote-prop-track",
      tags$div(
        class = "vote-prop-fill",
        title = paste0(round(pct * 100), "%"),
        style = paste0("width: ", pos(pct), ";")
      ),
      if (!is.nan(avg)) {
        tags$div(
          class = "vote-prop-average",
          title = paste0("Party average: ", round(avg * 100), "%"),
          style = paste0("left: ", pos(avg), ";")
        )
      }
    ),
    if (!is.nan(avg)) {
      tags$div(class = "vote-prop-caption", paste0("Party average ", round(avg * 100), "%"))
    }
  )
}

#' The bills a legislator voted on most closely, for one side of the chamber
#'
#' @param votes From `read_legislator_votes()`, or NULL.
#' @param side "majority" or "minority".
#' @param heading What to call the list.
#' @param who Who voted, named in each vote's description.
#'
#' @return Shiny tags, or NULL when there are no such votes.
#' @noRd
legislator_votes_list <- function(votes, side, heading, who) {
  if (is.null(votes)) return(NULL)
  votes <- votes[votes$side == side, ]
  if (nrow(votes) == 0) return(NULL)

  tags$section(
    tags$h3(heading),
    tags$ul(
      class = "vote-list",
      lapply(seq_len(nrow(votes)), function(i) {
        vote <- votes[i, ]
        tags$li(
          tags$strong(bill_link(vote$bill, vote$bill_url)), " ", shorten_title(vote$title),
          tags$br(),
          tags$span(
            paste0(
              vote$motion, ", ", format(vote$date), ". ", who, " voted ", vote$vote, "; ",
              vote$yes, " yes, ", vote$no, " no (", vote$result, "ed)"
            )
          )
        )
      })
    )
  )
}

#' The bills a legislator sponsors that have the most cosponsors
#'
#' @param bills From `read_legislator_sponsorships()`, or NULL.
#'
#' @return Shiny tags, or NULL when there are no bills.
#' @noRd
legislator_sponsorships_list <- function(bills) {
  if (is.null(bills) || nrow(bills) == 0) return(NULL)

  tags$section(
    tags$h3("Bills with most cosponsors"),
    tags$ul(
      class = "vote-list",
      lapply(seq_len(nrow(bills)), function(i) {
        bill <- bills[i, ]
        tags$li(
          tags$strong(bill_link(bill$bill, bill$bill_url)), " ", shorten_title(bill$title),
          tags$br(),
          tags$span(paste0(
            bill$n_cosponsors, if (bill$n_cosponsors == 1) " cosponsor" else " cosponsors",
            if (!is.na(bill$introduced)) paste0(", introduced ", format(bill$introduced))
          ))
        )
      })
    )
  )
}

#' A bill's name, linked to its page on the site when the page is known
#'
#' @param bill Its name, e.g. "HB 1045".
#' @param url Its page on sdlegislature.gov, NA or NULL when unknown.
#'
#' @return Shiny tags, or the name alone.
#' @noRd
bill_link <- function(bill, url) {
  if (is.null(url) || is.na(url)) return(bill)
  tags$a(href = url, target = "_blank", rel = "noopener", bill)
}

#' What the card says before a legislator is picked
#' @noRd
legislator_card_placeholder <- function() {
  tags$p(
    class = "note",
    "Click a legislator in the graph, or search for one, to see their details."
  )
}

#' The legislators of a graph that no line reaches
#' @noRd
unlinked_ids <- function(g) {
  setdiff(g$nodes$id, c(g$edges$from, g$edges$to))
}

#' What a graph's colors and sizes stand for, and how many are in it
#'
#' @param g From `read_backbone()`.
#'
#' @return Shiny tags.
#' @noRd
graph_key <- function(g) {
  parties <- unique(g$nodes$group)
  parties <- parties[order(match(parties, c("Republican", "Democratic")))]
  has_leaders <- any(g$nodes$size == LEADER_NODE_SIZE)

  swatch <- function(label, color, large = FALSE) {
    tags$li(
      tags$span(
        class = paste("key-dot", if (large) "key-dot-large"),
        style = paste0("--c: ", color, ";"),
        `aria-hidden` = "true"
      ),
      label
    )
  }

  tags$div(
    class = "graph-key",
    tags$p(
      class = "graph-status",
      paste0(nrow(g$nodes), " members"),
      if (length(unlinked_ids(g)) > 0) paste0(" \u00b7 ", length(unlinked_ids(g)), " with no links")
    ),
    tags$ul(
      lapply(parties, function(party) swatch(party, party_color(party))),
      if (has_leaders) swatch("Leadership position", "#78716c", large = TRUE)
    )
  )
}

#' Every legislator of a graph as a table to sort and pick from
#'
#' @param g From `read_backbone()`.
#' @param votes Whether to include how often they vote with their party and its
#'     leadership, which only applies to the vote graphs.
#' @param input_id The Shiny input to set to the legislator picked.
#'
#' @return Shiny tags.
#' @noRd
members_table <- function(g, votes = TRUE, input_id = NULL) {
  l <- g$legislators
  links <- table(factor(c(g$edges$from, g$edges$to), levels = l$id))
  l <- l[order(tolower(l$name)), ]
  links <- links[l$id]
  percent <- function(x) ifelse(is.na(x), "", paste0(round(x * 100), "%"))

  columns <- list(
    list(label = "Name", sort = tolower(l$name), text = NULL),
    list(label = "Party", sort = l$party, text = l$party),
    list(label = "District", sort = suppressWarnings(as.numeric(gsub("[^0-9]", "", l$district))), text = l$district),
    if (votes) list(label = "With party", sort = ifelse(is.na(l$pct_party), -1, l$pct_party), text = percent(l$pct_party)),
    if (votes) list(label = "With leadership", sort = ifelse(is.na(l$pct_leadership), -1, l$pct_leadership), text = percent(l$pct_leadership)),
    list(label = "Links", sort = as.integer(links), text = as.integer(links))
  )
  columns <- Filter(Negate(is.null), columns)

  tags$table(
    class = "member-table",
    `data-input` = input_id,
    tags$thead(tags$tr(lapply(seq_along(columns), function(i) {
      tags$th(
        scope = "col",
        `aria-sort` = if (i == 1) "ascending" else "none",
        tags$button(type = "button", `data-column` = i - 1, columns[[i]]$label)
      )
    }))),
    tags$tbody(lapply(seq_len(nrow(l)), function(i) {
      tags$tr(
        tags$th(
          scope = "row",
          `data-sort` = columns[[1]]$sort[i],
          tags$button(type = "button", class = "member-link", `data-id` = l$id[i], l$name[i])
        ),
        lapply(columns[-1], function(col) {
          tags$td(`data-sort` = col$sort[i], col$text[i])
        })
      )
    }))
  )
}
