test_that("module ui works", {
  ui <- mod_network_ui(id = "test", years = c(2025, 2024))
  golem::expect_shinytaglist(ui)
  fmls <- formals(mod_network_ui)
  for (i in c("id", "years")) {
    expect_true(i %in% names(fmls))
  }
})

test_that("read_backbone returns nodes covering every edge", {
  g <- read_backbone("house", 2025, data_path = app_sys("extdata"))
  expect_true(all(c("id", "label", "group") %in% names(g$nodes)))
  expect_true(all(c(g$edges$from, g$edges$to) %in% g$nodes$id))
  expect_false(anyNA(g$nodes$label))
})

test_that("network server reads the selected year", {
  skip_if_not_installed("visNetwork")
  testServer(mod_network_server, args = list(backbone = "senate"), {
    session$setInputs(select_year = 2024)
    expect_true(nrow(graph()$edges) > 0)
  })
})

test_that("every node of a backbone is a member of that session", {
  for (year in get_session_years(app_sys("extdata"))) {
    for (backbone in c("house", "senate", "sponsorship")) {
      g <- read_backbone(backbone, year, data_path = app_sys("extdata"))
      expect_false(
        any(startsWith(g$nodes$label, "ocd-person/")),
        info = paste(backbone, year)
      )
    }
  }
})

test_that("network server selects a searched legislator", {
  skip_if_not_installed("visNetwork")
  testServer(mod_network_server, args = list(backbone = "senate"), {
    session$setInputs(select_year = 2024)
    id <- graph()$nodes$id[[1]]
    expect_no_error(session$setInputs(legislator = id))
    expect_no_error(session$setInputs(legislator = ""))
  })
})

test_that("party_color gives each party its own color and everyone else gray", {
  colors <- party_color(c("Republican", "Democratic", "Independent", "Unknown"))

  expect_equal(colors[3:4], c("gray", "gray"))
  expect_false(anyDuplicated(colors[1:3]) > 0)
  expect_equal(party_color(c("Republican", "Republican")), rep(colors[1], 2))
})

test_that("read_backbone gives the details of each node's legislator", {
  g <- read_backbone("house", 2024, data_path = app_sys("extdata"))

  expect_equal(g$legislators$id, g$nodes$id)
  expect_equal(g$legislators$name, g$nodes$label)
  expect_true(all(g$legislators$office == "House"))
  expect_false(anyNA(g$legislators$district))
  expect_true("Speaker of the House" %in% g$legislators$position)
  expect_false("title" %in% names(g$nodes))
})

test_that("the legislator card shows what is known and escapes HTML", {
  legislator <- data.frame(
    id = "a", name = "<b>Ann</b>", party = "Democratic", office = "Senate",
    district = "12", position = NA_character_, occupation = "Farmer",
    inactive_date = as.Date(NA), image = NA_character_,
    n_party = NA_real_, pct_party = NA_real_, n_leadership = NA_real_,
    pct_leadership = NA_real_, min_minority = NA_real_
  )
  card <- as.character(legislator_card(legislator))

  expect_match(card, "&lt;b&gt;Ann&lt;/b&gt;", fixed = TRUE)
  expect_no_match(card, "<b>Ann</b>", fixed = TRUE)
  expect_match(card, "<dd>Senate</dd>", fixed = TRUE)
  expect_match(card, "<dd>12</dd>", fixed = TRUE)
  # Nothing is shown for what isn't known
  expect_no_match(card, "Position", fixed = TRUE)
  expect_no_match(card, "Left office", fixed = TRUE)
  expect_no_match(card, "<img", fixed = TRUE)

  legislator$position <- "Majority Whip"
  legislator$inactive_date <- as.Date("2025-05-01")
  legislator$image <- "data:image/webp;base64,AAAA"
  card <- as.character(legislator_card(legislator))
  expect_match(card, "<dd>Majority Whip</dd>", fixed = TRUE)
  expect_match(card, "<dd>2025-05-01</dd>", fixed = TRUE)
  expect_match(card, "data:image/webp;base64,AAAA", fixed = TRUE)
})

test_that("nodes are circular images bordered by party color", {
  g <- read_backbone("house", 2024, data_path = app_sys("extdata"))
  with_image <- !is.na(g$nodes$image)
  expect_true(any(with_image))
  # Photos are in the data, so the browser doesn't request them, except the few
  # too big to send with the graph, which stay as links
  embedded <- startsWith(g$nodes$image[with_image], "data:image/")
  expect_gt(mean(embedded), 0.9)
  expect_true(all(startsWith(g$nodes$image[with_image][!embedded], "http")))
  expect_true(all(g$nodes$shape[with_image] == "circularImage"))
  expect_true(all(g$nodes$shape[!with_image] == "dot"))
  expect_equal(g$nodes$color.border, party_color(g$nodes$group))
})

test_that("chamber tabs only show members of that chamber", {
  # Dean Wink was a representative in 2022 and a senator from 2023
  g <- read_backbone("house", 2022, data_path = app_sys("extdata"))
  wink <- g$legislators[g$legislators$name == "Dean Wink", ]
  expect_equal(wink$office, "House")
  expect_false(is.na(wink$district))

  for (year in get_session_years(app_sys("extdata"))) {
    house <- read_backbone("house", year, data_path = app_sys("extdata"))
    expect_false("Senate" %in% house$legislators$office, info = year)
    senate <- read_backbone("senate", year, data_path = app_sys("extdata"))
    expect_false("House" %in% senate$legislators$office, info = year)
  }
})

test_that("each tab shows its own backbone", {
  skip_if_not_installed("jsonlite")
  year <- 2026
  expected <- lapply(
    c(house = "house", senate = "senate", sponsorship = "sponsorship"),
    function(b) sort(read_backbone(b, year, data_path = app_sys("extdata"))$nodes$id)
  )

  testServer(app_server, {
    session$setInputs(
      `house-select_year` = year,
      `senate-select_year` = year,
      `sponsorship-select_year` = year
    )
    for (tab in names(expected)) {
      widget <- jsonlite::fromJSON(output[[paste0(tab, "-network_viz")]])
      expect_equal(sort(widget$x$nodes$id), expected[[tab]], info = tab)
    }
  })
})

test_that("graphs are laid out by igraph, the same way each time, with no physics", {
  skip_if_not_installed("jsonlite")
  testServer(mod_network_server, args = list(backbone = "house"), {
    session$setInputs(select_year = 2024)
    widget <- jsonlite::fromJSON(output$network_viz)

    expect_false(is.null(widget$x$igraphlayout))
    expect_false(anyNA(c(widget$x$nodes$x, widget$x$nodes$y)))
    expect_match(widget$x$OnceEvents$afterDrawing, "fit()", fixed = TRUE)

    again <- jsonlite::fromJSON(output$network_viz)
    expect_equal(again$x$nodes[c("x", "y")], widget$x$nodes[c("x", "y")])
  })
})

test_that("every session's graphs get a position for each node", {
  for (year in get_session_years(app_sys("extdata"))) {
    for (backbone in c("house", "senate", "sponsorship")) {
      g <- read_backbone(backbone, year, data_path = app_sys("extdata"))
      widget <- network_widget(g)
      expect_equal(nrow(widget$x$nodes), nrow(g$nodes), info = paste(backbone, year))
      expect_false(anyNA(c(widget$x$nodes$x, widget$x$nodes$y)), info = paste(backbone, year))
      expect_lte(max(abs(c(widget$x$nodes$x, widget$x$nodes$y))), 1)
    }
  }
})


test_that("the card shows the legislator picked by a click or by searching", {
  testServer(mod_network_server, args = list(backbone = "house"), {
    session$setInputs(select_year = 2024)
    legislators <- graph()$legislators
    first <- legislators[1, ]
    second <- legislators[2, ]

    expect_match(output$legislator_card$html, "Click a legislator", fixed = TRUE)

    session$setInputs(clicked = first$id)
    expect_equal(selected(), first$id)
    expect_match(output$legislator_card$html, first$name, fixed = TRUE)

    # Searching for someone else replaces the card
    session$setInputs(legislator = second$id)
    expect_equal(selected(), second$id)
    expect_match(output$legislator_card$html, second$name, fixed = TRUE)

    # Clicking the background deselects
    session$setInputs(clicked = NULL)
    expect_null(selected())
    expect_match(output$legislator_card$html, "Click a legislator", fixed = TRUE)
  })
})

test_that("the card is cleared when the year changes", {
  testServer(mod_network_server, args = list(backbone = "house"), {
    session$setInputs(select_year = 2024)
    session$setInputs(clicked = graph()$legislators$id[1])
    expect_false(is.null(selected()))

    session$setInputs(select_year = 2023, legislator = "")
    expect_null(selected())
  })
})

test_that("the widget reports clicks to the given Shiny input", {
  g <- read_backbone("house", 2024, data_path = app_sys("extdata"))

  expect_match(network_widget(g, clicked_input = "house-clicked")$x$events$select, "house-clicked", fixed = TRUE)
  expect_null(network_widget(g)$x$events$select)
})

test_that("only the card, not a popup, is offered for a node", {
  g <- read_backbone("house", 2024, data_path = app_sys("extdata"))
  expect_false("title" %in% names(network_widget(g)$x$nodes))
  expect_match(as.character(mod_network_ui("x", 2024)), "x-legislator_card")
})

test_that("legislators carry how often they vote with their party and leadership", {
  for (backbone in c("house", "senate", "sponsorship")) {
    legislators <- read_backbone(backbone, 2024, data_path = app_sys("extdata"))$legislators
    for (pct in c("pct_party", "pct_leadership")) {
      shares <- legislators[[pct]][!is.na(legislators[[pct]])]
      expect_true(all(shares >= 0 & shares <= 1), info = paste(backbone, pct))
    }
    # A share only exists where there are votes to take it of
    expect_true(all(is.na(legislators$pct_party) == (legislators$n_party %in% c(0, NA))))
    expect_true(all(legislators$min_minority[!is.na(legislators$min_minority)] == 0.1))
  }

  # The 2021 session lists no Democratic leadership to compare them to
  house_2021 <- read_backbone("house", 2021, data_path = app_sys("extdata"))$legislators
  democrats <- house_2021[house_2021$party == "Democratic", ]
  expect_true(all(is.na(democrats$pct_leadership)))
  expect_false(anyNA(democrats$pct_party))
})

test_that("the card shows how often a legislator votes with their party and leadership", {
  legislator <- data.frame(
    id = "a", name = "Ann", party = "Republican", office = "House", district = "12",
    position = NA_character_, occupation = NA_character_, inactive_date = as.Date(NA),
    image = NA_character_, n_party = 126, pct_party = 0.8968, n_leadership = 120,
    pct_leadership = 0.9365, min_minority = 0.1
  )
  card <- as.character(legislator_card(legislator))

  expect_match(card, "Votes with party[^<]*<bslib-tooltip[\\s\\S]*?</strong>\\s*<span>90% of 126 votes</span>", perl = TRUE)
  expect_match(card, "Votes with leadership[^<]*<bslib-tooltip[\\s\\S]*?</strong>\\s*<span>94% of 120 votes</span>", perl = TRUE)
  # Each label has a ? button explaining it, so it opens on tap as well as hover
  expect_equal(lengths(regmatches(card, gregexpr('<button type="button" class="vote-prop-why"', card, fixed = TRUE))), 2)
  # The help names them, by their full name when no last name is known
  expect_match(card, "How often did Ann vote the same as most other Republicans?", fixed = TRUE)
  expect_match(card, "How often did Ann vote the same as most of the Republicans' leaders?", fixed = TRUE)
  # With no one else in the party, the bar has no average to mark
  expect_no_match(card, "Party average", fixed = TRUE)

  peers <- rbind(legislator, transform(legislator, id = "b", pct_party = 0.5, pct_leadership = 0.5))
  card <- as.character(legislator_card(legislator, peers = peers))
  expect_match(card, "Party average 70%", fixed = TRUE)
  expect_match(card, "Party average 72%", fixed = TRUE)
  expect_match(card, "at least 10% of the chamber voted the other way", fixed = TRUE)

  # With no leadership to compare to, only the party share is shown
  legislator$n_leadership <- 0
  legislator$pct_leadership <- NA_real_
  card <- as.character(legislator_card(legislator))
  expect_match(card, "Votes with party", fixed = TRUE)
  expect_no_match(card, "Votes with leadership", fixed = TRUE)

  # Nothing about votes for someone who has none to count
  legislator$n_party <- NA_real_
  legislator$pct_party <- NA_real_
  card <- as.character(legislator_card(legislator))
  expect_no_match(card, "Votes with", fixed = TRUE)
  expect_no_match(card, "voted the other way", fixed = TRUE)
})

test_that("the card lists the bills a legislator voted on most closely, by side", {
  legislator <- data.frame(
    id = "a", name = "Ann Lee", last_name = "Lee", party = "Republican", office = "House", district = "12",
    position = NA_character_, occupation = NA_character_, inactive_date = as.Date(NA),
    image = NA_character_, n_party = NA_real_, pct_party = NA_real_, n_leadership = NA_real_,
    pct_leadership = NA_real_, min_minority = NA_real_
  )
  votes <- data.frame(
    side = c("majority", "minority", "minority"),
    bill = c("HB 1", "HB 2", "SB 3"),
    title = c("raise <b>taxes</b>", "lower taxes", "plant trees"),
    motion = c("Do Pass", "Do Pass Amended", "Tabled"),
    date = as.Date(c("2024-02-01", "2024-02-02", "2024-02-03")),
    vote = c("yes", "no", "yes"),
    yes = c(36, 33, 20), no = c(34, 35, 49), result = c("pass", "fail", "fail")
  )

  card <- as.character(legislator_card(legislator, votes))

  expect_match(card, "Most contentious votes, with majority", fixed = TRUE)
  expect_match(card, "Most contentious votes, against majority", fixed = TRUE)
  expect_match(card, "HB 1", fixed = TRUE)
  expect_match(card, "Do Pass, 2024-02-01. Lee voted yes; 36 yes, 34 no (passed)", fixed = TRUE)
  expect_match(card, "Tabled, 2024-02-03. Lee voted yes; 20 yes, 49 no (failed)", fixed = TRUE)
  # Titles are escaped, and the majority is listed before the minority
  expect_match(card, "Raise &lt;b&gt;taxes&lt;/b&gt;", fixed = TRUE)
  expect_lt(regexpr("with majority", card), regexpr("against majority", card))

  # A side with no votes isn't listed, and nothing is listed without any votes
  only_majority <- as.character(legislator_card(legislator, votes[votes$side == "majority", ]))
  expect_no_match(only_majority, "against majority", fixed = TRUE)
  none <- as.character(legislator_card(legislator))
  expect_no_match(none, "Most contentious", fixed = TRUE)
})

test_that("the card links each bill to its page on the site, when known", {
  legislator <- data.frame(
    id = "a", name = "Ann Lee", last_name = "Lee", party = "Republican", office = "House",
    district = "12", position = NA_character_, occupation = NA_character_,
    inactive_date = as.Date(NA), image = NA_character_, n_party = NA_real_, pct_party = NA_real_,
    n_leadership = NA_real_, pct_leadership = NA_real_, min_minority = NA_real_
  )
  url <- "https://sdlegislature.gov/Session/Bill/25083"
  link <- paste0('<a href="', url, '" target="_blank" rel="noopener">HB 1</a>')
  votes <- data.frame(
    side = c("majority", "minority"), bill = c("HB 1", "HB 2"), title = c("a", "b"),
    motion = "Do Pass", date = as.Date("2024-02-01"), vote = "yes", yes = 36, no = 34,
    result = "pass", bill_url = c(url, NA)
  )
  card <- as.character(legislator_card(legislator, votes))
  expect_match(card, link, fixed = TRUE)
  # A bill whose page isn't known is named without a link
  expect_match(card, "<strong>HB 2</strong>", fixed = TRUE)

  sponsored <- data.frame(
    bill = "HB 1", title = "a", n_cosponsors = 3, introduced = as.Date("2024-01-10"), bill_url = url
  )
  card <- as.character(legislator_card(legislator, sponsored = sponsored, alignment = FALSE))
  expect_match(card, link, fixed = TRUE)
})

test_that("the card loads the picked legislator's votes", {
  testServer(mod_network_server, args = list(backbone = "house"), {
    session$setInputs(select_year = 2024)
    session$setInputs(clicked = graph()$legislators$id[1])

    expect_match(output$legislator_card$html, "Most contentious votes, with majority", fixed = TRUE)
    expect_match(output$legislator_card$html, "Most contentious votes, against majority", fixed = TRUE)
  })
})

test_that("there is no table of contentious votes any more", {
  expect_no_match(as.character(app_ui(NULL)), "contentious_votes", fixed = TRUE)
  expect_false(exists("read_contentious_votes"))
})

test_that("legislators in leadership are drawn larger", {
  g <- read_backbone("house", 2024)
  leader <- !is.na(g$legislators$position[match(g$nodes$id, g$legislators$id)])

  expect_true(any(leader))
  expect_true(all(g$nodes$size[leader] == LEADER_NODE_SIZE))
  expect_true(all(g$nodes$size[!leader] == NODE_SIZE))
})

test_that("a legislator linked to no one is still a node of their chamber", {
  g <- read_backbone("senate", 2024)
  linked <- unique(c(g$edges$from, g$edges$to))
  alone <- g$legislators[!g$legislators$id %in% linked, ]

  # The four Democrats of the 2024 Senate agree with no one beyond chance
  expect_true(all(c("Liz Larson", "Reynold Nesiba") %in% alone$name))
  expect_true(all(alone$office == "Senate"))
  expect_equal(nrow(g$nodes), nrow(g$legislators))
  expect_false(anyDuplicated(g$nodes$id) > 0)
})

test_that("the about file gives each tab its intro and answered questions", {
  path <- tempfile(fileext = ".md")
  writeLines(c(
    "<!-- A note to the author,",
    "## not a question, and never shown -->",
    "",
    "The **intro**, with a [link](https://example.org).",
    "",
    "## Every tab",
    "An answer.",
    "",
    "## Votes only {tabs: house, senate}",
    "Only for votes.",
    "",
    "## Not written yet",
    "<!-- another note -->",
    "",
    "## Sponsorships only  {tabs:  sponsorship }",
    "Only for bills."
  ), path)

  questions <- function(backbone) vapply(read_about(backbone, path)$questions, \(x) x$question, "")
  expect_equal(questions("house"), c("Every tab", "Votes only"))
  expect_equal(questions("senate"), c("Every tab", "Votes only"))
  expect_equal(questions("sponsorship"), c("Every tab", "Sponsorships only"))
  expect_match(read_about("house", path)$intro, "The **intro**", fixed = TRUE)

  panel <- as.character(about_panel("house", path))
  expect_match(panel, "<summary>Every tab</summary>", fixed = TRUE)
  expect_match(panel, '<div class="about-intro">', fixed = TRUE)
  expect_match(panel, "<strong>intro</strong>", fixed = TRUE)
  # The intro is above the questions, and is not inside one
  expect_lt(regexpr("about-intro", panel), regexpr("<details>", panel))
  expect_no_match(panel, "Not written yet", fixed = TRUE)
  expect_no_match(panel, "Only for bills", fixed = TRUE)
  expect_no_match(panel, "tabs:", fixed = TRUE)
  expect_no_match(panel, "note to the author", fixed = TRUE)
  expect_no_match(panel, "never shown", fixed = TRUE)
  # Links leave the app in a new tab
  expect_match(panel, 'target="_blank"', fixed = TRUE)
  expect_match(panel, 'rel="noopener"', fixed = TRUE)
})

test_that("the about section can be only an intro, or nothing", {
  path <- tempfile(fileext = ".md")
  writeLines("Just an intro.", path)
  panel <- as.character(about_panel("house", path))
  expect_match(panel, "Just an intro.", fixed = TRUE)
  expect_no_match(panel, "<details>", fixed = TRUE)

  writeLines(c("## A question", "", "## Another"), path)
  expect_null(about_panel("house", path))
  expect_equal(read_about("house", path), list(intro = NULL, questions = list()))
  expect_null(about_panel("house", tempfile()))

  # The file shipped with the app has questions in it
  expect_true(any(grepl("^## ", readLines(app_sys("app/about.md")))))
})

test_that("the graph key counts the members and names what is drawn", {
  g <- read_backbone("senate", 2024)
  key <- as.character(graph_key(g))
  expect_match(key, paste0(nrow(g$nodes), " members"), fixed = TRUE)
  expect_match(key, paste0(length(unlinked_ids(g)), " with no links"), fixed = TRUE)
  expect_match(key, "Republican", fixed = TRUE)
  expect_match(key, "Leadership position", fixed = TRUE)

  # Nothing is said about people with no links when everyone has one
  g$edges <- rbind(g$edges, data.frame(from = unlinked_ids(g), to = g$edges$to[1], value = 1))
  expect_no_match(as.character(graph_key(g)), "with no links", fixed = TRUE)
})

test_that("the member table lists everyone, by name, with how many links they have", {
  g <- read_backbone("house", 2024)
  table <- as.character(members_table(g, input_id = "house-picked"))

  expect_equal(lengths(regmatches(table, gregexpr('class="member-link"', table))), nrow(g$legislators))
  expect_match(table, 'data-input="house-picked"', fixed = TRUE)
  expect_match(table, "With leadership", fixed = TRUE)
  # Sorted by name to begin with, and Links is the last column
  names <- regmatches(table, gregexpr('class="member-link"[^>]*>[^<]+', table))[[1]]
  names <- sub(".*>", "", names)
  expect_equal(names, g$legislators$name[order(tolower(g$legislators$name))])
  expect_match(table, '<button type="button" data-column="5">Links</button>', fixed = TRUE)

  # The sponsorship graph has no votes to compare
  sponsorship <- as.character(members_table(read_backbone("sponsorship", 2024), votes = FALSE))
  expect_no_match(sponsorship, "With party", fixed = TRUE)
})

test_that("the card says when no one is linked, and leaves out the vote bars where they don't apply", {
  legislator <- data.frame(
    id = "a", name = "Ann", party = "Republican", office = "House", district = "12",
    position = NA_character_, occupation = NA_character_, inactive_date = as.Date(NA),
    image = NA_character_, n_party = 126, pct_party = 0.9, n_leadership = 120,
    pct_leadership = 0.9, min_minority = 0.1
  )
  expect_no_match(as.character(legislator_card(legislator)), "No links", fixed = TRUE)
  expect_match(as.character(legislator_card(legislator, linked = FALSE)), "No links in this graph.", fixed = TRUE)
  expect_match(as.character(legislator_card(legislator)), "Votes with party", fixed = TRUE)
  expect_no_match(as.character(legislator_card(legislator, alignment = FALSE)), "Votes with party", fixed = TRUE)
})

test_that("the page is in English and the graph has a name for assistive technology", {
  html <- as.character(mod_network_ui("house", years = 2024))
  expect_match(html, 'aria-label="Zoom in"', fixed = TRUE)
  expect_match(html, 'aria-label="Reset zoom"', fixed = TRUE)
  expect_match(html, 'role="region"', fixed = TRUE)
  expect_match(html, 'aria-label="Network of legislators', fixed = TRUE)
})

test_that("the sponsorship graph lists a legislator's most cosponsored bills, not their votes", {
  g <- read_backbone("sponsorship", 2024)
  id <- g$legislators$id[1]
  sponsored <- read_legislator_sponsorships(id, 2024)

  expect_gt(nrow(sponsored), 0)
  expect_equal(sponsored$rank, seq_len(nrow(sponsored)))
  expect_false(is.unsorted(rev(sponsored$n_cosponsors)))
  expect_true(all(sponsored$session_year == 2024 & sponsored$id == id))

  card <- as.character(legislator_card(g$legislators[1, ], sponsored = sponsored, alignment = FALSE))
  expect_match(card, "Bills with most cosponsors", fixed = TRUE)
  expect_match(card, sponsored$bill[1], fixed = TRUE)
  expect_match(card, paste(sponsored$n_cosponsors[1], "cosponsors"), fixed = TRUE)
  expect_no_match(card, "Most contentious", fixed = TRUE)

  # One cosponsor reads in the singular, and no bills means no list
  one <- data.frame(bill = "HB 1", title = "a bill", n_cosponsors = 1L, introduced = as.Date("2024-01-10"))
  expect_match(as.character(legislator_sponsorships_list(one)), "1 cosponsor, introduced 2024-01-10", fixed = TRUE)
  expect_null(legislator_sponsorships_list(one[0, ]))
  expect_null(legislator_sponsorships_list(NULL))
})

test_that("the sponsorship tab's card carries bills in place of votes", {
  testServer(mod_network_server, args = list(backbone = "sponsorship"), {
    session$setInputs(select_year = 2024)
    session$setInputs(clicked = graph()$legislators$id[1])

    expect_match(output$legislator_card$html, "Bills with most cosponsors", fixed = TRUE)
    expect_no_match(output$legislator_card$html, "Most contentious", fixed = TRUE)
  })
})
