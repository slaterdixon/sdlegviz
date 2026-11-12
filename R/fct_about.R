#' The intro and questions in the about file that a tab shows
#'
#' The file is Markdown. Text before the first `## ` heading is the intro, shown
#' open above the questions. Each `## ` heading is then a question and the text
#' under it its answer. A question with no answer yet is left out. A suffix
#' `{tabs: house, senate}` on a heading limits the question to those tabs, and
#' without one it is shown on every tab. Text in `<!-- -->` is for whoever
#' writes the file, and is dropped.
#'
#' @param backbone One of "house", "senate" or "sponsorship".
#' @param path The about file.
#'
#' @return A list with `intro`, Markdown or NULL when there is none, and
#'     `questions`, a list with an element per question, each a list of
#'     `question` and `answer`, where `answer` is Markdown.
#' @noRd
read_about <- function(backbone, path = app_sys("app/about.md")) {
  none <- list(intro = NULL, questions = list())
  if (!file.exists(path)) return(none)

  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  lines <- strsplit(gsub("(?s)<!--.*?-->", "", text, perl = TRUE), "\n", fixed = TRUE)[[1]]
  join <- function(x) trimws(paste(x, collapse = "\n"))

  starts <- grep("^##[[:space:]]", lines)
  ends <- c(starts[-1] - 1, length(lines))
  intro <- join(lines[seq_len(if (length(starts)) starts[1] - 1 else length(lines))])

  tabs_pat <- "[[:space:]]*\\{tabs:([^}]*)\\}[[:space:]]*$"
  questions <- Map(function(start, end) {
    heading <- sub("^##[[:space:]]+", "", lines[start])
    tabs <- regmatches(heading, regexec(tabs_pat, heading))[[1]][2]
    list(
      question = trimws(sub(tabs_pat, "", heading)),
      answer = join(lines[seq_len(end - start) + start]),
      tabs = if (is.na(tabs)) NULL else strsplit(trimws(tabs), "[[:space:],]+")[[1]]
    )
  }, starts, ends)

  list(
    intro = if (nzchar(intro)) intro,
    questions = Filter(function(item) {
      nzchar(item$answer) && (is.null(item$tabs) || backbone %in% item$tabs)
    }, questions)
  )
}

#' The intro and questions answered beside a graph
#'
#' @param backbone One of "house", "senate" or "sponsorship".
#' @param path The about file, see `read_about()`.
#'
#' @return Shiny tags, or NULL when there is nothing to show.
#' @noRd
about_panel <- function(backbone, path = app_sys("app/about.md")) {
  about <- read_about(backbone, path)
  if (is.null(about$intro) && length(about$questions) == 0) return(NULL)

  tags$section(
    class = "about",
    tags$h3("About"),
    if (!is.null(about$intro)) {
      tags$div(class = "about-intro", answer_html(about$intro))
    },
    lapply(about$questions, function(item) {
      tags$details(tags$summary(item$question), answer_html(item$answer))
    })
  )
}

#' An answer's Markdown as HTML, with links that open in a new tab
#'
#' A link takes the reader out of the app.
#' @noRd
answer_html <- function(answer) {
  html <- as.character(shiny::markdown(answer))
  shiny::HTML(gsub("<a href=", '<a target="_blank" rel="noopener" href=', html, fixed = TRUE))
}
