#' Read the bills a legislator voted most closely on in one session
#'
#' @param id The legislator's OpenStates person id.
#' @param year Session year.
#' @param data_path Folder holding the published data.
#'
#' @return A data frame with the closest bills each side of the chamber's vote
#'     they were on: the majority first, then the minority, closest vote first.
#'     Empty if they voted on none.
#' @noRd
read_legislator_votes <- function(id, year, data_path = app_data_path()) {
  votes <- arrow::read_parquet(file.path(data_path, "legislator_votes.parquet"))
  votes <- votes[votes$session_year == as.integer(year) & votes$id == id, ]
  votes[order(votes$side, votes$rank), ]
}

#' Capitalize a bill title and cut it off at `width` characters
#' @noRd
shorten_title <- function(title, width = 110) {
  title <- ifelse(is.na(title), "", title)
  title <- paste0(toupper(substr(title, 1, 1)), substring(title, 2))
  ifelse(
    nchar(title) > width,
    paste0(substr(title, 1, width - 1), "…"),
    title
  )
}
