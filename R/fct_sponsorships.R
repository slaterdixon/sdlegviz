#' Read the bills a legislator sponsors that have the most cosponsors
#'
#' @param id The legislator's OpenStates person id.
#' @param year Session year.
#' @param data_path Folder holding the published data.
#'
#' @return A data frame with their bills in one session, the most cosponsored
#'     first. Empty if they sponsored none.
#' @noRd
read_legislator_sponsorships <- function(id, year, data_path = app_data_path()) {
  bills <- arrow::read_parquet(file.path(data_path, "legislator_sponsorships.parquet"))
  bills <- bills[bills$session_year == as.integer(year) & bills$id == id, ]
  bills[order(bills$rank), ]
}
