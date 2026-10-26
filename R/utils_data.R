#' Path to the data bundled with the app
#'
#' Written by `data-raw/publish_app_data.R`.
#'
#' @noRd
app_data_path <- function() {
  path <- app_sys("extdata")
  if (path == "") {
    stop("No bundled data found. Run data-raw/process_data.R to publish it.")
  }
  path
}

#' Session years that have a full set of backbones
#'
#' @return Integer vector of years, sorted descending.
#' @noRd
get_session_years <- function(data_path = app_data_path()) {
  backbones <- c(
    "house_backbone.parquet",
    "senate_backbone.parquet",
    "sponsorship_backbone.parquet"
  )
  year_dirs <- list.files(data_path, pattern = "^20[0-9]{2}$", full.names = TRUE)
  complete <- vapply(
    year_dirs,
    function(d) all(file.exists(file.path(d, backbones))),
    logical(1)
  )
  sort(as.integer(basename(year_dirs[complete])), decreasing = TRUE)
}
