library(dplyr)
library(futile.logger)

# Needs the votechoices from process_all_sessions, to link members to the
# OpenStates person ids in the votes
source("data-raw/get_sdlrc_data.R")
source("data-raw/build_sd_legislators.R")
source("data-raw/build_floor_votes.R")
source("data-raw/build_vote_alignment.R")
source("data-raw/build_legislator_votes.R")

# Builds legislators_sd.parquet in the processed folder from the site's session
# members and the votes, legislator_photos.parquet with their photos as text
# the app draws without requesting them, vote_alignment.parquet with how often
# each votes with their party and its leadership, and legislator_votes.parquet
# with the bills each voted on most closely, with the majority and the minority.
# Stops at members that need an OpenStates id keyed by hand: they are written to
# review_p, to be added to manual_p before re-running.
process_sd_legislators <- function(sd_raw_data_path,
                                   processed_data_path,
                                   manual_p,
                                   review_p,
                                   refresh = FALSE) {
  # The sessions processed by process_all_sessions
  years <- as.integer(fs::path_file(
    fs::dir_ls(processed_data_path, type = "directory", regexp = "20[0-9]{2}$")
  ))

  members <- get_sd_members(years, sd_raw_data_path, refresh = refresh)

  read_sessions <- \(file) {
    purrr::map(years, \(yr) arrow::read_parquet(fs::path(processed_data_path, yr, file))) |>
      bind_rows()
  }
  vote_choices <- read_sessions("votechoices.parquet")

  legislators <- build_sd_legislators(
    members = members,
    vote_choices = vote_choices,
    manual_p = manual_p,
    review_p = review_p
  )

  legislators_sd_p <- fs::path(processed_data_path, "legislators_sd.parquet")
  arrow::write_parquet(legislators, legislators_sd_p)
  flog.info(paste0("Wrote ", legislators_sd_p))

  # Only members with an id are drawn in a graph
  photos <- get_sd_photos(
    legislators$photo[!is.na(legislators$id)],
    fs::path(sd_raw_data_path, "photos")
  )
  photos_p <- fs::path(processed_data_path, "legislator_photos.parquet")
  arrow::write_parquet(photos, photos_p)
  flog.info(paste0("Wrote ", photos_p))

  alignment <- get_vote_alignment(vote_choices, read_sessions("votes.parquet"), legislators)
  alignment_p <- fs::path(processed_data_path, "vote_alignment.parquet")
  arrow::write_parquet(alignment, alignment_p)
  flog.info(paste0("Wrote ", alignment_p))

  legislator_votes <- get_legislator_votes(
    vote_choices,
    read_sessions("votes.parquet"),
    read_sessions("bills.parquet")
  )
  legislator_votes_p <- fs::path(processed_data_path, "legislator_votes.parquet")
  arrow::write_parquet(legislator_votes, legislator_votes_p)
  flog.info(paste0("Wrote ", legislator_votes_p))
}
