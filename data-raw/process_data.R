# The whole pipeline, from the raw data to what the app ships with:
#   1. process_all_sessions: the OpenStates votes, bills and graphs of each
#      session
#   2. process_sd_legislators: the legislators of each session from the SD
#      legislature site, linked to the OpenStates person ids in the votes. Stops
#      for manual review if a member can't be linked.
#   3. publish_app_data: stage the app's files in inst/extdata. Runs last, so a
#      stop in an earlier step leaves what the app ships with untouched.

library(dotenv)
library(fs)
library(tidyverse)
library(visNetwork)
library(futile.logger)


# Helper functions
source("data-raw/get_os_data.R")
source("data-raw/build_graph_components.R")
source("data-raw/process_sd_legislators.R")
source("data-raw/publish_app_data.R")

# Paths
load_dot_env(file = ".env")
BASE_PATH <- Sys.getenv("BASE_PATH")
DATA_PATH <- fs::path(BASE_PATH, "pipeline-data")
OPENSTATES_RAW_DATA_PATH <- fs::path(DATA_PATH, "raw", "openstates")
if (!(fs::dir_exists(OPENSTATES_RAW_DATA_PATH))) {
  stop("Could not find raw data at '", OPENSTATES_RAW_DATA_PATH, "'")
}

SD_RAW_DATA_PATH <- fs::path(DATA_PATH, "raw", "sdlegislature")
# Hand-keyed OpenStates ids for the SD members the votes can't link
MANUAL_SD_IDS_PATH <- fs::path(DATA_PATH, "handkeyed", "sd_personid_fixed.csv")
SD_REVIEW_PATH <- fs::path(DATA_PATH, "handkeyed", "to_review", "sd_members_unmatched.csv")

DEST_DATA_PATH <- fs::path(DATA_PATH, "processed")
APP_DATA_PATH <- fs::path(BASE_PATH, "inst", "extdata")


# Constants
YEARS <- c(2018, 2026)
REFRESH_SD_DATA <- TRUE # TRUE = download the SD legislature members again

process_session <- function(
  raw_data_base_path,
  dest_data_base_path,
  session_year
) {
  session_raw_data_p <- fs::path(raw_data_base_path, session_year)
  session_processed_data_p <- fs::path(dest_data_base_path, session_year)
  dir.create(file.path(dest_data_base_path, session_year), showWarnings = FALSE)

  ################# Data #######################
  bills <- get_os_bills(session_year, session_raw_data_p)
  bills_dest_p <- fs::path(session_processed_data_p, "bills.parquet")
  arrow::write_parquet(bills, bills_dest_p)
  flog.info(paste0("Wrote ", bills_dest_p))

  vote_descriptions <- get_os_votes(session_year, session_raw_data_p)
  votes_dest_p <- fs::path(session_processed_data_p, "votes.parquet")
  keyed_personids_p <- fs::path(DATA_PATH, "handkeyed", "fixed", "fixed.csv")
  arrow::write_parquet(vote_descriptions, votes_dest_p)
  flog.info(paste0("Wrote ", votes_dest_p))

  vote_choices <- get_os_votechoices(
    session_year,
    session_raw_data_p,
    vote_descriptions,
    keyed_personids_p,
    session_year
  )
  vote_choices_dest_p <- fs::path(
    session_processed_data_p,
    "votechoices.parquet"
  )
  arrow::write_parquet(vote_choices, vote_choices_dest_p)
  flog.info(paste0("Wrote ", vote_choices_dest_p))

  sponsorships <- get_os_sponsorships(session_year, session_raw_data_p, bills, vote_choices)
  sponsorships_dest_p <- fs::path(
    session_processed_data_p,
    "sponsorships.parquet"
  )
  arrow::write_parquet(sponsorships, sponsorships_dest_p)
  flog.info(paste0("Wrote ", sponsorships_dest_p))

  ################# Graphs #######################
  backbones <- list(
    house = get_vote_backbone(vote_descriptions, vote_choices, session_year, "HOUSE"),
    senate = get_vote_backbone(vote_descriptions, vote_choices, session_year, "SENATE"),
    sponsorship = get_sponsorship_backbone(session_year, sponsorships)
  )

  for (backbone_name in names(backbones)) {
    backbone_export_p <- fs::path(
      session_processed_data_p,
      paste0(backbone_name, "_backbone.parquet")
    )
    backbones[[backbone_name]] |>
      igraph::as_data_frame() |>
      arrow::write_parquet(backbone_export_p)
    flog.info(paste0("Wrote ", backbone_export_p))
  }
}

process_all_sessions <- function(raw_data_base_path, dest_data_base_path) {
  # Get session data folders in format 20*
  years <- lapply(
    grep("20", list.dirs(raw_data_base_path, full.name = FALSE), value = TRUE),
    as.numeric
  )

  for (yr in years) {
    flog.info(paste0("Doing ", yr))
    process_session(
      raw_data_base_path = raw_data_base_path,
      dest_data_base_path = dest_data_base_path,
      session_year = yr
    )
  }
}


process_all_sessions(
  raw_data_base_path = OPENSTATES_RAW_DATA_PATH,
  dest_data_base_path = DEST_DATA_PATH
)

process_sd_legislators(
  sd_raw_data_path = SD_RAW_DATA_PATH,
  processed_data_path = DEST_DATA_PATH,
  manual_p = MANUAL_SD_IDS_PATH,
  review_p = SD_REVIEW_PATH,
  refresh = REFRESH_SD_DATA
)

publish_app_data(
  processed_base_path = DEST_DATA_PATH,
  app_data_base_path = APP_DATA_PATH
)
