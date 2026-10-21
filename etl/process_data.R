library(dotenv)
library(fs)
library(tidyverse)
library(visNetwork)
library(futile.logger)


# Helper functions
source("etl/get_os_data.R")
source("etl/build_graph_components.R")

# Paths
load_dot_env(file = ".env")
BASE_PATH <- Sys.getenv("BASE_PATH")
DATA_PATH <- fs::path(BASE_PATH, "data")
OPENSTATES_RAW_DATA_PATH <- fs::path(DATA_PATH, "raw", "openstates")
if (!(fs::dir_exists(OPENSTATES_RAW_DATA_PATH))) {
  stop("Could not find raw data at '", OPENSTATES_RAW_DATA_PATH, "'")
}

DEST_DATA_PATH <- fs::path(DATA_PATH, "processed")


# Constants
YEARS <- c(2018, 2026)

process_session <- function(
  raw_data_base_path,
  dest_data_base_path,
  session_year,
  all_legs
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

  sponsorships <- get_os_sponsorships(session_year, session_raw_data_p, bills)
  sponsorships_dest_p <- fs::path(
    session_processed_data_p,
    "sponsorships.parquet"
  )
  arrow::write_parquet(sponsorships, sponsorships_dest_p)
  flog.info(paste0("Wrote ", sponsorships_dest_p))

  ################# Graphs #######################
  for (chamber in c("HOUSE", "SENATE")) {
    backbone <- get_vote_backbone(
      all_legs,
      vote_descriptions,
      vote_choices,
      session_year,
      chamber
    )

    backbone_export_p <- fs::path(
      session_processed_data_p,
      paste0(str_to_lower(chamber), "_backbone.parquet")
    )
    backbone_df <- backbone |> igraph::as_data_frame()
    arrow::write_parquet(backbone_df, backbone_export_p)

    sponsorship_backbone <- get_sponsorship_backbone(
      session_year,
      sponsorships,
      all_legs
    )

    spon_backbone_export_p <- fs::path(
      session_processed_data_p,
      "sponsorship_backbone.parquet"
    )
    spon_backbone_df <- backbone |> igraph::as_data_frame()
    arrow::write_parquet(spon_backbone_df, spon_backbone_export_p)
    flog.info(paste0("Wrote ", spon_backbone_export_p))
    
  }
}

process_all_sessions <- function(raw_data_base_path, dest_data_base_path) {
  # Get session data folders in format 20*
  years <- lapply(
    grep("20", list.dirs(raw_data_base_path, full.name = FALSE), value = TRUE),
    as.numeric
  )

  legislators <- get_os_legislators(raw_data_base_path)
  legislators_proc_p <- fs::path(dest_data_base_path, "legislators.parquet")
  arrow::write_parquet(legislators, legislators_proc_p)
  flog.info(paste0("Wrote", legislators_proc_p))

  for (yr in years) {
    flog.info(paste0("Doing ", yr))
    process_session(
      raw_data_base_path = raw_data_base_path,
      dest_data_base_path = dest_data_base_path,
      session_year = yr,
      all_legs = legislators
    )
  }
}


process_all_sessions(
  raw_data_base_path = OPENSTATES_RAW_DATA_PATH,
  dest_data_base_path = DEST_DATA_PATH
)
