library(dotenv)
library(fs)
library(tidyverse)
library(visNetwork)

# Helper functions
source("etl/get_os_data.R")
source("etl/build_graph_components.R")

# Paths
load_dot_env(file = ".env")
BASE_PATH <- Sys.getenv("BASE_PATH")
DATA_PATH <- fs::path(BASE_PATH, "data")
OPENSTATES_RAW_DATA_PATH <- fs::path(DATA_PATH, "raw", "openstates")
if (!(fs::dir_exists(OPENSTATES_RAW_DATA_PATH))){
  stop("Could not find raw data at '", OPENSTATES_RAW_DATA_PATH, "'")
}

DEST_DATA_PATH <- fs::path(DATA_PATH, "processed")


# Constants
YEARS = c(2018, 2026)

process_session <- function(raw_data_base_path, 
                            dest_data_base_path,
                            session_year, 
                            all_legs){

  session_raw_data_p <- fs::path(raw_data_base_path, session_year)
  session_processed_data_p <- fs::path(dest_data_base_path, session_year)
  dir.create(file.path(dest_data_base_path, session_year), showWarnings = FALSE)


  ################# Data #######################


  bills <- get_os_bills(session_year, session_raw_data_p)
  bills_dest_p <- fs::path(session_processed_data_p, "bills.parquet")
  arrow::write_parquet(bills, bills_dest_p)

  votes <- get_os_votes(session_year, session_raw_data_p)
  votes_dest_p <- fs::path(session_processed_data_p, "votes.parquet")
  arrow::write_parquet(votes, votes_dest_p)

  vote_choices <- get_os_votechoices(session_year, session_raw_data_p)
  vote_choices_dest_p <- fs::path(session_processed_data_p, "votechoices.parquet")
  arrow::write_parquet(vote_choices, vote_choices_dest_p)

  sponsorships <- get_os_sponsorships(session_year, session_raw_data_p, bills)
  sponsorships_dest_p <- fs::path(session_processed_data_p, "sponsorships.parquet")
  arrow::write_parquet(sponsorships, sponsorships_dest_p)

  ################# Graphs #######################
  vote_edges <- build_vote_edges(raw_vote_choices = vote_choices,
                                  raw_vote_info = votes
                                )
  vote_vertices <- build_vote_vertices(votes_edges = vote_edges,
                                        legislator_info = all_legs
                                      )
  
  backbone <- get_graph_backbone(vertices = vote_vertices,
                                  edges = vote_edges,
                                  legislators = legislators,
                                  session_year = session_year
                      )
  
  backbone_export_p <- fs::path(session_processed_data_p, "backbone.parquet")
  backbone_df <- backbone |>  igraph::as_data_frame() 
  arrow::write_parquet(backbone_df, backbone_export_p)

}

process_all_sessions <- function(raw_data_base_path, dest_data_base_path){

  # Get session data folders in format 20*
  years <- lapply(grep("20", list.dirs(raw_data_base_path, full.name = FALSE), value = TRUE), as.numeric)

  legislators <- get_os_legislators(raw_data_base_path)
  legislators_proc_p <- fs::path(dest_data_base_path, "legislators.parquet")
  arrow::write_parquet(legislators, legislators_proc_p)

  for(yr in years){

      b <- process_session(raw_data_base_path = raw_data_base_path,
                      dest_data_base_path = dest_data_base_path,
                      session_year = yr,
                        all_legs = legislators
                    )

  }
}


process_all_sessions(raw_data_base_path = OPENSTATES_RAW_DATA_PATH,
                    dest_data_base_path = DEST_DATA_PATH
                    )

