library(dotenv)
library(fs)

# Helper functions
source("get_os_data.R")
source("build_graph_components.R")


# Paths
load_dot_env(file = ".env")
BASE_PATH <- Sys.getenv("BASE_PATH")
DATA_PATH <- fs::path(BASE_PATH, "data")
OPENSTATES_RAW_DATA_PATH <- fs::path(DATA_PATH, "raw", "openstates")
if (!(fs::dir_exists(OPENSTATES_RAW_DATA_PATH))){
  stop("Could not find raw data at '", OPENSTATES_RAW_DATA_PATH, "'")
}

# Constants
YEARS = c(2018, 2026)

process_session <- function(
  raw_data_base_path,
  session_year,
){
  ################# Data #######################
  legislators <- get_os_legislators()


  bills <- get_os_bills()


  votes <- get_os_votes()


  sponsorships <- get_os_sponsorships()


  ################# Graphs #######################
  vote_vertices <- build_vote_vertices()
  vote_edges <- build_vote_edges()

  sponsorship_vertices <- build_sponsorship_vertices()
  sponsorship_edges <- build_sponsorship_edges()



  EDGES_DEST <- 



}



