library(yaml)

### Nodes
## Legislators
get_yaml_file_names <- function(yaml_dir){
  list.files(yaml_dir, pattern = "\\.yml$", full.names = TRUE)
}


read_legislator_yaml <- function(leg_yaml_p) {
  yaml_content <- yaml::read_yaml(leg_yaml_p)
  
  first_name <- yaml_content$given_name
  last_name <- yaml_content$family_name
  id <- yaml_content$id
  district <- yaml_content$roles
  party <- yaml_content$party[[1]]$name
  
  yaml_row <- tibble(id, last_name, first_name, party, district)
  
}

get_os_legislators <- function(raw_data_path) {

    yaml_dir = fs::path(raw_data_path, "sd_legislators_all")

    yaml_file_names <- get_yaml_file_names(yaml_dir)
  
    map(yaml_file_names, read_legislator_yaml) |> 
        dplyr::bind_rows() |>
        tidyr::unnest_wider(district) |>
      rename(person_id = id)
}

## Bills
get_os_bills <- function(year, year_data_p){


    bills_filename <- paste0("/SD_", year, "_bills.csv")
    bills_csv_p <- fs::path(year_data_p, bills_filename)

    bill_actions_filename <- paste0("/SD_", year, "_bill_actions.csv")
    bill_actions_csv_p <- fs::path(year_data_p, bill_actions_filename)
    dates_introduced_df <- readr::read_csv(bill_actions_csv_p, show_col_types = FALSE) |>
                                group_by(bill_id) |>
                                summarise(earliest_action = min(date)) |>
                                select(bill_id, earliest_action) 


    bills_df <- readr::read_csv(bills_csv_p, show_col_types = FALSE) |>
                    select(c("id", "identifier", "title", "classification", "subject")) |>
                    rename(bill_id = id, name = title)  |>
                    mutate(year = year) |>
                    left_join(dates_introduced_df)

    return(bills_df)

}

### Edges
get_os_votes <- function(year, year_data_p){


  vote_counts_p <- fs::path(year_data_p, paste0("/SD_", year, "_vote_counts.csv"))
  vote_counts <- read_csv(vote_counts_p, show_col_types = F)
  
  house_orgid <- "ocd-organization/2432bd5a-3c3f-4784-b1f5-c6134171162b"
  vote_counts <- read_csv(vote_counts_p, show_col_types = F) |>
    pivot_wider(id_cols = vote_event_id,
                values_from = value,
                names_from = option)
  
  votes_p <- fs::path(year_data_p, paste0("/SD_", year, "_votes.csv"))
  read_csv(votes_p, show_col_types = F) |>
    left_join(vote_counts, by = c("id" = "vote_event_id")) |>
    rename(year = session_identifier) |>
    mutate(total_voted = (yes + no + absent + excused)) |>
    # 2018 organization IDs just give the SD state govt. 2018 votes where total was > sixty are house votes
    mutate(organization_id = if_else(((year == 2018) &
                                        (total_voted > 60)), house_orgid, organization_id))


}

get_os_votechoices <- function(year, year_data_p){
    vote_selections_p <- fs::path(year_data_p, paste0("/SD_", year, "_vote_people.csv"))
      vote_selections <- read_csv(vote_selections_p, show_col_types = FALSE)
      vote_selections |>
        filter(option %in% c("yes", "no")) |>
        mutate(year = year) |>
        rename(person_id = voter_id)
}


get_os_sponsorships <- function(year, year_data_p, bills){

  sponsorships_p <- fs::path(year_data_p, paste0("/SD_", year, "_bill_sponsorships.csv"))

  read_csv(sponsorships_p, show_col_types = FALSE) |>
    inner_join(bills, by = "bill_id") |>
    filter(entity_type == "person") |>
    select(-c(entity_type, organization_id)) |>
    select(person_id, bill_id) |>
    mutate(year = year) |>
    drop_na()


}