library(yaml)
library(futile.logger)


### Nodes
## Legislators
parse_role <- function(role, leg_id){
  tibble(
    id = leg_id,
    start_date = role$start_date,
    end_date = role$end_date,
    type  = role$type,
    jurisdiction = role$jurisdiction,
    district = role$district,
    party = role$party$name
  )
}

read_legislator_yaml <- function(leg_yaml_p){

  yaml_content <- yaml::read_yaml(leg_yaml_p)

  base_leg_info <- tibble(
        id = yaml_content$id,
        part = yaml_content$party$name,
        name = yaml_content$name,
        party = if(length(yaml_content$party) > 0) {yaml_content$party[[1]]$name} else { NULL},
        given_name = yaml_content$given_name,
        family_name = yaml_content$family_name,
        birth_date = yaml_content$birth_date,
        gender = yaml_content$gender,
        email = yaml_content$email,
        image = yaml_content$image
)

  roles <- tibble()
  if (length(yaml_content$roles) > 0){
    roles <-  bind_rows(map(yaml_content$roles, ~parse_role(., leg_id = yaml_content$id)))

     return(base_leg_info |>
        inner_join(roles, by = "id")
     )
  }

  else {
    return(base_leg_info)
  }
}

get_os_legislators <- function(raw_data_path) {
  yaml_dir <- fs::path(raw_data_path, "sd_legislators_all")

  yaml_file_names <- list.files(yaml_dir, pattern = "\\.yml$", full.names = TRUE)

  map(yaml_file_names, read_legislator_yaml) |>
    bind_rows()  |>
    mutate(
      across(ends_with("date"), ymd)
    ) |>
   mutate(
      start_year = year(start_date),
      end_year = year(end_date)
   ) |>
    filter(!(is.na(start_year) & is.na(end_year))) |>
    mutate(
      start_year =  if_else(is.na(start_year), end_year - 1, start_year),
      end_year =  if_else(is.na(end_year), start_year + 1, end_year)
    ) |>
      mutate(session_year = map2(start_year, end_year, ~.x:.y)) |>
  unnest(session_year) |>
  select(-c(start_year, end_year))
}


## Bills
get_os_bills <- function(year, year_data_p) {
  bills_filename <- paste0("/SD_", year, "_bills.csv")
  bills_csv_p <- fs::path(year_data_p, bills_filename)

  bill_actions_filename <- paste0("/SD_", year, "_bill_actions.csv")
  bill_actions_csv_p <- fs::path(year_data_p, bill_actions_filename)
  dates_introduced_df <- readr::read_csv(
    bill_actions_csv_p,
    show_col_types = FALSE
  ) |>
    group_by(bill_id) |>
    summarise(earliest_action = min(date)) |>
    select(bill_id, earliest_action)

  bills_df <- readr::read_csv(bills_csv_p, show_col_types = FALSE) |>
    select(c("id", "identifier", "title", "classification", "subject")) |>
    rename(bill_id = id, name = title) |>
    mutate(year = year) |>
    left_join(dates_introduced_df)

  return(bills_df)
}

### Edges
get_os_votes <- function(year, year_data_p) {
  org_type_p <- fs::path(
    year_data_p,
    paste0("/SD_", year, "_organizations.csv")
  )
  orgs <- read_csv(org_type_p, show_col_types = FALSE) |>
    select(id, name) |>
    mutate(name = str_to_upper(name)) |>
    rename(organization_id = id, chamber = name)

  vote_counts_p <- fs::path(
    year_data_p,
    paste0("/SD_", year, "_vote_counts.csv")
  )
  vote_counts <- read_csv(vote_counts_p, show_col_types = FALSE)

  vote_counts <- read_csv(vote_counts_p, show_col_types = FALSE) |>
    pivot_wider(
      id_cols = vote_event_id,
      values_from = value,
      names_from = option
    )

  votes_p <- fs::path(year_data_p, paste0("/SD_", year, "_votes.csv"))
  read_csv(votes_p, show_col_types = F) |>
    left_join(vote_counts, by = c("id" = "vote_event_id")) |>
    rename(year = session_identifier) |>
    mutate(total_voted = (yes + no + absent + excused)) |>
    # Add chamber col
    left_join(orgs, by = "organization_id") |>
    rename(vote_id = id)
}

get_os_votechoices <- function(
  year,
  year_data_p,
  vote_infos,
  keyed_personids,
  session_year
) {
  chamber_lookup <- vote_infos |>
    select(c(vote_id, chamber))

  fixed_fltr <- read_csv(keyed_personids, show_col_types = FALSE) |>
    filter(year == session_year) |>
    select(voter_name, person_id)

  vote_selections_p <- fs::path(
    year_data_p,
    paste0("/SD_", year, "_vote_people.csv")
  )

  vote_selections <- read_csv(vote_selections_p, show_col_types = FALSE) |>
                        left_join(fixed_fltr, by = "voter_name") |>
                        mutate(voter_id = coalesce(voter_id, person_id)) |>
                        select(-c("person_id"))

  selections <- vote_selections |>
    filter(option %in% c("yes", "no")) |>
    mutate(year = year) |>
    rename(person_id = voter_id, vote_id = vote_event_id) |>
    left_join(chamber_lookup, by = "vote_id")

  missing_personids <- selections |>
    filter(is.na(person_id)) |>
    distinct(person_id, voter_name)

  if (nrow(missing_personids) > 0) {
    to_fix <- fs::path(year_data_p, "voters_to_key.csv")
    write_csv(missing_personids, to_fix)
    flog.info(paste0("Wrote ", to_fix))
    stop(
      "There are ",
      nrow(missing_personids),
      " personids that need added to ",
      keyed_personids
    )
  }

selections


}


get_os_sponsorships <- function(year, year_data_p, bills) {
  sponsorships_p <- fs::path(
    year_data_p,
    paste0("/SD_", year, "_bill_sponsorships.csv")
  )

  read_csv(sponsorships_p, show_col_types = FALSE) |>
    inner_join(bills, by = "bill_id") |>
    filter(entity_type == "person") |>
    select(-c(entity_type, organization_id)) |>
    select(person_id, bill_id) |>
    mutate(year = year) |>
    drop_na()
}
