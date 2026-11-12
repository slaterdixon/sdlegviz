library(futile.logger)
library(tidyverse)

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
  # OpenStates organization ids are the same in every session, so a session
  # exported without its organizations can use another one's
  if (!fs::file_exists(org_type_p)) {
    org_type_p <- sort(fs::dir_ls(
      fs::path_dir(year_data_p),
      recurse = TRUE,
      regexp = "_organizations\\.csv$"
    ))[1]
    if (is.na(org_type_p)) stop("No organizations.csv found next to ", year_data_p)
    flog.info(paste0("Using the organizations in ", org_type_p, " for ", year))
  }
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
                        left_join(fixed_fltr, by = "voter_name", relationship = "many-to-one") |>
                        mutate(voter_id = coalesce(voter_id, person_id)) |>
                        select(-c("person_id"))

  # The presiding officer's tie-breaking vote isn't a member's, so has no person
  selections <- vote_selections |>
    filter(option %in% c("yes", "no"), voter_name != "President Votes") |>
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


# vote_choices: the votechoices of the session. Some sponsorships have a name
#   but no person id, and take the id of the voter with that name.
get_os_sponsorships <- function(year, year_data_p, bills, vote_choices) {
  voter_ids <- vote_choices |>
    filter(!is.na(person_id)) |>
    distinct(name = voter_name, voter_person_id = person_id)
  ambiguous <- voter_ids |> count(name) |> filter(n > 1)
  if (nrow(ambiguous) > 0) {
    stop("Voter names with more than one person id in ", year, ": ", paste(ambiguous$name, collapse = ", "))
  }

  sponsorships_p <- fs::path(
    year_data_p,
    paste0("/SD_", year, "_bill_sponsorships.csv")
  )

  read_csv(sponsorships_p, show_col_types = FALSE) |>
    filter(entity_type == "person") |>
    left_join(voter_ids, by = "name") |>
    mutate(person_id = coalesce(person_id, voter_person_id)) |>
    inner_join(bills, by = "bill_id") |>
    select(person_id, bill_id) |>
    mutate(year = year) |>
    drop_na()
}
