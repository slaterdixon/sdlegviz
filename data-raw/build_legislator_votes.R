library(dplyr)

# How many votes to keep for each legislator on each side
VOTES_PER_SIDE <- 5

# The most contentious bills each legislator voted on, on the winning side and
# on the losing side
# vote_choices: votechoices from every session, with person_id, vote_id, option,
#   chamber ("HOUSE"/"SENATE") and year
# vote_infos: votes from every session
# bills: bills from every session, to name the bill each vote is on
# -> the closest floor votes of each legislator in each session, for each side
#    (majority = voted with the larger share of the chamber, minority = with the
#    smaller), closest first. A bill is listed once per side, by its closest
#    vote. A vote that was tied has no majority, so it isn't listed.
get_legislator_votes <- function(vote_choices, vote_infos, bills, n = VOTES_PER_SIDE) {
  floor_votes <- get_floor_votes(vote_infos) |>
    filter(yes != no) |>
    transmute(
      vote_id, chamber, bill_id,
      motion = motion_text, date = start_date,
      yes, no, result, dissent_share,
      majority_option = if_else(yes > no, "yes", "no")
    )

  bill_names <- bills |>
    select(bill_id, bill = identifier, title = name)

  vote_choices |>
    filter(option %in% c("yes", "no"), !is.na(person_id)) |>
    inner_join(floor_votes, by = c("vote_id", "chamber")) |>
    left_join(bill_names, by = "bill_id") |>
    mutate(
      session_year = as.integer(year),
      side = if_else(option == majority_option, "majority", "minority")
    ) |>
    arrange(desc(dissent_share), desc(yes + no), date, vote_id) |>
    slice_head(n = 1, by = c(session_year, person_id, side, bill_id)) |>
    slice_head(n = n, by = c(session_year, person_id, side)) |>
    mutate(rank = row_number(), .by = c(session_year, person_id, side)) |>
    select(
      session_year, id = person_id, side, rank, vote_id, bill, title,
      motion, date, vote = option, yes, no, result, dissent_share
    ) |>
    arrange(session_year, id, side, rank)
}
