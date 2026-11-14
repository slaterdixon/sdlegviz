library(dplyr)

# Unanimous votes would put everyone near 100%, so only votes where at least
# this share of the yes and no votes were on the minority side are counted
CONTESTED_MIN_MINORITY <- 0.1

# Needs get_floor_votes() from build_floor_votes.R
# How often each legislator votes with their party and with its leadership
# vote_choices: votechoices from every session, with person_id, vote_id, option,
#   chamber ("HOUSE"/"SENATE") and year
# vote_infos: votes from every session, to find the contested floor votes
# legislators: from build_sd_legislators, with party and position (leadership)
# -> one row per session and legislator, with the share of contested floor
#    votes (pct_) and the number of them (n_) where they voted with
#    - the majority of their party in the chamber, and
#    - the majority of the leadership of that party
#    Neither counts the legislator's own vote, so a legislator is never
#    compared to a majority they are part of. Votes where the majority is split
#    evenly, or the party has no leadership listed, aren't counted.
get_vote_alignment <- function(vote_choices,
                               vote_infos,
                               legislators,
                               min_minority = CONTESTED_MIN_MINORITY) {
  contested <- get_floor_votes(vote_infos) |>
    filter(dissent_share >= min_minority) |>
    distinct(vote_id, chamber)

  members <- legislators |>
    filter(!is.na(id), !is.na(party)) |>
    transmute(session_year, member_chamber = chamber, person_id = id, party, is_leader = !is.na(position))

  vote_choices |>
    filter(option %in% c("yes", "no"), !is.na(person_id)) |>
    inner_join(contested, by = c("vote_id", "chamber")) |>
    mutate(
      session_year = as.integer(year),
      member_chamber = if_else(chamber == "HOUSE", "lower", "upper"),
      yes = option == "yes"
    ) |>
    inner_join(members, by = c("session_year", "member_chamber", "person_id")) |>
    mutate(
      party_yes = sum(yes), party_no = sum(!yes),
      leaders_yes = sum(yes & is_leader), leaders_no = sum(!yes & is_leader),
      .by = c(vote_id, party)
    ) |>
    mutate(
      others_party_yes = party_yes - yes,
      others_party_no = party_no - !yes,
      others_leaders_yes = leaders_yes - (is_leader & yes),
      others_leaders_no = leaders_no - (is_leader & !yes),
      with_party = case_when(
        others_party_yes > others_party_no ~ yes,
        others_party_yes < others_party_no ~ !yes
      ),
      with_leadership = case_when(
        others_leaders_yes > others_leaders_no ~ yes,
        others_leaders_yes < others_leaders_no ~ !yes
      )
    ) |>
    summarise(
      n_party = sum(!is.na(with_party)),
      pct_party = mean(with_party, na.rm = TRUE),
      n_leadership = sum(!is.na(with_leadership)),
      pct_leadership = mean(with_leadership, na.rm = TRUE),
      .by = c(session_year, person_id)
    ) |>
    rename(id = person_id) |>
    mutate(
      across(c(pct_party, pct_leadership), \(pct) if_else(is.nan(pct), NA_real_, pct)),
      min_minority = min_minority
    ) |>
    arrange(session_year, id)
}
