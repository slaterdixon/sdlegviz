library(dplyr)

CHAMBER_SIZES <- c(HOUSE = 70, SENATE = 35)
# Committee votes have at most 15 voters, floor votes nearly the whole chamber
FLOOR_VOTE_MIN_SHARE <- 0.85

# The floor votes, with how evenly the chamber split on each
# vote_infos: votes from every session, with chamber ("HOUSE"/"SENATE") and the
#   yes, no, absent and excused counts
get_floor_votes <- function(vote_infos) {
  vote_infos |>
    filter(chamber %in% names(CHAMBER_SIZES)) |>
    mutate(participants = yes + no + absent + excused) |>
    filter(
      participants >= FLOOR_VOTE_MIN_SHARE * CHAMBER_SIZES[chamber],
      yes + no > 0
    ) |>
    mutate(dissent_share = pmin(yes, no) / (yes + no))
}
