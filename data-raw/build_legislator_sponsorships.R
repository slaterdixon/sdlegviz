library(dplyr)

# How many bills to keep for each legislator
SPONSORED_BILLS <- 10

# The bills each legislator sponsors that have the most cosponsors
# sponsorships: sponsorships from every session, with person_id, bill_id and
#   year. OpenStates marks every sponsor of a South Dakota bill as primary, so
#   the cosponsors of a legislator's bill are the other legislators on it.
# bills: bills from every session, to name each bill
# -> the bills of each legislator in each session, the most cosponsored first,
#    and ties by when the bill was introduced
get_legislator_sponsorships <- function(sponsorships, bills, n = SPONSORED_BILLS) {
  bill_names <- bills |>
    select(bill_id, bill = identifier, title = name, introduced = earliest_action)

  sponsorships |>
    distinct(year, person_id, bill_id) |>
    mutate(n_cosponsors = n() - 1L, .by = c(year, bill_id)) |>
    inner_join(bill_names, by = "bill_id") |>
    arrange(desc(n_cosponsors), introduced, bill) |>
    slice_head(n = n, by = c(year, person_id)) |>
    mutate(rank = row_number(), .by = c(year, person_id)) |>
    transmute(
      session_year = as.integer(year), id = person_id, rank,
      bill, title, n_cosponsors, introduced
    ) |>
    arrange(session_year, id, rank)
}
