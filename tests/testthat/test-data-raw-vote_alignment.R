# The data-raw scripts aren't part of the installed package
for (script in c("build_floor_votes.R", "build_vote_alignment.R")) {
  script_p <- test_path("../../data-raw", script)
  skip_if_not(file.exists(script_p), "data-raw is not available")
  assign(paste0("script_", sub("\\.R$", "", script)), script_p)
}
skip_if_not_installed("dplyr")
source(script_build_floor_votes, local = TRUE)
source(script_build_vote_alignment, local = TRUE)

# House vote v1 is a contested floor vote, v2 is nearly unanimous and v3 is a
# committee vote, so only v1 counts
fake_vote_infos <- function() {
  tibble::tibble(
    vote_id = c("v1", "v2", "v3"),
    chamber = "HOUSE",
    yes = c(40, 69, 7), no = c(20, 1, 6),
    absent = c(5, 0, 0), excused = c(5, 0, 0)
  )
}

# Republicans r1 and r2 are the leadership; r1, r3 and r4 vote yes and r2 no.
# Democrats d1 and d2 have no leadership and both vote no.
fake_members <- function() {
  tibble::tibble(
    session_year = 2024L, chamber = "lower",
    id = c("r1", "r2", "r3", "r4", "d1", "d2"),
    party = c(rep("Republican", 4), rep("Democratic", 2)),
    position = c("Speaker of the House", "Majority Leader", NA, NA, NA, NA)
  )
}

fake_choices <- function(votes = c("v1", "v2", "v3")) {
  tidyr::expand_grid(
    vote_id = votes,
    tibble::tibble(
      person_id = c("r1", "r2", "r3", "r4", "d1", "d2"),
      option = c("yes", "no", "yes", "yes", "no", "no")
    )
  ) |>
    dplyr::mutate(chamber = "HOUSE", year = 2024)
}

test_that("get_floor_votes keeps the votes nearly the whole chamber took part in", {
  floor_votes <- get_floor_votes(fake_vote_infos())

  expect_equal(floor_votes$vote_id, c("v1", "v2"))
  expect_equal(floor_votes$dissent_share, c(20 / 60, 1 / 70))
})

test_that("alignment compares each legislator to the others in their party and leadership", {
  alignment <- get_vote_alignment(fake_choices(), fake_vote_infos(), fake_members())
  by_id <- split(alignment, alignment$id)

  # Only v1 is a contested floor vote
  expect_equal(alignment$n_party, rep(1, 6))

  # Without their own vote, the Republicans' majority is yes, except for r1,
  # whose others (r2 no, r3 yes, r4 yes) also say yes
  expect_equal(by_id$r1$pct_party, 1)
  expect_equal(by_id$r2$pct_party, 0)
  expect_equal(by_id$r3$pct_party, 1)
  expect_equal(by_id$r4$pct_party, 1)
  # d1's only other Democrat voted no, like d1
  expect_equal(by_id$d1$pct_party, 1)

  # Each leader is compared to the other leader alone, and they split
  expect_equal(by_id$r1$pct_leadership, 0)
  expect_equal(by_id$r2$pct_leadership, 0)
  # The leaders split evenly, so there's no majority for the rest to vote with
  expect_equal(by_id$r3$n_leadership, 0)
  expect_true(is.na(by_id$r3$pct_leadership))
  # The Democrats have no leadership listed
  expect_equal(by_id$d1$n_leadership, 0)
  expect_true(is.na(by_id$d1$pct_leadership))
})

test_that("alignment is a share of the contested votes, counting each of them", {
  vote_infos <- dplyr::bind_rows(
    fake_vote_infos(),
    tibble::tibble(vote_id = "v4", chamber = "HOUSE", yes = 35, no = 30, absent = 3, excused = 2)
  )
  # r3 votes against the Republicans on v4, where r1 and r2 (the leaders) vote yes
  v4 <- fake_choices("v4") |>
    dplyr::mutate(option = c("yes", "yes", "no", "yes", "no", "no"))

  alignment <- get_vote_alignment(dplyr::bind_rows(fake_choices(), v4), vote_infos, fake_members())
  r3 <- alignment[alignment$id == "r3", ]

  expect_equal(r3$n_party, 2)
  expect_equal(r3$pct_party, 0.5)
  # On v4 the leaders agree on yes, so r3 votes against them
  expect_equal(r3$n_leadership, 1)
  expect_equal(r3$pct_leadership, 0)
  expect_equal(unique(alignment$min_minority), CONTESTED_MIN_MINORITY)
})
