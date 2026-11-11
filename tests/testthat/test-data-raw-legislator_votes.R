# The data-raw scripts aren't part of the installed package
for (script in c("build_floor_votes.R", "build_legislator_votes.R")) {
  script_p <- test_path("../../data-raw", script)
  skip_if_not(file.exists(script_p), "data-raw is not available")
  assign(paste0("script_", sub("\\.R$", "", script)), script_p)
}
skip_if_not_installed("dplyr")
source(script_build_floor_votes, local = TRUE)
source(script_build_legislator_votes, local = TRUE)

# House floor votes: v1 passes 36-34, v2 fails 33-35 (both close), v3 is nearly
# unanimous, v4 is tied and v5 is a committee vote
fake_vote_infos <- function() {
  tibble::tibble(
    vote_id = paste0("v", 1:5),
    chamber = "HOUSE",
    bill_id = c("b1", "b2", "b3", "b4", "b1"),
    motion_text = c("Do Pass", "Do Pass Amended", "Do Pass", "Tabled", "Do Pass"),
    start_date = as.Date("2024-02-01") + 0:4,
    result = c("pass", "fail", "pass", "fail", "pass"),
    yes = c(36, 33, 68, 35, 7), no = c(34, 35, 2, 35, 6),
    absent = 0, excused = c(0, 2, 0, 0, 0)
  )
}

fake_bills <- function() {
  tibble::tibble(
    bill_id = c("b1", "b2", "b3", "b4"),
    identifier = c("HB 1", "HB 2", "HB 3", "HB 4"),
    name = c("first bill", "second bill", "third bill", "fourth bill")
  )
}

# Ann votes yes on v1, v2 and v3; Bo votes no on all of them
fake_choices <- function() {
  tidyr::expand_grid(
    tibble::tibble(person_id = c("ann", "bo"), option = c("yes", "no")),
    vote_id = c("v1", "v2", "v3", "v4", "v5")
  ) |>
    dplyr::mutate(chamber = "HOUSE", year = 2024)
}

test_that("each legislator's floor votes are split by whether they won", {
  votes <- get_legislator_votes(fake_choices(), fake_vote_infos(), fake_bills())
  ann <- votes[votes$id == "ann", ]
  bo <- votes[votes$id == "bo", ]

  # v4 is tied and v5 is a committee vote, so neither is listed
  expect_setequal(ann$vote_id, c("v1", "v2", "v3"))

  # Ann was with the larger side on v1 (36-34) and v3, and the smaller on v2
  expect_equal(ann$side[match(c("v1", "v2", "v3"), ann$vote_id)], c("majority", "minority", "majority"))
  # Bo, voting the other way every time, is on the opposite side
  expect_equal(bo$side[match(c("v1", "v2", "v3"), bo$vote_id)], c("minority", "majority", "minority"))
})

test_that("votes are listed closest first, with the bill named", {
  votes <- get_legislator_votes(fake_choices(), fake_vote_infos(), fake_bills())
  ann_majority <- votes[votes$id == "ann" & votes$side == "majority", ]

  # v1 was closer than v3
  expect_equal(ann_majority$vote_id, c("v1", "v3"))
  expect_equal(ann_majority$rank, 1:2)
  expect_equal(ann_majority$bill, c("HB 1", "HB 3"))
  expect_equal(ann_majority$title, c("first bill", "third bill"))
  expect_equal(ann_majority$motion, c("Do Pass", "Do Pass"))
  expect_equal(ann_majority$dissent_share, c(34 / 70, 2 / 70))
})

test_that("a bill is listed once per side, by its closest vote, and each side is capped", {
  # A second, closer vote on b1, where Ann is also with the larger side
  vote_infos <- dplyr::bind_rows(
    fake_vote_infos(),
    tibble::tibble(
      vote_id = "v6", chamber = "HOUSE", bill_id = "b1", motion_text = "Reconsidered",
      start_date = as.Date("2024-03-01"), result = "pass", yes = 35, no = 34, absent = 1, excused = 0
    )
  )
  choices <- dplyr::bind_rows(fake_choices(), dplyr::mutate(fake_choices()[1:2, ], vote_id = "v6"))
  ann <- get_legislator_votes(choices, vote_infos, fake_bills(), n = 1)
  ann <- ann[ann$id == "ann" & ann$side == "majority", ]

  # v6 (35-34) is closer than v1 (36-34) on the same bill, and n = 1 keeps one
  expect_equal(nrow(ann), 1)
  expect_equal(ann$vote_id, "v6")
  expect_equal(ann$bill, "HB 1")
})
