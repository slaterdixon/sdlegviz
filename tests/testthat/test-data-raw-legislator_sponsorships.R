# The data-raw scripts aren't part of the installed package
script_p <- test_path("../../data-raw/build_legislator_sponsorships.R")
skip_if_not(file.exists(script_p), "data-raw is not available")
skip_if_not_installed("dplyr")
source(script_p, local = TRUE)

# b1 has three sponsors, b2 has two, b3 and b4 have one, and b5 is Ann's alone in
# a session of its own
fake_sponsorships <- function() {
  tibble::tibble(
    person_id = c("ann", "bo", "cy", "ann", "bo", "ann", "bo", "ann"),
    bill_id = c("b1", "b1", "b1", "b2", "b2", "b3", "b4", "b5"),
    year = c(rep(2024, 7), 2025)
  )
}

fake_bills <- function() {
  tibble::tibble(
    bill_id = paste0("b", 1:5),
    identifier = paste("HB", 1:5),
    name = paste(c("first", "second", "third", "fourth", "fifth"), "bill"),
    earliest_action = as.Date("2024-01-10") + c(4, 3, 2, 1, 0)
  )
}

test_that("a legislator's bills are listed by how many others are on them", {
  bills <- get_legislator_sponsorships(fake_sponsorships(), fake_bills())
  ann <- bills[bills$id == "ann" & bills$session_year == 2024, ]

  expect_equal(ann$bill, c("HB 1", "HB 2", "HB 3"))
  expect_equal(ann$n_cosponsors, c(2L, 1L, 0L))
  expect_equal(ann$rank, 1:3)
  expect_equal(ann$title[1], "first bill")

  # Each legislator and session is ranked on its own
  bo <- bills[bills$id == "bo", ]
  expect_equal(bo$bill, c("HB 1", "HB 2", "HB 4"))
  expect_equal(bills$bill[bills$session_year == 2025], "HB 5")
  expect_equal(bills$rank[bills$session_year == 2025], 1L)
})

test_that("bills with as many cosponsors are listed by when they were introduced", {
  sponsorships <- tibble::tibble(person_id = "ann", bill_id = c("b3", "b4", "b5"), year = 2024)
  bills <- get_legislator_sponsorships(sponsorships, fake_bills())
  # b5 was introduced first, then b4, then b3
  expect_equal(bills$bill, c("HB 5", "HB 4", "HB 3"))
})

test_that("only the most cosponsored bills are kept, and a repeated row counts once", {
  sponsorships <- tibble::tibble(person_id = "ann", bill_id = paste0("b", 1:5), year = 2024)
  expect_equal(nrow(get_legislator_sponsorships(sponsorships, fake_bills(), n = 2)), 2)

  repeated <- rbind(fake_sponsorships(), fake_sponsorships())
  expect_equal(
    get_legislator_sponsorships(repeated, fake_bills()),
    get_legislator_sponsorships(fake_sponsorships(), fake_bills())
  )
})
