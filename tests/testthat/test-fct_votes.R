test_that("read_legislator_votes gives a legislator's closest bills, majority first", {
  legislators <- read_backbone("house", 2024, data_path = app_sys("extdata"))$legislators
  votes <- read_legislator_votes(legislators$id[1], 2024, data_path = app_sys("extdata"))

  expect_gt(nrow(votes), 0)
  expect_equal(unique(votes$session_year), 2024)
  expect_equal(unique(votes$id), legislators$id[1])
  expect_setequal(unique(votes$side), c("majority", "minority"))
  # Majority first, and closest first within each side
  expect_equal(votes$side, sort(votes$side))
  for (side in c("majority", "minority")) {
    expect_false(is.unsorted(votes$rank[votes$side == side]))
    expect_false(is.unsorted(rev(votes$dissent_share[votes$side == side])))
  }
})

test_that("a legislator's votes are on the side the tally says", {
  votes <- arrow::read_parquet(file.path(app_sys("extdata"), "legislator_votes.parquet"))

  with_larger_side <- (votes$yes > votes$no & votes$vote == "yes") | (votes$no > votes$yes & votes$vote == "no")
  expect_equal(votes$side == "majority", with_larger_side)
  # Each bill is listed once per legislator and side
  expect_false(anyDuplicated(votes[c("session_year", "id", "side", "bill")]) > 0)
})

test_that("read_legislator_votes is empty for someone who voted on nothing", {
  expect_equal(nrow(read_legislator_votes("ocd-person/none", 2024, data_path = app_sys("extdata"))), 0)
})

test_that("shorten_title capitalizes and truncates", {
  expect_equal(shorten_title("revise rules"), "Revise rules")
  expect_equal(shorten_title(NA_character_), "")
  expect_equal(nchar(shorten_title(strrep("a", 200), width = 50)), 50)
})
