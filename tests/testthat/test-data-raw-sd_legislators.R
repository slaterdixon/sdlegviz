# The data-raw scripts aren't part of the installed package
build_p <- test_path("../../data-raw/build_sd_legislators.R")
skip_if_not(file.exists(build_p), "data-raw is not available")
for (pkg in c("dplyr", "stringr", "purrr", "readr", "fs", "tibble", "futile.logger")) {
  skip_if_not_installed(pkg)
}
source(build_p, local = TRUE)

# A session of members in the shape the site's API returns: 35 districts, with
# two House members each except 26 and 28, which have two one-member subdistricts
fake_members <- function(year = 2024) {
  house_districts <- c(
    rep(sprintf("%02d", setdiff(1:35, c(26, 28))), each = 2),
    "26A", "26B", "28A", "28B"
  )
  districts <- c(house_districts, sprintf("%02d", 1:35))
  n <- length(districts)
  tibble::tibble(
    PersonId = year * 1000L + seq_len(n),
    MemberType = rep(c("H", "S"), c(length(house_districts), 35)),
    District = districts,
    FirstName = paste0("First", seq_len(n)),
    LastName = paste0("Last", seq_len(n)),
    UniqueName = paste0("Last", seq_len(n)),
    PartyName = "Republican",
    Picture = "full.jpg",
    PictureSmall = "small.webp",
    Position = NA_character_,
    Occupation = NA_character_,
    InactiveDate = NA_character_,
    session_year = year
  )
}

fake_person_id <- function(i) sprintf("ocd-person/%08x-0000-0000-0000-%012x", i, i)

# The votes of every member of fake_members(), under the name the site uses
fake_votes <- function(members) {
  chamber <- if_else(members$MemberType == "H", "HOUSE", "SENATE")
  tibble::tibble(
    person_id = fake_person_id(members$PersonId),
    voter_name = members$UniqueName,
    chamber = chamber,
    year = members$session_year,
    vote_id = paste0("vote-", chamber),
    option = "yes"
  )
}

# One contested floor vote in each chamber, on one bill
fake_vote_infos <- function(year = 2024) {
  tibble::tibble(
    vote_id = c("vote-HOUSE", "vote-SENATE"),
    chamber = c("HOUSE", "SENATE"),
    bill_id = "b1", motion_text = "Do Pass", start_date = as.Date(paste0(year, "-02-01")),
    result = "pass", yes = c(60, 30), no = c(10, 5), absent = 0, excused = 0
  )
}

test_that("clean_sd_members maps chambers and rejects duplicate unique names", {
  members <- clean_sd_members(fake_members())
  expect_equal(count(members, chamber)$n, c(70, 35))
  expect_true(all(members$photo == "small.webp"))

  duplicated_name <- fake_members()
  duplicated_name$UniqueName[2] <- duplicated_name$UniqueName[1]
  expect_error(clean_sd_members(duplicated_name))
})

test_that("get_voter_ids rejects malformed person ids", {
  votes <- fake_votes(fake_members())
  expect_equal(nrow(get_voter_ids(votes)), 105)

  votes$person_id[1] <- paste0("NA,", votes$person_id[1])
  expect_error(get_voter_ids(votes), "malformed")
})

test_that("members are linked to the voter with their unique name", {
  members <- clean_sd_members(fake_members())
  linked <- link_members_to_voters(members, get_voter_ids(fake_votes(fake_members())))
  expect_equal(linked$id, fake_person_id(linked$sd_person_id))
  expect_true(all(linked$match_source == "votes"))
})

test_that("a member who matches two voters is an error", {
  members <- clean_sd_members(fake_members())
  votes <- fake_votes(fake_members())
  extra <- votes[1, ]
  extra$person_id <- fake_person_id(999999)
  expect_error(
    link_members_to_voters(members, get_voter_ids(bind_rows(votes, extra))),
    "more than one voter person_id"
  )
})

test_that("hand-keyed ids replace the votes, and 'none' leaves no id", {
  members <- clean_sd_members(fake_members())
  ids <- fake_person_id(c(1, 2))
  manual <- tibble::tibble(
    session_year = 2024L,
    sd_person_id = members$sd_person_id[1:2],
    openstates_person_id = c(ids[1], NO_PERSON_ID)
  )
  keyed <- apply_manual_ids(mutate(members, id = NA_character_, match_source = NA_character_), manual)

  expect_equal(keyed$id[1:2], c(ids[1], NA))
  expect_equal(keyed$match_source[1:2], c("manual", "manual"))
  expect_true(all(is.na(keyed$match_source[-(1:2)])))

  expect_error(
    apply_manual_ids(mutate(members, id = NA_character_, match_source = NA_character_),
                     mutate(manual, openstates_person_id = "ocd-person/nope")),
    "must be an ocd-person id"
  )
  expect_error(
    apply_manual_ids(mutate(members, id = NA_character_, match_source = NA_character_),
                     mutate(manual, sd_person_id = 1L)),
    "match no member"
  )
})

test_that("a missing id is carried from another session unless the person has several", {
  members <- tibble::tibble(
    session_year = c(2023L, 2024L, 2023L, 2024L, 2025L),
    sd_person_id = c(1L, 1L, 2L, 2L, 2L),
    name = c("One", "One", "Two", "Two", "Two"),
    id = c(fake_person_id(1), NA, fake_person_id(2), fake_person_id(3), NA),
    match_source = c("votes", NA, "votes", "votes", NA)
  )
  carried <- carry_ids_across_sessions(members)

  expect_equal(carried$id[2], fake_person_id(1))
  expect_equal(carried$match_source[2], "other_session")
  # Person 2 has two ids, so 2025 stays unresolved
  expect_true(is.na(carried$id[5]))
  expect_true(is.na(carried$match_source[5]))
})

test_that("members without an id go to review with a suggested id, then pass once keyed", {
  members <- fake_members()
  votes <- fake_votes(members)
  # Member 1 voted under a name the site doesn't use
  votes$voter_name[1] <- paste0(votes$voter_name[1], " (Different)")
  review_p <- withr::local_tempfile(fileext = ".csv")
  manual_p <- withr::local_tempfile(fileext = ".csv")

  expect_error(build_sd_legislators(members, votes, manual_p, review_p), "1 members have no OpenStates")
  review <- readr::read_csv(review_p, show_col_types = FALSE)
  expect_equal(review$sd_person_id, members$PersonId[1])
  expect_equal(review$suggested_id, fake_person_id(members$PersonId[1]))

  readr::write_csv(
    tibble::tibble(
      session_year = 2024, sd_person_id = members$PersonId[1],
      openstates_person_id = review$suggested_id
    ),
    manual_p
  )
  legislators <- build_sd_legislators(members, votes, manual_p, review_p)
  expect_equal(nrow(legislators), 105)
  expect_equal(legislators$match_source[legislators$sd_person_id == members$PersonId[1]], "manual")
  expect_false(anyNA(legislators$id))
})

test_that("validation rejects duplicate ids, voters without a member and bad districts", {
  members <- fake_members()
  votes <- fake_votes(members)
  voter_ids <- get_voter_ids(votes)
  legislators <- clean_sd_members(members) |>
    mutate(id = fake_person_id(sd_person_id), match_source = "votes")
  expect_no_error(validate_sd_legislators(legislators, voter_ids))

  duplicated <- legislators
  duplicated$id[2] <- duplicated$id[1]
  expect_error(validate_sd_legislators(duplicated, voter_ids), "more than one member")

  expect_error(
    validate_sd_legislators(legislators, bind_rows(voter_ids, mutate(voter_ids[1, ], person_id = fake_person_id(999999)))),
    "no member in their session"
  )

  expect_error(validate_sd_legislators(legislators[-1, ], voter_ids[-1, ]), "right number of members")

  # A member who left with no replacement listed is fine...
  one_departed <- legislators
  one_departed$inactive_date[1] <- as.Date("2024-02-01")
  expect_no_error(validate_sd_legislators(one_departed, voter_ids))

  # ...but an extra member is only fine when someone in the district left
  extra <- bind_rows(legislators, mutate(legislators[1, ], sd_person_id = 1L, id = fake_person_id(1)))
  expect_error(validate_sd_legislators(extra, voter_ids), "right number of members")
  extra$inactive_date[1] <- as.Date("2024-02-01")
  expect_no_error(validate_sd_legislators(extra, voter_ids))
})

test_that("process_sd_legislators writes the legislators of every processed session", {
  process_p <- normalizePath(test_path("../../data-raw/process_sd_legislators.R"))
  skip_if_not_installed("arrow")
  skip_if_not_installed("jsonlite")
  # The scripts source their helpers relative to the project root
  withr::local_dir(test_path("../.."))
  source(process_p, local = TRUE)
  # Offline: the one session whose members are cached below
  get_session_id_lookup <- function(years) tibble::tibble(session_id = 1L, session_year = 2024L)

  processed <- withr::local_tempdir()
  raw_sd <- withr::local_tempdir()
  members <- fake_members(2024)
  fs::dir_create(fs::path(processed, 2024))
  arrow::write_parquet(fake_votes(members), fs::path(processed, 2024, "votechoices.parquet"))
  arrow::write_parquet(fake_vote_infos(), fs::path(processed, 2024, "votes.parquet"))
  arrow::write_parquet(
    tibble::tibble(
      bill_id = "b1", identifier = "HB 1", name = "a bill", earliest_action = as.Date("2024-01-10")
    ),
    fs::path(processed, 2024, "bills.parquet")
  )
  # The first two members sponsor the one bill together
  arrow::write_parquet(
    tibble::tibble(person_id = fake_person_id(members$PersonId[1:2]), bill_id = "b1", year = 2024L),
    fs::path(processed, 2024, "sponsorships.parquet")
  )
  # Cached as the site returns it, so nothing is downloaded
  jsonlite::write_json(
    members[setdiff(names(members), "session_year")],
    fs::path(raw_sd, "SD_2024_members.json"),
    na = "null"
  )
  jsonlite::write_json(
    data.frame(BillId = 25083, BillType = "HB", BillNumberOnly = 1),
    fs::path(raw_sd, "SD_2024_bills.json")
  )

  suppressMessages(process_sd_legislators(
    raw_sd, processed, withr::local_tempfile(fileext = ".csv"), withr::local_tempfile(fileext = ".csv")
  ))

  legislators <- arrow::read_parquet(fs::path(processed, "legislators_sd.parquet"))
  expect_equal(nrow(legislators), 105)
  expect_equal(unique(legislators$session_year), 2024)

  # The files built from the votes and legislators
  expect_equal(nrow(arrow::read_parquet(fs::path(processed, "vote_alignment.parquet"))), 105)
  legislator_votes <- arrow::read_parquet(fs::path(processed, "legislator_votes.parquet"))
  expect_equal(nrow(legislator_votes), 105)
  expect_equal(unique(legislator_votes$side), "majority")
  sponsored <- arrow::read_parquet(fs::path(processed, "legislator_sponsorships.parquet"))
  expect_equal(nrow(sponsored), 2)
  expect_equal(unique(sponsored$n_cosponsors), 1)
  # Both link to the bill's page on the site, found from its name
  expect_equal(unique(legislator_votes$bill_url), "https://sdlegislature.gov/Session/Bill/25083")
  expect_equal(unique(sponsored$bill_url), "https://sdlegislature.gov/Session/Bill/25083")
})
