library(dplyr)
library(stringr)
library(futile.logger)

N_DISTRICTS <- 35
# House seats per district; the subdistricts of 26 and 28 (26A, 26B, ...) have one each
DISTRICT_SEATS <- c(lower = 2, upper = 1)
SUBDISTRICT_SEATS <- 1
OS_PERSON_ID_PAT <- "^ocd-person/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
# Marks a member who has no OpenStates person id, in the hand-keyed file
NO_PERSON_ID <- "none"

### Members
# members: a df from get_sd_members
# -> one row per member per session, with chamber as "lower" or "upper"
clean_sd_members <- function(members) {
  members <- members |>
    transmute(
      session_year,
      sd_person_id = PersonId,
      chamber = case_when(MemberType == "H" ~ "lower", MemberType == "S" ~ "upper"),
      district = District,
      name = paste(FirstName, LastName),
      first_name = FirstName,
      last_name = LastName,
      unique_name = UniqueName,
      party = PartyName,
      # The thumbnail: the full-size photos are several MB each
      photo = PictureSmall,
      position = Position,
      occupation = Occupation,
      inactive_date = as.Date(str_sub(InactiveDate, 1, 10))
    )

  stopifnot(!anyNA(members$chamber))
  stopifnot(!anyDuplicated(members[c("session_year", "sd_person_id", "chamber")]))
  # The vote files name a legislator by unique_name, so it has to be unique
  stopifnot(!anyDuplicated(members[c("session_year", "chamber", "unique_name")]))

  members
}

### Voters
# vote_choices: votechoices from every session, with person_id, voter_name,
#   chamber ("HOUSE"/"SENATE") and year columns
# -> the distinct voters per session and chamber. Stops if a person_id isn't a
#    well-formed OpenStates id, e.g. from a bad row in the hand-keyed voter ids.
get_voter_ids <- function(vote_choices) {
  voter_ids <- vote_choices |>
    filter(chamber %in% c("HOUSE", "SENATE"), !is.na(person_id)) |>
    transmute(
      session_year = as.integer(year),
      chamber = if_else(chamber == "HOUSE", "lower", "upper"),
      voter_name,
      person_id
    ) |>
    distinct()

  bad_ids <- voter_ids |> filter(!str_detect(person_id, OS_PERSON_ID_PAT))
  if (nrow(bad_ids) > 0) {
    stop(
      nrow(bad_ids), " voter person_ids are malformed, e.g. '", bad_ids$person_id[1],
      "' (", bad_ids$voter_name[1], ", ", bad_ids$session_year[1],
      "). Fix the hand-keyed voter ids and re-run get_os_votechoices."
    )
  }

  voter_ids
}

### Linking members to OpenStates person ids
# Voters are named in the vote files by the site's unique_name, and each vote
# carries the OpenStates person_id, so this links a member without any other
# OpenStates data.
# -> members with id and match_source = "votes" where a voter matched. Stops if
#    a member matches more than one person_id.
link_members_to_voters <- function(members, voter_ids) {
  matches <- members |>
    inner_join(
      voter_ids,
      by = c("session_year", "chamber", "unique_name" = "voter_name"),
      relationship = "many-to-many"
    ) |>
    distinct(session_year, chamber, sd_person_id, name, id = person_id)

  conflicts <- matches |>
    count(session_year, chamber, sd_person_id, name) |>
    filter(n > 1)
  if (nrow(conflicts) > 0) {
    stop(
      nrow(conflicts), " members match more than one voter person_id: ",
      paste0(conflicts$name, " (", conflicts$session_year, ")", collapse = ", ")
    )
  }

  members |>
    left_join(
      matches |> mutate(match_source = "votes"),
      by = c("session_year", "chamber", "sd_person_id", "name")
    )
}

# manual: the hand-keyed ids, with session_year, sd_person_id and
#   openstates_person_id, which is a person id or "none".
# -> members where the hand-keyed id replaces whatever the votes gave and
#    match_source is "manual"; "none" leaves id empty.
apply_manual_ids <- function(members, manual) {
  bad_ids <- manual |>
    filter(openstates_person_id != NO_PERSON_ID, !str_detect(openstates_person_id, OS_PERSON_ID_PAT))
  if (nrow(bad_ids) > 0) {
    stop(
      "The hand-keyed openstates_person_id must be an ocd-person id or '", NO_PERSON_ID,
      "': ", paste(bad_ids$openstates_person_id, collapse = ", ")
    )
  }

  unknown <- manual |>
    anti_join(members, by = c("session_year", "sd_person_id"))
  if (nrow(unknown) > 0) {
    stop(
      nrow(unknown), " hand-keyed rows match no member: ",
      paste0(unknown$sd_person_id, " (", unknown$session_year, ")", collapse = ", ")
    )
  }

  members |>
    left_join(
      manual |> select(session_year, sd_person_id, manual_id = openstates_person_id),
      by = c("session_year", "sd_person_id")
    ) |>
    mutate(
      id = if_else(!is.na(manual_id), na_if(manual_id, NO_PERSON_ID), id),
      match_source = if_else(!is.na(manual_id), "manual", match_source)
    ) |>
    select(-manual_id)
}

# The site lists someone who changes chambers within a session, e.g. by
# appointment, as a member of both. Their votes are under the chamber they were
# in, so the other row, with no voter to link to, is dropped.
drop_chamber_switches <- function(members) {
  switched <- members |>
    mutate(has_other_id = any(!is.na(id)), .by = c(session_year, sd_person_id)) |>
    filter(is.na(id), has_other_id)
  if (nrow(switched) > 0) {
    flog.info(paste0(
      "Dropping ", nrow(switched), " members listed in a chamber they didn't vote in: ",
      paste0(switched$name, " (", switched$session_year, " ", switched$chamber, ")", collapse = ", ")
    ))
  }

  members |>
    anti_join(switched, by = c("session_year", "chamber", "sd_person_id"))
}

# A member with no id in one session takes the id they have in another. This
# skips anyone OpenStates gave more than one id over the sessions (they are
# logged), since which one applies to a session can't be told from the others.
carry_ids_across_sessions <- function(members) {
  known <- members |>
    filter(!is.na(id)) |>
    distinct(sd_person_id, carried_id = id)

  split_ids <- known |> count(sd_person_id) |> filter(n > 1)
  if (nrow(split_ids) > 0) {
    flog.info(paste0(
      nrow(split_ids), " people have more than one OpenStates id across sessions: ",
      paste(
        members |> filter(sd_person_id %in% split_ids$sd_person_id) |> distinct(name) |> pull(name),
        collapse = ", "
      )
    ))
  }

  members |>
    left_join(known |> filter(!sd_person_id %in% split_ids$sd_person_id), by = "sd_person_id") |>
    mutate(
      match_source = if_else(is.na(match_source) & !is.na(carried_id), "other_session", match_source),
      id = coalesce(id, carried_id)
    ) |>
    select(-carried_id)
}

# The voters in a session and chamber who matched no member, to pair with a
# member that has no id. suggested_id is set when exactly one of them has the
# member's last name in the name it voted under.
review_candidates <- function(free_voters, year, chamber_type, last_name) {
  candidates <- free_voters |>
    filter(session_year == year, chamber == chamber_type)
  same_last_name <- candidates |>
    filter(str_detect(str_to_lower(voter_name), fixed(str_to_lower(last_name))))

  tibble::tibble(
    candidate_voter_names = paste(candidates$voter_name, collapse = "; "),
    candidate_ids = paste(candidates$person_id, collapse = "; "),
    suggested_id = if (nrow(same_last_name) == 1) same_last_name$person_id else NA_character_
  )
}

# Writes the members with no id yet to a file for review and stops. The review
# fills openstates_person_id in the hand-keyed file, with an id or "none".
write_review_file <- function(unmatched, members, voter_ids, review_p) {
  free_voters <- voter_ids |>
    anti_join(
      members |> filter(!is.na(id)) |> select(session_year, chamber, person_id = id),
      by = c("session_year", "chamber", "person_id")
    )

  review <- unmatched |>
    select(session_year, chamber, sd_person_id, name, unique_name, district, party, last_name) |>
    bind_cols(purrr::pmap(
      list(unmatched$session_year, unmatched$chamber, unmatched$last_name),
      \(year, chamber_type, last_name) review_candidates(free_voters, year, chamber_type, last_name)
    ) |> bind_rows()) |>
    select(-last_name) |>
    mutate(openstates_person_id = NA_character_, note = NA_character_)

  fs::dir_create(fs::path_dir(review_p))
  readr::write_csv(review, review_p)
  flog.info(paste0("Wrote ", review_p))
  stop(
    nrow(review), " members have no OpenStates person id. Copy the rows you resolve from ",
    review_p, " into the hand-keyed file, filling openstates_person_id with an ocd-person ",
    "id, or '", NO_PERSON_ID, "' if they have none, then re-run."
  )
}

### Checks
# legislators: the finished table
# voter_ids: from get_voter_ids
# Stops if an id is used for two members in a session, a voter has no member in
# their session and chamber, or a district has the wrong number of members.
validate_sd_legislators <- function(legislators, voter_ids) {
  dup_ids <- legislators |>
    filter(!is.na(id)) |>
    count(session_year, id) |>
    filter(n > 1)
  if (nrow(dup_ids) > 0) {
    stop(nrow(dup_ids), " person_ids belong to more than one member in a session: ", paste(dup_ids$id, collapse = ", "))
  }

  voters_without_member <- voter_ids |>
    anti_join(legislators, by = c("session_year", "chamber", "person_id" = "id"))
  if (nrow(voters_without_member) > 0) {
    stop(
      nrow(voters_without_member), " voters have no member in their session and chamber: ",
      paste0(voters_without_member$voter_name, " (", voters_without_member$session_year, " ",
             voters_without_member$chamber, ")", collapse = ", ")
    )
  }

  districts <- legislators |>
    summarise(
      members = n(),
      departed = sum(!is.na(inactive_date)),
      .by = c(session_year, chamber, district)
    ) |>
    mutate(seats = if_else(str_detect(district, "[A-Z]$"), SUBDISTRICT_SEATS, DISTRICT_SEATS[chamber]))
  # A district has its seats filled, and has more members than seats only
  # because someone left
  bad_districts <- districts |>
    filter(members < seats | members - departed > seats)
  if (nrow(bad_districts) > 0) {
    stop(
      nrow(bad_districts), " districts don't have the right number of members: ",
      paste0(bad_districts$session_year, " ", bad_districts$chamber, " district ",
             bad_districts$district, " (", bad_districts$members, " for ", bad_districts$seats, " seats)",
             collapse = ", ")
    )
  }

  n_districts <- districts |>
    distinct(session_year, chamber, district = str_remove(district, "[A-Z]$")) |>
    count(session_year, chamber) |>
    filter(n != N_DISTRICTS)
  if (nrow(n_districts) > 0) {
    stop(
      "Expected ", N_DISTRICTS, " districts in: ",
      paste0(n_districts$session_year, " ", n_districts$chamber, " (", n_districts$n, ")", collapse = ", ")
    )
  }

  invisible(legislators)
}

### Build
# members: from get_sd_members
# vote_choices: votechoices from every session
# manual_p: csv of hand-keyed ids (session_year, sd_person_id, openstates_person_id)
# review_p: where to write the members that still need an id
# -> legislators with one row per member per session, id being the OpenStates
#    person id and match_source how it was found
build_sd_legislators <- function(members, vote_choices, manual_p, review_p) {
  members <- clean_sd_members(members)
  voter_ids <- get_voter_ids(vote_choices)

  manual <- if (fs::file_exists(manual_p)) {
    readr::read_csv(
      manual_p,
      col_types = readr::cols(.default = readr::col_character(), session_year = readr::col_integer()),
      show_col_types = FALSE
    ) |>
      select(session_year, sd_person_id, openstates_person_id) |>
      mutate(sd_person_id = as.integer(sd_person_id))
  } else {
    tibble::tibble(session_year = integer(), sd_person_id = integer(), openstates_person_id = character())
  }

  members <- members |>
    link_members_to_voters(voter_ids) |>
    apply_manual_ids(manual) |>
    drop_chamber_switches() |>
    carry_ids_across_sessions()

  unmatched <- members |> filter(is.na(match_source))
  if (nrow(unmatched) > 0) {
    write_review_file(unmatched, members, voter_ids, review_p)
  }

  validate_sd_legislators(members, voter_ids)

  members |>
    arrange(session_year, chamber, district, name) |>
    select(
      session_year, chamber, district, id, sd_person_id, name, first_name,
      last_name, party, position, occupation, photo, inactive_date, match_source
    )
}
