library(dplyr)
library(jsonlite)
library(futile.logger)

LRC_API_URL <- "https://sdlegislature.gov/api"

### Members
# Regular sessions in `years`, special sessions excluded
# -> a df with session_id and session_year
get_session_id_lookup <- function(years) {
  lookup <- fromJSON(paste0(LRC_API_URL, "/Sessions")) |>
    filter(!SpecialSession) |>
    transmute(session_id = SessionId, 
          session_year = as.integer(Year)) |>
    filter(session_year %in% years)

    missing_years <- setdiff(years, lookup$session_year)
    if (length(missing_years) > 0) {
      stop("No regular session found on the site for: ", paste(missing_years, collapse = ", "))
    }

    return(lookup)
  }

# The members of one session, from the raw json saved in raw_data_path so
# builds don't depend on the site. Set refresh to download it again.
get_sd_session_members <- function(session_id,
                                   session_year,
                                   raw_data_path,
                                   refresh = FALSE) {
  members_p <- fs::path(raw_data_path, paste0("SD_", session_year, "_members.json"))

  if (refresh || !fs::file_exists(members_p)) {
    fs::dir_create(raw_data_path)
    download.file(
      paste0(LRC_API_URL, "/SessionMembers/Session/", session_id),
      members_p,
      quiet = TRUE
    )
    flog.info(paste0("Wrote ", members_p))
  }

  fromJSON(members_p) |>
    tibble::as_tibble() |>
    mutate(session_year = session_year)
}

get_sd_members <- function(years, raw_data_path, refresh = FALSE) {
  
  session_id_lookup <- get_session_id_lookup(years)

  purrr::map2(
    session_id_lookup$session_id,
    session_id_lookup$session_year,
    get_sd_session_members,
    raw_data_path = raw_data_path,
    refresh = refresh
  ) |>
    bind_rows()
}

### Bills
# The bills of one session, from the raw json saved in raw_data_path like the
# members. Set refresh to download it again.
# -> a df with session_year, bill (e.g. "HB 1045") and the site's bill_url
get_sd_session_bills <- function(session_id,
                                 session_year,
                                 raw_data_path,
                                 refresh = FALSE) {
  bills_p <- fs::path(raw_data_path, paste0("SD_", session_year, "_bills.json"))

  if (refresh || !fs::file_exists(bills_p)) {
    fs::dir_create(raw_data_path)
    download.file(
      paste0(LRC_API_URL, "/Bills/Session/", session_id),
      bills_p,
      quiet = TRUE
    )
    flog.info(paste0("Wrote ", bills_p))
  }

  fromJSON(bills_p) |>
    tibble::as_tibble() |>
    transmute(
      session_year = as.integer(session_year),
      bill = paste(BillType, BillNumberOnly),
      bill_url = paste0("https://sdlegislature.gov/Session/Bill/", BillId)
    )
}

get_sd_bills <- function(years, raw_data_path, refresh = FALSE) {
  session_id_lookup <- get_session_id_lookup(years)

  purrr::map2(
    session_id_lookup$session_id,
    session_id_lookup$session_year,
    get_sd_session_bills,
    raw_data_path = raw_data_path,
    refresh = refresh
  ) |>
    bind_rows()
}

### Photos
# The thumbnail of a few members is a full-size photo of hundreds of KB, which
# would be sent with the graph each time, so photos over this stay as links
MAX_PHOTO_BYTES <- 100 * 1024

# A photo as text a browser can draw without requesting a file
# photo_p: the path of an image
photo_data_uri <- function(photo_p) {
  mime <- c(webp = "image/webp", jpg = "image/jpeg", jpeg = "image/jpeg", png = "image/png")
  # base64_enc breaks lines every 76 characters, which a data URI has no use for
  encoded <- gsub("\n", "", jsonlite::base64_enc(readBin(photo_p, "raw", fs::file_size(photo_p))), fixed = TRUE)
  paste0("data:", mime[[tolower(tools::file_ext(photo_p))]], ";base64,", encoded)
}

# Saves each photo in photos_path, skipping the ones already saved so builds
# don't depend on the site. A photo that fails to download, or is bigger than
# max_bytes, is logged and left out.
# urls: the photo of each member, as in get_sd_members
# -> a df with the url and data_uri of each photo saved
get_sd_photos <- function(urls, photos_path, max_bytes = MAX_PHOTO_BYTES) {
  urls <- unique(urls[!is.na(urls)])
  fs::dir_create(photos_path)
  photo_p <- fs::path(photos_path, paste0(purrr::map_chr(urls, rlang::hash), ".", tools::file_ext(urls)))

  for (i in which(!fs::file_exists(photo_p))) {
    failed <- tryCatch(
      download.file(utils::URLencode(urls[i]), photo_p[i], quiet = TRUE, mode = "wb") != 0,
      error = \(e) TRUE,
      warning = \(w) TRUE
    )
    if (failed) {
      fs::file_delete(photo_p[i][fs::file_exists(photo_p[i])])
      flog.warn(paste0("Could not download ", urls[i]))
    }
  }
  flog.info(paste0("Saved ", sum(fs::file_exists(photo_p)), "/", length(urls), " photos in ", photos_path))

  saved <- fs::file_exists(photo_p)
  sizes <- rep(0, length(photo_p))
  sizes[saved] <- fs::file_size(photo_p[saved])
  too_big <- sizes > max_bytes
  if (any(too_big)) {
    flog.info(paste0(
      sum(too_big), " photos are over ", max_bytes / 1024, " KB and stay as links: ",
      paste(urls[too_big], collapse = ", ")
    ))
  }
  saved <- saved & !too_big

  tibble::tibble(
    url = urls[saved],
    data_uri = purrr::map_chr(photo_p[saved], photo_data_uri)
  )
}
