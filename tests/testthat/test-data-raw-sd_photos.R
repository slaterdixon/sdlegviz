# The data-raw scripts aren't part of the installed package
sd_data_p <- test_path("../../data-raw/get_sd_data.R")
skip_if_not(file.exists(sd_data_p), "data-raw is not available")
for (pkg in c("dplyr", "jsonlite", "futile.logger", "fs", "purrr", "rlang", "tibble")) {
  skip_if_not_installed(pkg)
}
source(sd_data_p, local = TRUE)

# Writes a file of random bytes and returns its path and file:// URL
fake_photo <- function(dir, name, size = 100) {
  photo_p <- fs::path(dir, name)
  writeBin(as.raw(sample(0:255, size, replace = TRUE)), photo_p)
  list(path = photo_p, url = paste0("file://", photo_p))
}

test_that("photo_data_uri gives the bytes of the photo as text", {
  photo <- fake_photo(withr::local_tempdir(), "a.webp")

  uri <- photo_data_uri(photo$path)

  expect_match(uri, "^data:image/webp;base64,")
  expect_false(grepl("\n", uri, fixed = TRUE))
  expect_equal(
    jsonlite::base64_dec(sub("^data:[^,]+,", "", uri)),
    readBin(photo$path, "raw", 100)
  )
  expect_match(photo_data_uri(fake_photo(withr::local_tempdir(), "b.jpg")$path), "^data:image/jpeg;")
})

test_that("get_sd_photos saves each photo once and keeps working without the site", {
  source_dir <- withr::local_tempdir()
  photos_path <- withr::local_tempdir()
  a <- fake_photo(source_dir, "a.webp")
  b <- fake_photo(source_dir, "b.jpg")

  photos <- suppressMessages(get_sd_photos(c(a$url, b$url, a$url, NA), photos_path))
  expect_setequal(photos$url, c(a$url, b$url))
  expect_length(fs::dir_ls(photos_path), 2)

  # Saved photos are used when the site can't be reached
  fs::file_delete(c(a$path, b$path))
  again <- suppressMessages(get_sd_photos(c(a$url, b$url), photos_path))
  expect_equal(again[order(again$url), ], photos[order(photos$url), ])
})

test_that("a photo that can't be downloaded is left out", {
  source_dir <- withr::local_tempdir()
  photos_path <- withr::local_tempdir()
  a <- fake_photo(source_dir, "a.webp")
  missing_url <- paste0("file://", fs::path(source_dir, "missing.webp"))

  photos <- suppressMessages(get_sd_photos(c(a$url, missing_url), photos_path))

  expect_equal(photos$url, a$url)
  expect_length(fs::dir_ls(photos_path), 1)
})

test_that("a photo over the size limit is left out", {
  source_dir <- withr::local_tempdir()
  photos_path <- withr::local_tempdir()
  small <- fake_photo(source_dir, "small.webp", size = 100)
  big <- fake_photo(source_dir, "big.jpg", size = 500)

  photos <- suppressMessages(get_sd_photos(c(small$url, big$url), photos_path, max_bytes = 200))

  expect_equal(photos$url, small$url)
})
