# The data-raw scripts aren't part of the installed package
publish_p <- test_path("../../data-raw/publish_app_data.R")
skip_if_not(file.exists(publish_p), "data-raw is not available")
for (pkg in c("fs", "futile.logger")) skip_if_not_installed(pkg)
source(publish_p, local = TRUE)

# A processed folder with a complete 2024, a 2023 missing a file, and the
# files covering every session
fake_processed <- function(root_files = APP_ROOT_FILES) {
  processed <- withr::local_tempdir(.local_envir = parent.frame())
  for (year in c(2023, 2024)) {
    fs::dir_create(fs::path(processed, year))
    files <- if (year == 2023) APP_SESSION_FILES[-1] else APP_SESSION_FILES
    fs::file_create(fs::path(processed, year, files))
  }
  fs::file_create(fs::path(processed, root_files))
  processed
}

test_that("publish_app_data stages complete sessions and the root files", {
  processed <- fake_processed()
  app_data <- withr::local_tempdir()

  suppressMessages(publish_app_data(processed, app_data))

  expect_setequal(fs::path_file(fs::dir_ls(app_data, type = "directory")), "2024")
  expect_setequal(fs::path_file(fs::dir_ls(fs::path(app_data, 2024))), APP_SESSION_FILES)
  expect_true(all(fs::file_exists(fs::path(app_data, APP_ROOT_FILES))))
})

test_that("publish_app_data copies nothing if a root file is missing", {
  processed <- fake_processed(root_files = "legislators.parquet")
  app_data <- withr::local_tempdir()

  expect_error(publish_app_data(processed, app_data), "legislators_sd.parquet")
  expect_length(fs::dir_ls(app_data), 0)
})
