library(fs)
library(futile.logger)

APP_SESSION_FILES <- c(
  "house_backbone.parquet",
  "senate_backbone.parquet",
  "sponsorship_backbone.parquet"
)

# Files in the processed folder that cover every session
APP_ROOT_FILES <- c(
  "legislators_sd.parquet",
  "legislator_photos.parquet",
  "vote_alignment.parquet",
  "legislator_votes.parquet"
)

# Copy from the processed folder into inst/extdata
publish_app_data <- function(processed_base_path, app_data_base_path) {
  root_src <- path(processed_base_path, APP_ROOT_FILES)
  if (!all(file_exists(root_src))) {
    stop(
      "Missing from ", processed_base_path, ": ",
      paste(path_file(root_src[!file_exists(root_src)]), collapse = ", ")
    )
  }

  year_dirs <- dir_ls(processed_base_path, type = "directory", regexp = "20[0-9]{2}$")

  for (year_dir in year_dirs) {
    src <- path(year_dir, APP_SESSION_FILES)
    if (!all(file_exists(src))) {
      flog.warn(paste0("Skipping ", path_file(year_dir), ": missing app files"))
      next
    }
    dest_dir <- path(app_data_base_path, path_file(year_dir))
    dir_create(dest_dir)
    file_copy(src, dest_dir, overwrite = TRUE)
    flog.info(paste0("Published ", path_file(year_dir), " to ", dest_dir))
  }

  file_copy(root_src, app_data_base_path, overwrite = TRUE)
  flog.info(paste0("Published ", paste(APP_ROOT_FILES, collapse = ", ")))
}
