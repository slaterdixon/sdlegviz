library(fs)
library(futile.logger)

APP_BACKBONES <- c(
  "house_backbone.parquet",
  "senate_backbone.parquet",
  "sponsorship_backbone.parquet"
)

# Copy what the app reads from the processed folder into inst/extdata,
# so the app ships with its data
publish_app_data <- function(processed_base_path, app_data_base_path) {
  year_dirs <- dir_ls(processed_base_path, type = "directory", regexp = "20[0-9]{2}$")

  for (year_dir in year_dirs) {
    src <- path(year_dir, APP_BACKBONES)
    if (!all(file_exists(src))) {
      flog.warn(paste0("Skipping ", path_file(year_dir), ": missing backbones"))
      next
    }
    dest_dir <- path(app_data_base_path, path_file(year_dir))
    dir_create(dest_dir)
    file_copy(src, dest_dir, overwrite = TRUE)
    flog.info(paste0("Published ", path_file(year_dir), " to ", dest_dir))
  }

  file_copy(
    path(processed_base_path, "legislators.parquet"),
    app_data_base_path,
    overwrite = TRUE
  )
}
