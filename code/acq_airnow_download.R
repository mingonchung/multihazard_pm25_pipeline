## =============================================================================
## acq_airnow_download.R
## Download AirNow hourly .dat files and site metadata files from AWS S3.
##
## Downloads 24 hourly HourlyData_YYYYMMDDhh.dat files per day, plus
## active-sites.dat, historical-sites.dat, and (from 2016)
## monitoring_site_locations.dat.
##
## Source: https://s3-us-west-1.amazonaws.com/files.airnowtech.org/airnow/
## Output: DIR_AIRNOW_HOURLY/<YYYY>/<YYYYMMDD>/*.dat
## =============================================================================

rm(list = ls(all.names = TRUE))

source("config.R")

## Date range — downloads the full study period
all_dates <- seq(STUDY_START, STUDY_END, by = "day")

for (ii in seq_along(all_dates)) {

  current_date <- all_dates[ii]
  yyyy     <- format(current_date, "%Y")
  yyyymmdd <- format(current_date, "%Y%m%d")

  local_dir <- file.path(DIR_AIRNOW_HOURLY, yyyy, yyyymmdd)
  if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

  ## Download 24 hourly data files
  for (hour in 0:23) {
    file_name  <- paste0("HourlyData_", yyyymmdd, sprintf("%02d", hour), ".dat")
    remote_url <- paste0(
      "https://s3-us-west-1.amazonaws.com/files.airnowtech.org/airnow/",
      yyyy, "/", yyyymmdd, "/", file_name
    )
    local_file <- file.path(local_dir, file_name)
    tryCatch(
      download.file(remote_url, destfile = local_file, mode = "wb", quiet = TRUE),
      error = function(e) message("Skipping missing file: ", remote_url)
    )
  }

  ## Download site metadata files
  site_files <- c("active-sites.dat", "historical-sites.dat")
  if (as.integer(yyyy) >= 2016)
    site_files <- c(site_files, "monitoring_site_locations.dat")

  for (f in site_files) {
    remote_url <- paste0(
      "https://s3-us-west-1.amazonaws.com/files.airnowtech.org/airnow/",
      yyyy, "/", yyyymmdd, "/", f
    )
    local_file <- file.path(local_dir, f)
    tryCatch(
      download.file(remote_url, destfile = local_file, mode = "wb", quiet = TRUE),
      error = function(e) message("Skipping missing file: ", remote_url)
    )
  }

  cat("Downloaded:", yyyymmdd, "\n")
}
