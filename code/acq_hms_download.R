## =============================================================================
## acq_hms_download.R
## Download NOAA HMS smoke polygon and fire point zip files.
##
## Source: https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/
## Outputs:
##   DIR_HMS_SMOKE/hms_smoke<YYYYMMDD>.zip
##   DIR_HMS_FIRE/hms_fire<YYYYMMDD>.zip
## =============================================================================

rm(list = ls(all.names = TRUE))

source("config.R")

## Date range — downloads the full study period
all_dates <- seq(STUDY_START, STUDY_END, by = "day")

if (!dir.exists(DIR_HMS_SMOKE)) dir.create(DIR_HMS_SMOKE, recursive = TRUE)
if (!dir.exists(DIR_HMS_FIRE))  dir.create(DIR_HMS_FIRE,  recursive = TRUE)

for (ii in seq_along(all_dates)) {

  current_date <- all_dates[ii]
  yyyy <- format(current_date, "%Y")
  mm   <- format(current_date, "%m")
  dd   <- format(current_date, "%d")

  ## Smoke polygons
  url_smoke   <- paste0(
    "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/",
    yyyy, "/", mm, "/hms_smoke", yyyy, mm, dd, ".zip"
  )
  local_smoke <- file.path(DIR_HMS_SMOKE, paste0("hms_smoke", yyyy, mm, dd, ".zip"))
  tryCatch(
    download.file(url_smoke, destfile = local_smoke, mode = "wb", quiet = TRUE),
    error = function(e) message("Skipping missing smoke file: ", url_smoke)
  )

  ## Fire points
  url_fire   <- paste0(
    "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Fire_Points/Shapefile/",
    yyyy, "/", mm, "/hms_fire", yyyy, mm, dd, ".zip"
  )
  local_fire <- file.path(DIR_HMS_FIRE, paste0("hms_fire", yyyy, mm, dd, ".zip"))
  tryCatch(
    download.file(url_fire, destfile = local_fire, mode = "wb", quiet = TRUE),
    error = function(e) message("Skipping missing fire file: ", url_fire)
  )

  cat("Downloaded:", yyyy, mm, dd, "\n")
}
