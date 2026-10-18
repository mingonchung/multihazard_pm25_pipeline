## =============================================================================
## acq_airnow_station_metadata.R
## Extract AirNow station coordinates from active-sites.dat and
## monitoring_site_locations.dat files downloaded by acq_airnow_download.R.
##
## Manual step: the output of this script was completed by hand (README,
## "Inputs prepared by hand").
##
## Outputs:
##   airnow_aqsid_location_010926.csv     — output of this script (one row per
##                                          AQSID; decimal coordinates missing
##                                          for some sites)
##   airnow_aqsid_location_rev_010926.csv — hand-edited version used by all
##                                          downstream scripts (FILE_AIRNOW_LOCS
##                                          in config.R): missing decimal
##                                          coordinates filled, and column
##                                          AQSID_rev added to link identifiers
##                                          of the same or a co-located site
##                                          (used only to share smoke and storm
##                                          flags between linked identifiers)
## =============================================================================

rm(list = ls(all.names = TRUE))

library(dplyr)
library(tidyr)
library(readr)

source("config.R")

all_dates <- seq(STUDY_START, STUDY_END, by = "day")

all_sites <- data.frame(
  AQSID = character(), lat_deg = numeric(), lat_min = numeric(),
  lat_sec = numeric(), long_deg = numeric(), long_min = numeric(),
  long_sec = numeric(), lat_numeric = numeric(), long_numeric = numeric(),
  station_name = character(), organization = character(), location = character(),
  stringsAsFactors = FALSE
)

for (ii in seq_along(all_dates)) {

  current_date <- all_dates[ii]
  yyyy     <- format(current_date, "%Y")
  yyyymmdd <- format(current_date, "%Y%m%d")
  folder_path <- file.path(DIR_AIRNOW_HOURLY, yyyy, yyyymmdd)

  ## A) active-sites.dat
  active_file <- file.path(folder_path, "active-sites.dat")
  if (file.exists(active_file) && file.info(active_file)$size > 0) {
    df_active <- read.csv(active_file, header = FALSE, stringsAsFactors = FALSE,
                          strip.white = TRUE)
    colnames(df_active)[1:9] <- c("AQSID", "lat_deg", "lat_min", "lat_sec",
                                  "long_deg", "long_min", "long_sec",
                                  "station_name", "region")
    df_active <- df_active[, c("AQSID", "lat_deg", "lat_min", "lat_sec",
                               "long_deg", "long_min", "long_sec", "station_name")]
    for (cn in c("lat_deg","lat_min","lat_sec","long_deg","long_min","long_sec"))
      df_active[[cn]] <- as.numeric(df_active[[cn]])
    df_active$lat_numeric  <- NA
    df_active$long_numeric <- NA
    df_active$organization <- NA
    df_active$location     <- NA
  } else { df_active <- NULL }

  ## B) monitoring_site_locations.dat
  monitor_file <- file.path(folder_path, "monitoring_site_locations.dat")
  if (file.exists(monitor_file) && file.info(monitor_file)$size > 0) {
    df_mon_raw <- read_delim(monitor_file, delim = "|", col_names = FALSE,
                             na = c("", "NA"))
    df_monitor <- data.frame(
      AQSID        = df_mon_raw[[1]],
      organization = df_mon_raw[[7]],
      lat_numeric  = df_mon_raw[[9]],
      long_numeric = df_mon_raw[[10]],
      location     = df_mon_raw[[17]],
      lat_deg = NA, lat_min = NA, lat_sec = NA,
      long_deg = NA, long_min = NA, long_sec = NA,
      station_name = NA,
      stringsAsFactors = FALSE
    )
    df_monitor <- df_monitor[, c("AQSID","organization","location","station_name",
                                 "lat_deg","lat_min","lat_sec",
                                 "long_deg","long_min","long_sec",
                                 "lat_numeric","long_numeric")]
  } else { df_monitor <- NULL }

  ## C) Combine and merge into all_sites
  daily_df <- if (!is.null(df_active) && !is.null(df_monitor)) {
    rbind(df_active, df_monitor)
  } else if (!is.null(df_active)) {
    df_active
  } else if (!is.null(df_monitor)) {
    df_monitor
  } else { NULL }

  if (!is.null(daily_df) && nrow(daily_df) > 0) {
    for (r in seq_len(nrow(daily_df))) {
      new_aqsid <- daily_df$AQSID[r]
      idx <- which(all_sites$AQSID == new_aqsid)
      if (length(idx) == 0) {
        all_sites <- rbind(all_sites, daily_df[r, ])
      } else {
        for (col_n in c("lat_deg","lat_min","lat_sec","long_deg","long_min","long_sec",
                        "lat_numeric","long_numeric","station_name","organization","location")) {
          old_val <- all_sites[idx, col_n]
          new_val <- daily_df[r, col_n]
          if ((is.na(old_val) || old_val == "") && !is.na(new_val) && new_val != "")
            all_sites[idx, col_n] <- new_val
        }
      }
    }
  }
  cat("Processed:", yyyymmdd, "\n")
}

## Clean station names and write output
all_sites$station_name <- iconv(all_sites$station_name, from = "latin1",
                                to = "UTF-8", sub = "")
all_sites$station_name <- trimws(gsub("^'|'$", "", all_sites$station_name))

## No coordinate validation or duplicate resolution is done in this script.
## The file written below was checked and completed by hand (see header):
## missing lat/long_numeric values filled and AQSID_rev added; the edited file
## is saved as airnow_aqsid_location_rev_010926.csv (FILE_AIRNOW_LOCS).

write.csv(all_sites, file.path(DIR_AIRNOW_HOURLY, "airnow_aqsid_location_010926.csv"),
          row.names = FALSE)
