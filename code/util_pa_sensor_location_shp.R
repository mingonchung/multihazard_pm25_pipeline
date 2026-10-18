## =============================================================================
## util_pa_sensor_location_shp.R
## Build PurpleAir sensor location shapefiles from the sensor list CSV
## (produced by acq_pa_hourly_combine.R) and attach US spatial attributes.
##
## This script replaces the shp-creation parts of:
##   2_2_purpleair_PM25_cumulative.R         (outdoor shp from combined files)
##   2_2_purpleair_PM25_cumulative_indoor.R  (indoor shp from combined files)
##   0_all_us_urban_rural_county_state_extract.R (PA sections)
##
## Why sensor_list_{type}.csv instead of combined hourly files:
##   - sensor_list is the canonical source produced at download time
##   - Avoids reading gigabytes of hourly data just for unique locations
##   - Column schema of combined files changed between the EPA RSIG version
##     and the native PA API version; sensor_list is stable
##
## Outputs (written to DIR_PA_SHP):
##   purpleair_{outdoor|indoor}_location.shp
##       Columns: STATION, lat, lon, elevation
##
##   purpleair_{outdoor|indoor}_location_us_attrs.shp
##       Adds:  urban (0/1), ua_name, statefp, stusps, st_name,
##              cntyfp, cnty_name, cnty_geoid
##       Used by: stage04_pa_noaa_storm_events_join.R (county FIPS lookup)
##
## Usage:
##   Rscript util_pa_sensor_location_shp.R outdoor
##   Rscript util_pa_sensor_location_shp.R indoor
##   Rscript util_pa_sensor_location_shp.R all      # runs both types
##
## Prerequisite: acq_pa_hourly_combine.R must have completed for all years
##   of both types. The combine step creates sensor_list_{type}.csv
##   (columns: STATION, lat, lon, elevation) as a side output.
##   Verify before running:
##     /purpleair/raw/outdoor/sensor_list_outdoor.csv
##     /purpleair/raw/indoor/sensor_list_indoor.csv
##
## Outputs (two shp pairs — one per type):
##   purpleair_outdoor_location.shp + purpleair_outdoor_location_us_attrs.shp
##   purpleair_indoor_location.shp  + purpleair_indoor_location_us_attrs.shp
##   Sensor ID column in all shp files: STATION (integer, matches STATION(-)
##   in combined files and sensor_list.csv)
## =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(data.table)
  library(tigris)
})

sf_use_s2(FALSE)
options(tigris_use_cache = TRUE, tigris_class = "sf")

source("config_pa.R")

## ---------------------------------------------------------------------------
## 1. Parse argument
## ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
TYPE_ARG <- if (length(args) >= 1) tolower(trimws(args[1])) else "all"
if (!TYPE_ARG %in% c("outdoor", "indoor", "all"))
  stop("Usage: Rscript util_pa_sensor_location_shp.R <outdoor|indoor|all>")

TYPES <- if (TYPE_ARG == "all") c("outdoor", "indoor") else TYPE_ARG

## ---------------------------------------------------------------------------
## 2. Load shared spatial layers (once, reused for both types)
## ---------------------------------------------------------------------------
message("Loading USCAN boundary ...")
uscan <- st_read(FILE_USCAN_SHP, quiet = TRUE) |> st_transform(4326) |> st_make_valid()

message("Loading US states and counties (TIGER/Line 2020) ...")
states   <- states(  year = 2020, cb = TRUE, progress_bar = FALSE) |> st_transform(4326)
counties <- counties(year = 2020, cb = TRUE, progress_bar = FALSE) |> st_transform(4326)
us_union <- st_union(states) |> st_make_valid()   # US-only boundary for filtering

message("Loading Urban Areas 2020 ...")
ua <- st_read(FILE_URBAN_SHP, quiet = TRUE) |> st_transform(4326) |> st_make_valid()

## Detect Urban Area name column (same logic as 0_all_ script)
cn   <- names(ua)
cand <- grep("^(NAME20|NAME)$", cn, ignore.case = TRUE, value = TRUE)
if (!length(cand))
  cand <- grep("(GEOID20|GEOID|UACE20|UACE)", cn, ignore.case = TRUE, value = TRUE)
if (!length(cand))
  stop("Cannot find UA name column. Available: ", paste(cn, collapse = ", "))
ua_name_col <- cand[1]

## ---------------------------------------------------------------------------
## 3. Helper functions (same logic as util_sensor_us_attrs.R / 0_all_ script)
## ---------------------------------------------------------------------------
add_urban_flag <- function(pts) {
  idx      <- st_intersects(pts, ua)
  is_urban <- lengths(idx) > 0
  ua_name  <- rep(NA_character_, nrow(pts))
  if (any(is_urban)) {
    first_idx       <- sapply(idx[is_urban], `[[`, 1)
    ua_name[is_urban] <- ua[[ua_name_col]][first_idx]
  }
  pts$urban   <- ifelse(is_urban, 1L, 0L)
  pts$ua_name <- ua_name
  pts
}

add_state_county <- function(pts) {
  ## County join
  pts_c <- st_join(pts,
                   counties |> select(STATEFP, COUNTYFP, NAME, GEOID),
                   left = TRUE)
  names(pts_c)[names(pts_c) == "NAME"]  <- "cnty_name"
  names(pts_c)[names(pts_c) == "GEOID"] <- "cnty_geoid"

  ## State join
  pts_cs <- st_join(pts_c,
                    states |> select(STATEFP, STUSPS, NAME),
                    left = TRUE)
  names(pts_cs)[names(pts_cs) == "NAME"]       <- "state_name"
  names(pts_cs)[names(pts_cs) == "STATEFP.x"]  <- "county_stfp"
  names(pts_cs)[names(pts_cs) == "STATEFP.y"]  <- "statefp"

  ## Keep and rename shapefile-safe columns (≤10 chars)
  keep <- c("urban", "ua_name", "statefp", "STUSPS", "state_name",
            "COUNTYFP", "cnty_name", "cnty_geoid")
  keep <- keep[keep %in% names(pts_cs)]
  pts_cs <- pts_cs |> select(any_of(keep), everything())

  sn <- names(pts_cs)
  sn[sn == "STUSPS"]     <- "stusps"
  sn[sn == "state_name"] <- "st_name"
  sn[sn == "COUNTYFP"]   <- "cntyfp"
  names(pts_cs) <- substr(sn, 1, 10)
  pts_cs
}

## ---------------------------------------------------------------------------
## 4. Process each type
## ---------------------------------------------------------------------------
if (!dir.exists(DIR_PA_SHP)) dir.create(DIR_PA_SHP, recursive = TRUE)

for (TYPE in TYPES) {

  message(sprintf("\n===== Processing type: %s =====", TYPE))

  sensor_csv <- pa_sensor_list_file(TYPE)
  if (!file.exists(sensor_csv))
    stop("Sensor list not found: ", sensor_csv,
         "\nRun acq_pa_hourly_combine.R for all years of type '", TYPE, "' first.")

  ## -- Load sensor list --
  ## Columns written by acq_pa_hourly_combine.R: STATION, lat, lon, elevation
  sensors_dt <- fread(sensor_csv)
  sensors_dt[, STATION := as.character(STATION)]
  sensors_dt <- sensors_dt[!is.na(lat) & !is.na(lon) & STATION != ""]
  message(sprintf("Sensor list loaded: %d sensors.", nrow(sensors_dt)))

  ## -- Build sf point layer --
  sensors_sf <- st_as_sf(sensors_dt,
                          coords = c("lon", "lat"),
                          crs    = 4326,
                          remove = FALSE)   # keep lat/lon columns in shp

  ## -- Wrap dateline (Alaska sensors) --
  sensors_sf <- st_wrap_dateline(sensors_sf,
                                 options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

  ## -- Filter to USCAN boundary --
  n0 <- nrow(sensors_sf)
  sensors_sf <- st_filter(sensors_sf, uscan, .predicate = st_within)
  message(sprintf("USCAN filter: %d → %d sensors.", n0, nrow(sensors_sf)))

  if (nrow(sensors_sf) == 0L) {
    warning("No sensors remain after USCAN filter for type: ", TYPE)
    next
  }

  ## -- Ensure SHP-safe column names (≤10 chars) --
  names(sensors_sf) <- substr(names(sensors_sf), 1, 10)

  ## -- Write base location shp --
  out_base <- pa_loc_shp_file(TYPE, attrs = FALSE)
  st_write(sensors_sf, out_base, delete_layer = TRUE, quiet = TRUE)
  message("Wrote base shp: ", out_base)

  ## -- Filter to US-only for US attrs (excludes Canada) --
  n_uscan <- nrow(sensors_sf)
  us_keep <- st_intersects(sensors_sf, us_union, sparse = FALSE)[, 1]
  sensors_us <- sensors_sf[us_keep, ]
  message(sprintf("US-only filter for attrs: %d → %d sensors.",
                  n_uscan, nrow(sensors_us)))

  ## -- Add urban flag and state/county FIPS --
  sensors_us <- add_urban_flag(sensors_us)
  sensors_us <- add_state_county(sensors_us)
  sensors_us <- st_transform(sensors_us, 4326)
  names(sensors_us) <- substr(names(sensors_us), 1, 10)

  ## -- Write us_attrs shp --
  out_attrs <- pa_loc_shp_file(TYPE, attrs = TRUE)
  st_write(sensors_us, out_attrs, delete_layer = TRUE, quiet = TRUE)
  message("Wrote us_attrs shp: ", out_attrs)

  message(sprintf("Done: %s | %d USCAN sensors | %d US sensors with attrs.",
                  TYPE, nrow(sensors_sf), nrow(sensors_us)))
}

message("\nAll done.")
