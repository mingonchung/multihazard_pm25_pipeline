## =============================================================================
## stage04_pa_noaa_storm_events_join.R
## Join NOAA Storm Events to PurpleAir sensor locations by county.
##
## KEY DIFFERENCE FROM AIRNOW STAGE 04:
##   AirNow stage04 expands events to hourly records immediately (feasible with
##   ~2,000 AirNow sensors). PurpleAir has ~33,000 outdoor sensors, so the same
##   hourly expansion is too large to hold in memory.
##
##   This script outputs a COMPACT interval table instead:
##     STATION, EVENT_ID, EVENT_TYPE, begin_datetime, end_datetime
##   The hourly expansion is deferred to stage06_pa (per-year, in memory).
##
## Join strategy (same as AirNow stage04). Storm Events records are coded in
## two ways and are processed in separate paths, because CZ_FIPS means
## different things in each:
##   County-coded (CZ_TYPE = "C"): CZ_FIPS = county FIPS code.
##     All event types kept. Joined to sensors by state + county.
##   Zone-coded (CZ_TYPE = "Z"): CZ_FIPS = NWS forecast zone number.
##     Only Hurricane, Tropical, and Dust Storm events kept (Z_KEEP_PATTERN).
##     Each event is first resolved to the counties of its zone
##     (util_storm_zone_resolve.R: NWS zone-county file, then the lookup file
##     for old zones), then joined to sensors by state + county.
##     Events that cannot be resolved are dropped and counted.
##   Marine (CZ_TYPE = "M"): dropped.
##
## Both joins use vectorised data.table merges (no per-event for loop).
##
## Usage:
##   Rscript stage04_pa_noaa_storm_events_join.R outdoor
##   Rscript stage04_pa_noaa_storm_events_join.R indoor
##
## Storm Events raw files are shared with the AirNow pipeline (DIR_STORM_RAW).
## FIPS obtained from pre-computed us_attrs shp (util_pa_sensor_location_shp.R).
## =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(sf)
  library(data.table)
  library(stringr)
  library(tigris)
})

sf_use_s2(FALSE)
options(tigris_use_cache = TRUE)

source("config_pa.R")
source("util_storm_zone_resolve.R")

## ---------------------------------------------------------------------------
## 1. Parse argument
## ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
TYPE <- if (length(args) >= 1) tolower(trimws(args[1])) else stop(
  "Usage: Rscript stage04_pa_noaa_storm_events_join.R <outdoor|indoor>"
)
if (!TYPE %in% c("outdoor", "indoor"))
  stop("type must be 'outdoor' or 'indoor'")

OUT_FILE <- pa_stage_file("storm", TYPE)

if (file.exists(OUT_FILE)) {
  message("Output already exists, skipping: ", OUT_FILE)
  quit(save = "no", status = 0)
}

message(sprintf("Stage 04 PA | type: %s", TYPE))

## ---------------------------------------------------------------------------
## 2. Load PA sensor locations and county FIPS
##
## Preferred: use pre-computed _us_attrs shp from util_pa_sensor_location_shp.R
## Fallback:  spatial join to tigris county polygons inline
## ---------------------------------------------------------------------------
usattrs_shp <- pa_loc_shp_file(TYPE, attrs = TRUE)

if (file.exists(usattrs_shp)) {
  message("Loading pre-computed US attrs shp: ", usattrs_shp)
  sensors_fips <- st_read(usattrs_shp, quiet = TRUE) |>
    st_drop_geometry() |>
    as.data.table()

  ## Normalise FIPS column names (shp truncates to 10 chars)
  if ("statefp" %in% names(sensors_fips))
    setnames(sensors_fips, "statefp", "STATE_FIPS_chr")
  if ("cntyfp"  %in% names(sensors_fips))
    setnames(sensors_fips, "cntyfp",  "CNTY_FIPS_chr")

  sensors_fips[, STATE_FIPS := as.integer(STATE_FIPS_chr)]
  sensors_fips[, CNTY_FIPS  := as.integer(CNTY_FIPS_chr)]
  sensors_fips <- sensors_fips[!is.na(STATE_FIPS)]

  ## Ensure STATION column exists (shp may truncate long names)
  if (!"STATION" %in% names(sensors_fips)) {
    st_col <- grep("^STATION", names(sensors_fips), value = TRUE)[1]
    if (!is.na(st_col)) setnames(sensors_fips, st_col, "STATION")
  }

} else {
  message(sprintf(
    "us_attrs shp not found; running tigris county join for %s.\n",
    TYPE
  ), "  → Run util_pa_sensor_location_shp.R to pre-compute for future runs.")

  sensor_file <- pa_sensor_list_file(TYPE)
  if (!file.exists(sensor_file))
    stop("Sensor list not found: ", sensor_file,
         "\nRun acq_pa_hourly_combine.R first.")

  sensors_dt <- fread(sensor_file, select = c("STATION", "lat", "lon"))
  sensors_dt[, STATION := as.character(STATION)]
  sensors_dt <- sensors_dt[!is.na(lat) & !is.na(lon)]

  sensors_sf <- st_as_sf(sensors_dt,
                         coords = c("lon", "lat"),
                         crs    = 4326,
                         remove = FALSE)

  counties_sf <- counties(cb = TRUE, year = 2021, progress_bar = FALSE) |>
    st_transform(4326) |>
    select(STATEFP, COUNTYFP)

  sensors_fips <- st_join(sensors_sf, counties_sf, join = st_within) |>
    st_drop_geometry() |>
    as.data.table()

  sensors_fips[, STATE_FIPS := as.integer(STATEFP)]
  sensors_fips[, CNTY_FIPS  := as.integer(COUNTYFP)]
  sensors_fips <- sensors_fips[!is.na(STATE_FIPS)]
}

sensors_fips[, STATION := as.character(STATION)]
message(sprintf("Loaded %d %s sensors with FIPS.",
                n_distinct(sensors_fips$STATION), TYPE))

## Sensor lookup by county: STATE_FIPS + CNTY_FIPS (county FIPS code).
## Both event paths join to this one table.
sensor_county <- sensors_fips[!is.na(CNTY_FIPS),
                               .(STATION, STATE_FIPS, CNTY_FIPS)] |>
  unique()

message(sprintf("County lookup: %d sensors.", n_distinct(sensor_county$STATION)))

## ---------------------------------------------------------------------------
## 3. Read and parse raw Storm Events files (shared with AirNow pipeline)
## ---------------------------------------------------------------------------
raw_files <- list.files(DIR_STORM_RAW,
                        pattern = "StormEvents_details.*\\.csv\\.gz",
                        full.names = TRUE)

if (length(raw_files) == 0L)
  stop("No Storm Events files found in: ", DIR_STORM_RAW,
       "\nRun acq_noaa_storm_events_download.py first.")

message(sprintf("Reading %d raw Storm Events files ...", length(raw_files)))

events_raw <- rbindlist(lapply(raw_files, function(f) {
  fread(f, select = c("EVENT_ID", "STATE_FIPS", "CZ_FIPS", "CZ_TYPE",
                      "CZ_TIMEZONE", "EVENT_TYPE",
                      "BEGIN_DATE_TIME", "END_DATE_TIME"),
        na.strings = c("", "NA"))
}), use.names = TRUE, fill = TRUE)

## Keep county-coded (C) and zone-coded (Z) events; drop marine (M)
events_land <- events_raw[CZ_TYPE %in% c("C", "Z")]
events_land[, STATE_FIPS := as.integer(STATE_FIPS)]
events_land[, CZ_FIPS    := as.integer(CZ_FIPS)]

## Datetime parsing: local standard time → UTC (identical to AirNow stage04)
parse_local_noaa <- function(x) {
  x <- str_trim(x)
  x <- na_if(x, "")
  parse_date_time(
    x,
    orders = c("m/d/Y HMS", "m/d/Y HM",
               "m/d/y HMS", "m/d/y HM",
               "d-b-Y HMS", "d-b-Y HM",
               "d-b-y HMS", "d-b-y HM"),
    tz = "UTC", quiet = TRUE
  )
}

events_land[, offset_hr := fcase(
  CZ_TIMEZONE == "GST10",  10,
  CZ_TIMEZONE == "SST-11", 11,
  default = suppressWarnings(
    as.numeric(str_extract(CZ_TIMEZONE, "(?<=-)[0-9]+"))
  )
)]
events_land[, offset_hr := fcoalesce(offset_hr, 0)]

events_land[, begin_dt := with_tz(
  parse_local_noaa(BEGIN_DATE_TIME) + hours(offset_hr), "UTC")]
events_land[, end_dt := with_tz(
  parse_local_noaa(END_DATE_TIME)   + hours(offset_hr), "UTC")]

events_land <- events_land[!is.na(begin_dt) & !is.na(end_dt)]

## Filter to PA study period
study_start_dt <- as.POSIXct(paste0(PA_STUDY_START, " 00:00:00"), tz = "UTC")
study_end_dt   <- as.POSIXct(paste0(PA_STUDY_END,   " 23:59:59"), tz = "UTC")
events_land    <- events_land[begin_dt <= study_end_dt & end_dt >= study_start_dt]

message(sprintf("Retained %d land events (C+Z) within PA study period.",
                nrow(events_land)))

## ---------------------------------------------------------------------------
## 4. Join events to sensors — vectorised, NO hourly expansion
##
## Output: compact interval rows (STATION × EVENT_ID).
## Stage06_pa expands to hourly within each year's loop where memory is bounded
## to one year's worth of events × sensors.
## ---------------------------------------------------------------------------
message("Joining events to sensors (vectorised interval join; no hourly expansion) ...")

## ---- 4a. County-coded events: CZ_FIPS is the county FIPS code ----
c_events <- events_land[
  CZ_TYPE == "C",
  .(EVENT_ID, EVENT_TYPE, STATE_FIPS, CNTY_FIPS = CZ_FIPS, begin_dt, end_dt)
]

matched_c <- sensor_county[
  c_events,
  on      = .(STATE_FIPS, CNTY_FIPS),
  nomatch = 0L,
  allow.cartesian = TRUE
][, .(STATION, EVENT_ID, EVENT_TYPE,
      begin_datetime = begin_dt,
      end_datetime   = end_dt)]

message(sprintf("  County-coded: %d events read | %d sensor×event rows",
                uniqueN(c_events$EVENT_ID), nrow(matched_c)))

## ---- 4b. Zone-coded events: CZ_FIPS is the NWS zone number ----
##
## Zone-coded events are resolved to the counties of their zone and then
## joined by county. Only event types that are used in MS1 hazard
## classification (stage07/08 grepl patterns) are kept.
##
## EXCLUDED from zone-coded processing:
##   Drought       → captured by SPEI (point-level)
##   Heat          → captured by GridMET tmmx (point-level); grepl("Heat") in
##                   stage07/08 is already commented out in favour of tmmx threshold
##   Dense Fog, High Wind, Winter Storm, Blizzard, Ice Storm, Heavy Snow,
##   Cold Wind Chill, Frost/Freeze → not used in MS1 hazard classification;
##                   their EVENT_ID would remove baseline hours
##   Rip Current, Astronomical Low Tide, Waterspout, Storm Surge/Tide → marine /
##                   coastal phenomena
##   Coastal Flood, Lakeshore Flood → zone-coded flood variants; Flash Flood
##                   and Flood (riverine) are county-coded and already captured
##
## KEPT in zone-coded processing:
##   Hurricane (Typhoon), Tropical Storm, Tropical Depression
##   Dust Storm    → AOD > cln.thr filter in stage07/08 further guards against
##                   false classification

Z_KEEP_PATTERN <- paste(
  "Hurricane", "Tropical", "Dust Storm",
  sep = "|"
)

z_events <- events_land[
  CZ_TYPE == "Z" & grepl(Z_KEEP_PATTERN, EVENT_TYPE, ignore.case = TRUE),
  .(EVENT_ID, EVENT_TYPE, STATE_FIPS, ZONE_NUM = CZ_FIPS, begin_dt, end_dt)
]

zone_county <- read_zone_county(FILE_ZONE_COUNTY, FILE_ZONE_LEGACY)
z_res       <- resolve_zone_events(z_events, zone_county)

## Resolved zone-coded events now carry a county code; same join as 4a
zc_events <- unique(
  z_res$events[, .(EVENT_ID, EVENT_TYPE, STATE_FIPS, CNTY_FIPS, begin_dt, end_dt)],
  by = c("EVENT_ID", "STATE_FIPS", "CNTY_FIPS")
)

matched_z <- sensor_county[
  zc_events,
  on      = .(STATE_FIPS, CNTY_FIPS),
  nomatch = 0L,
  allow.cartesian = TRUE
][, .(STATION, EVENT_ID, EVENT_TYPE,
      begin_datetime = begin_dt,
      end_datetime   = end_dt)]

message(sprintf("  Zone-coded (%s): %d events read | %d sensor×event rows",
                Z_KEEP_PATTERN, uniqueN(z_events$EVENT_ID), nrow(matched_z)))
message("  Zone-coded events by outcome:")
for (k in seq_len(nrow(z_res$log)))
  message(sprintf("    %-58s %6d", z_res$log$outcome[k], z_res$log$events[k]))

## -- Combine, deduplicate, sort --
out <- rbindlist(list(matched_c, matched_z), use.names = TRUE)
out <- unique(out, by = c("STATION", "EVENT_ID"))
setorder(out, STATION, begin_datetime)

message(sprintf(
  "Output: %d sensor×event rows | %d unique STATIONs | %d unique EVENT_IDs.",
  nrow(out), n_distinct(out$STATION), n_distinct(out$EVENT_ID)
))

## ---------------------------------------------------------------------------
## 5. Write
## ---------------------------------------------------------------------------
dir.create(dirname(OUT_FILE), showWarnings = FALSE, recursive = TRUE)
fwrite(out, OUT_FILE, compress = "gzip")
message("[DONE] Wrote: ", OUT_FILE)
