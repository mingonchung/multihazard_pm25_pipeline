## =============================================================================
## stage04_noaa_storm_events_join.R
## Join NOAA Storm Events to AirNow sensor locations by county,
## then expand to hourly sensor-level records.
##
## Storm Events records are coded in two ways, and the two are processed in
## separate paths (CZ_FIPS means different things in each):
##
##   County-coded (CZ_TYPE = "C")  CZ_FIPS = county FIPS code
##     All event types are kept. Joined to sensors by state + county.
##
##   Zone-coded (CZ_TYPE = "Z")    CZ_FIPS = NWS forecast zone number
##     Only Hurricane, Tropical, and Dust Storm events are kept
##     (Z_KEEP_PATTERN). Each event is first resolved to the counties of its
##     zone (util_storm_zone_resolve.R: NWS zone-county file, then the lookup
##     file for old zones) and then joined to sensors by state + county.
##     Events that cannot be resolved are dropped and counted.
##
##   Marine (CZ_TYPE = "M")        dropped.
##
## Processing steps:
##   1. Read raw Storm Events details CSV files (one per year from NCEI download)
##   2. Convert event times from local standard time to UTC
##   3. Resolve every event to counties (two paths above)
##   4. Join to sensor locations (AirNow AQSID + AQSID_rev) by county
##   5. Expand each sensor x event to hourly datetimes spanning the event
##   6. Write a single sensor-level hourly file for Stage 6 integration
##
## Event types used in classification (MS1 §2.3):
##   Flood variants, Tornado (county-coded); Hurricane, Tropical Storm,
##   Dust Storm (zone-coded)
##   (Heat is classified via GridMET tmmx, not Storm Events records)
##
## Input:   DIR_STORM_RAW/*.csv.gz  (raw NCEI annual files)
##          FILE_AIRNOW_USATTRS_SHP (sensor → county FIPS, from util_sensor_us_attrs.R)
##          FILE_AIRNOW_LOCS        (sensor coordinates + AQSID_rev)
##          FILE_ZONE_COUNTY        (NWS zone-county correlation file)
##          FILE_ZONE_LEGACY        (lookup for zones absent from the NWS file)
## Output:  FILE_STORM_OUT          (sensor-level hourly storm event records)
##
## Follows:  acq_noaa_storm_events_download.py
## MS1 pipeline — Stage 4
## Precedes: stage06_master_hourly_integration.R
## =============================================================================

rm(list = ls())

library(dplyr)
library(readr)
library(lubridate)
library(sf)
library(data.table)
library(stringr)

source("config.R")
source("util_storm_zone_resolve.R")

## ---------------------------------------------------------------------------
## 1. Configuration
## ---------------------------------------------------------------------------

## County-coded events: all event types are retained without pre-classification.
## EVENT_TYPE is the raw NOAA string (e.g. "Flood", "Tornado",
## "Thunderstorm Wind"). Stage07_08 uses grepl() on EVENT_TYPE to assign hazard
## categories. County-coded types that are not used for classification
## (e.g. Thunderstorm Wind, Hail) still carry an EVENT_ID, which stage07_08
## uses to exclude those hours from the baseline pool.
##
## Zone-coded events: only the types below are retained. Other zone-coded
## types (Drought, Heat, Dense Fog, High Wind, Winter Storm, Coastal Flood,
## etc.) are not used in MS1 classification and are excluded so that they do
## not remove baseline hours.
Z_KEEP_PATTERN <- paste(
  "Hurricane", "Tropical", "Dust Storm",
  sep = "|"
)

## ---------------------------------------------------------------------------
## 2. Load sensor locations and county FIPS codes
## ---------------------------------------------------------------------------
message("Loading sensor US attributes ...")
usattrs <- st_read(FILE_AIRNOW_USATTRS_SHP, quiet = TRUE) |>
  st_drop_geometry() |>
  as.data.table()

## Normalise AQSID (remove leading apostrophes, pad to 9 digits)
normalize_aqsid <- function(x) {
  x <- stringr::str_trim(as.character(x))
  x <- dplyr::na_if(x, "'NA"); x <- dplyr::na_if(x, "NA"); x <- dplyr::na_if(x, "")
  x <- stringr::str_replace(x, "^'+", "")
  is_num <- !is.na(x) & stringr::str_detect(x, "^[0-9]+$")
  x[is_num] <- ifelse(nchar(x[is_num]) < 9,
                      stringr::str_pad(x[is_num], 9, "left", "0"),
                      x[is_num])
  x
}

## Load AQSID_rev mapping from location file
locs <- read_csv(FILE_AIRNOW_LOCS,
                 col_types = cols(AQSID = col_character(), AQSID_rev = col_character(),
                                  .default = col_skip())) |>
  mutate(AQSID     = normalize_aqsid(AQSID),
         AQSID_rev = normalize_aqsid(AQSID_rev)) |>
  distinct(AQSID, AQSID_rev)

## Sensor lookup by county: STATE_FIPS + CNTY_FIPS (county FIPS code).
## Both event paths join to this one table.
sensor_base <- usattrs |>
  mutate(AQSID      = normalize_aqsid(AQSID),
         STATE_FIPS = as.integer(statefp),
         CNTY_FIPS  = as.integer(cntyfp)) |>
  filter(!is.na(STATE_FIPS)) |>
  left_join(locs, by = "AQSID") |>
  distinct()

sensor_county <- sensor_base |>
  filter(!is.na(CNTY_FIPS)) |>
  select(AQSID, AQSID_rev, STATE_FIPS, CNTY_FIPS) |>
  distinct()

message(sprintf("Sensor-county lookup: %d sensors across %d unique state-county pairs.",
                n_distinct(sensor_county$AQSID),
                nrow(distinct(sensor_county, STATE_FIPS, CNTY_FIPS))))

## ---------------------------------------------------------------------------
## 3. Read raw Storm Events files and convert times to UTC
## ---------------------------------------------------------------------------
raw_files <- list.files(DIR_STORM_RAW, pattern = "StormEvents_details.*\\.csv\\.gz",
                        full.names = TRUE)

if (length(raw_files) == 0) {
  stop("No raw Storm Events files found in: ", DIR_STORM_RAW,
       "\nRun acq_noaa_storm_events_download.py first.")
}

message(sprintf("Reading %d raw Storm Events files ...", length(raw_files)))

events_raw <- rbindlist(lapply(raw_files, function(f) {
  message("  ", basename(f))
  dt <- fread(f, select = c("EVENT_ID", "STATE_FIPS", "CZ_FIPS", "CZ_TYPE",
                              "CZ_TIMEZONE", "EVENT_TYPE",
                              "BEGIN_DATE_TIME", "END_DATE_TIME"),
              na.strings = c("", "NA"))
  dt
}), use.names = TRUE, fill = TRUE)

## Keep county-coded ("C") and zone-coded ("Z") events.
## Drop marine events (CZ_TYPE "M") — no land-based AirNow sensors offshore.
events_land <- events_raw[CZ_TYPE %in% c("C", "Z")]

## Convert code columns to integer.
events_land[, STATE_FIPS := as.integer(STATE_FIPS)]
events_land[, CZ_FIPS    := as.integer(CZ_FIPS)]

## Parse event datetimes using the original parse_local_noaa approach.
## Storm Events times are in local standard time; offset_hr converts to UTC.
parse_local_noaa <- function(x) {
  x <- stringr::str_trim(x)
  x <- dplyr::na_if(x, "")
  lubridate::parse_date_time(
    x,
    orders = c(
      "m/d/Y HMS", "m/d/Y HM",
      "m/d/y HMS", "m/d/y HM",
      "d-b-Y HMS", "d-b-Y HM",
      "d-b-y HMS", "d-b-y HM"
    ),
    tz = "UTC",
    quiet = TRUE
  )
}

events_land[, offset_hr := fcase(
  CZ_TIMEZONE == "GST10",  10,
  CZ_TIMEZONE == "SST-11", 11,
  default = suppressWarnings(as.numeric(stringr::str_extract(CZ_TIMEZONE, "(?<=-)[0-9]+")))
)]
events_land[, offset_hr := fcoalesce(offset_hr, 0)]

events_land[, begin_dt := with_tz(parse_local_noaa(BEGIN_DATE_TIME) + lubridate::hours(offset_hr), "UTC")]
events_land[, end_dt   := with_tz(parse_local_noaa(END_DATE_TIME)   + lubridate::hours(offset_hr), "UTC")]

## Drop rows with unparseable times
events_land <- events_land[!is.na(begin_dt) & !is.na(end_dt)]

## Filter to study period
study_start_dt <- as.POSIXct(paste0(STUDY_START, " 00:00:00"), tz = "UTC")
study_end_dt   <- as.POSIXct(paste0(STUDY_END,   " 23:59:59"), tz = "UTC")

events_land <- events_land[begin_dt <= study_end_dt & end_dt >= study_start_dt]

## Keep the file order so that events are processed in the same order as before
events_land[, ev_order := .I]

message(sprintf("Retained %d land events (C+Z) within study period.",
                nrow(events_land)))

## ---------------------------------------------------------------------------
## 4. Resolve every event to counties — two separate paths
## ---------------------------------------------------------------------------

## ---- 4a. County-coded events: CZ_FIPS is the county FIPS code ----
events_c <- events_land[
  CZ_TYPE == "C",
  .(EVENT_ID, EVENT_TYPE, STATE_FIPS, CNTY_FIPS = CZ_FIPS, begin_dt, end_dt, ev_order)
]
events_c[, path := "county-coded"]

## ---- 4b. Zone-coded events: CZ_FIPS is the NWS zone number ----
events_z <- events_land[
  CZ_TYPE == "Z" & grepl(Z_KEEP_PATTERN, EVENT_TYPE, ignore.case = TRUE),
  .(EVENT_ID, EVENT_TYPE, STATE_FIPS, ZONE_NUM = CZ_FIPS, begin_dt, end_dt, ev_order)
]

zone_county <- read_zone_county(FILE_ZONE_COUNTY, FILE_ZONE_LEGACY)
z_res       <- resolve_zone_events(events_z, zone_county)

events_zc <- z_res$events[
  , .(EVENT_ID, EVENT_TYPE, STATE_FIPS, CNTY_FIPS, begin_dt, end_dt, ev_order)
]
events_zc[, path := "zone-coded"]

## ---- 4c. Both paths now carry a county code ----
events_cnty <- rbindlist(list(events_c, events_zc), use.names = TRUE)
events_cnty <- unique(events_cnty, by = c("EVENT_ID", "STATE_FIPS", "CNTY_FIPS"))

## Keep event x county rows whose county has at least one sensor
sensor_cnty_keys <- unique(sensor_county[, .(STATE_FIPS, CNTY_FIPS)])
events_hit <- events_cnty[sensor_cnty_keys, on = .(STATE_FIPS, CNTY_FIPS), nomatch = NULL]
setorder(events_hit, ev_order, CNTY_FIPS)

message("Storm Events by path:")
message(sprintf("  County-coded: %d events read | %d reach a sensor county",
                uniqueN(events_c$EVENT_ID),
                uniqueN(events_hit[path == "county-coded"]$EVENT_ID)))
message(sprintf("  Zone-coded (%s): %d events read | %d reach a sensor county",
                Z_KEEP_PATTERN, uniqueN(events_z$EVENT_ID),
                uniqueN(events_hit[path == "zone-coded"]$EVENT_ID)))
message("  Zone-coded events by outcome:")
for (k in seq_len(nrow(z_res$log)))
  message(sprintf("    %-58s %6d", z_res$log$outcome[k], z_res$log$events[k]))

## ---------------------------------------------------------------------------
## 5. Expand events to hourly datetimes and join to sensors
## ---------------------------------------------------------------------------
message("Expanding events to hourly records and joining to sensors ...")

## For each event x county, generate every hour in [begin_dt, end_dt]
## capped to the study period, then cross-join to sensors in that county.
all_rows <- vector("list", nrow(events_hit))

for (i in seq_len(nrow(events_hit))) {
  ev <- events_hit[i]

  sensors_here <- sensor_county[STATE_FIPS == ev$STATE_FIPS & CNTY_FIPS == ev$CNTY_FIPS]
  if (nrow(sensors_here) == 0L) next

  ## Floor to hour so seq() produces clean :00 datetimes that align with
  ## the PM2.5 hourly grid.  begin_dt may have non-zero minutes (e.g. 11:04
  ## local → 16:04 UTC); seq() without flooring would give 16:04, 17:04 ...
  t_start <- floor_date(max(ev$begin_dt, study_start_dt), "hour")
  t_end   <- floor_date(min(ev$end_dt,   study_end_dt),   "hour")
  if (t_start > t_end) next

  hours_seq <- seq(t_start, t_end, by = "hour")

  ## Cross-join: every sensor × every hour in event window
  rows <- CJ(AQSID    = sensors_here$AQSID,
             AQSID_rev = NA_character_,
             datetime  = hours_seq)
  rows[sensors_here, on = "AQSID", AQSID_rev := i.AQSID_rev]

  rows[, `:=`(
    EVENT_ID   = ev$EVENT_ID,
    EVENT_TYPE = ev$EVENT_TYPE
  )]
  all_rows[[i]] <- rows
}

out <- rbindlist(Filter(Negate(is.null), all_rows), use.names = TRUE, fill = TRUE)

if (nrow(out) == 0L) stop("No sensor-event matches produced. Check county FIPS alignment.")

## Collapse multiple events at the same sensor × hour.
## EVENT_ID retains all retained event types (for baseline exclusion in
## stage07_08). EVENT_TYPE is the raw NOAA string; stage07_08 uses grepl() on
## EVENT_TYPE to assign hazard categories without any pre-classification here.
out_ag <- out[
  ,
  .(EVENT_ID   = paste(unique(EVENT_ID),   collapse = "; "),
    EVENT_TYPE = paste(unique(EVENT_TYPE), collapse = "; ")),
  by = .(AQSID, AQSID_rev, datetime)
]

setorder(out_ag, AQSID, datetime)

message(sprintf("Output: %d sensor × hour records covering %d unique sensors.",
                nrow(out_ag), n_distinct(out_ag$AQSID)))

## ---------------------------------------------------------------------------
## 6. Write output
## ---------------------------------------------------------------------------
fwrite(out_ag, FILE_STORM_OUT, compress = "gzip")
message("Wrote: ", FILE_STORM_OUT)
