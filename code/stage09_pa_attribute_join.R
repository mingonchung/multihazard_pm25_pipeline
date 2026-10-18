## =============================================================================
## stage09_pa_attribute_join.R
## MS1 pipeline — Stage 9 (PurpleAir)
##
## Appends terrain, US spatial attributes (urban/rural, state, county), and
## calendar fields to the daily classified output from Stage 8. Filters to
## US sensors (drops any Canadian sensors without a state FIPS assignment).
##
## Aligned with stage09_attribute_join.R (AirNow); key differences:
##   - STATION replaces AQSID
##   - Uses FILE_PA_{TYPE}_TERRAIN_SHP for terrain attributes
##     (produced by util_pa_terrain_extract.R)
##   - Uses FILE_PA_{TYPE}_USATTRS_SHP for urban/rural + state/county FIPS
##     (produced by util_pa_sensor_location_shp.R)
##
## Day definition: `date` is the LOCAL STANDARD date (midnight to midnight,
## UTC + GMT_Offset, no daylight saving) set in Stage 6; year, month, and dow
## are taken from it. GMT_Offset is kept next to the date.
##
## Usage:
##   Rscript stage09_pa_attribute_join.R outdoor
##   Rscript stage09_pa_attribute_join.R indoor
##
## Input:   purpleair_{type}_daily_classified.csv.gz  (Stage 8 output)
## Output:  purpleair_{type}_daily_classified_US.csv.gz  (DIR_MERGE)
## =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(sf)
  library(stringr)
})

source("config_pa.R")

## ---------------------------------------------------------------------------
## 1. Parse argument
## ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
TYPE <- if (length(args) >= 1) tolower(trimws(args[1])) else stop(
  "Usage: Rscript stage09_pa_attribute_join.R <outdoor|indoor>"
)
if (!TYPE %in% c("outdoor", "indoor"))
  stop("type must be 'outdoor' or 'indoor'")

infile_classified <- if (TYPE == "outdoor")
  FILE_PA_DAILY_CLASSIFIED_OUTDOOR else FILE_PA_DAILY_CLASSIFIED_INDOOR
outfile <- if (TYPE == "outdoor")
  FILE_PA_DAILY_INTEGRATED_OUTDOOR else FILE_PA_DAILY_INTEGRATED_INDOOR
usattrs_shp <- pa_loc_shp_file(TYPE, attrs = TRUE)
terrain_shp <- pa_terrain_shp_file(TYPE)

metric_cols <- PA_METRIC_COLS

message(sprintf("Stage 09 PA | type: %s", TYPE))

## ---------------------------------------------------------------------------
## 2. Helpers
## ---------------------------------------------------------------------------
## Note: na_if() is dplyr — use base R to avoid adding a dplyr dependency.
normalize_station <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x[x %in% c("'NA", "NA", "")] <- NA_character_
  str_replace(x, "^'+", "")
}

## ---------------------------------------------------------------------------
## 3. Read Stage 8 output
## ---------------------------------------------------------------------------
if (!file.exists(infile_classified))
  stop("Missing Stage 8 output: ", infile_classified,
       "\nRun stage07_08_pa_daily_classification.R first.")

message("Reading: ", infile_classified)
daily <- fread(infile_classified, na.strings = c("", "NA"))
daily[, date    := as.Date(date)]
daily[, STATION := normalize_station(STATION)]
daily[, STATION_join := STATION]   # join key (no apostrophe prefix for PA)
message("Sensor-days loaded: ", nrow(daily))

## ---------------------------------------------------------------------------
## 4. Load terrain attributes
## ---------------------------------------------------------------------------
if (!file.exists(terrain_shp))
  stop("Missing terrain shp: ", terrain_shp,
       "\nRun util_pa_terrain_extract.R first.")

terrain <- st_read(terrain_shp, quiet = TRUE) |>
  st_transform(4326) |>
  st_drop_geometry() |>
  as.data.table()

## Normalise STATION column
if (!"STATION" %in% names(terrain)) {
  st_col <- grep("^STATION", names(terrain), ignore.case = TRUE, value = TRUE)[1]
  if (!is.na(st_col)) setnames(terrain, st_col, "STATION")
}
terrain[, STATION_join := normalize_station(STATION)]

need_terr <- intersect(
  c("STATION_join", "elev_m", "slope_deg", "aspect_deg"),
  names(terrain)
)
terrain <- terrain[, ..need_terr]
terrain <- unique(terrain, by = "STATION_join", fromLast = FALSE)
message("Terrain attrs loaded: ", nrow(terrain), " unique sensors")

## ---------------------------------------------------------------------------
## 5. Load US spatial attributes
## ---------------------------------------------------------------------------
if (!file.exists(usattrs_shp))
  stop("Missing us_attrs shp: ", usattrs_shp,
       "\nRun util_pa_sensor_location_shp.R first.")

usattrs <- st_read(usattrs_shp, quiet = TRUE) |>
  st_transform(4326) |>
  st_drop_geometry() |>
  as.data.table()

## Normalise STATION column (shp may truncate to 10 chars)
if (!"STATION" %in% names(usattrs)) {
  st_col <- grep("^STATION", names(usattrs), ignore.case = TRUE, value = TRUE)[1]
  if (!is.na(st_col)) setnames(usattrs, st_col, "STATION")
}
usattrs[, STATION_join := normalize_station(STATION)]

need_us <- intersect(
  c("STATION_join", "urban", "ua_name", "statefp", "stusps", "st_name",
    "cntyfp", "cnty_name", "cnty_geoid"),
  names(usattrs)
)
usattrs <- usattrs[, ..need_us]
usattrs <- unique(usattrs, by = "STATION_join", fromLast = FALSE)
message("US attrs loaded: ", nrow(usattrs), " unique sensors")

## ---------------------------------------------------------------------------
## 6. Merge attributes
## ---------------------------------------------------------------------------
out <- merge(daily, terrain, by = "STATION_join", all.x = TRUE, sort = FALSE)
out <- merge(out,   usattrs, by = "STATION_join", all.x = TRUE, sort = FALSE)

## Drop non-US sensors (missing statefp = Canadian or unmatched)
##
## Diagnostic: sensors with NA statefp fall into two groups:
##   (a) genuine non-US (Canada) — expected
##   (b) US sensor that failed to match in usattrs_shp (join mismatch) — unexpected
## Report both so join failures can be caught early.
n_before          <- nrow(out)
n_sensors_before  <- uniqueN(out$STATION_join)
n_na_statefp      <- out[is.na(statefp), uniqueN(STATION_join)]

message(sprintf("US filter: %d sensor-days (%d sensors) before drop",
                n_before, n_sensors_before))
message(sprintf("  Sensors with NA statefp (non-US or join mismatch): %d",
                n_na_statefp))

out <- out[!is.na(statefp)]

message(sprintf("  Retained: %d sensor-days (%d sensors)",
                nrow(out), uniqueN(out$STATION_join)))

## ---------------------------------------------------------------------------
## 7. Add calendar fields
## ---------------------------------------------------------------------------
out[, `:=`(
  year  = as.integer(year(date)),
  month = as.integer(month(date)),
  dow   = as.character(wday(date, label = TRUE, abbr = TRUE))
)]

## ---------------------------------------------------------------------------
## 8. Stable column order
## ---------------------------------------------------------------------------
key_cols  <- c("STATION", "STATION_join", "date", "GMT_Offset")
haz_cols  <- c("hazard_cat", "n_hours_pm25")
attr_cols <- c("elev_m", "slope_deg", "aspect_deg",
               "urban", "ua_name", "statefp", "stusps", "st_name",
               "cntyfp", "cnty_name", "cnty_geoid")
cal_cols  <- c("year", "month", "dow")

front <- c(
  intersect(key_cols,    names(out)),
  intersect(haz_cols,    names(out)),
  intersect(metric_cols, names(out)),
  intersect(attr_cols,   names(out)),
  intersect(cal_cols,    names(out))
)
setcolorder(out, c(front, setdiff(names(out), front)))

## ---------------------------------------------------------------------------
## 9. Write output
## ---------------------------------------------------------------------------
fwrite(out, outfile, compress = "gzip")
message("\nTotal sensor-days written: ", nrow(out))
message("Hazard mix:")
print(out[, .N, by = hazard_cat][order(-N)])
message("[DONE] Wrote: ", outfile)
