## =============================================================================
## stage09_attribute_join.R
## MS1 pipeline — Stage 9 (post-classification attribute enrichment)
##
## Appends terrain, US spatial attributes (urban/rural, state, county), and
## calendar fields to the daily classified output from Stage 8.
## Filters to sensors with complete spatial attributes (US only).
##
## Note: elevation, slope, and aspect (terrain_shp) are matching covariates
## used in MS2. They are included in the MS1 output for completeness but are
## not required for hazard classification itself.
##
## Output (written to DIR_MERGE):
##   airnow_daily_classified_US.csv.gz  — all sensor-days + attributes
##                                        (hazard_cat = one of 12 categories,
##                                         "Baseline", or NA = unclassified)
##
## Day definition: `date` is the LOCAL STANDARD date (midnight to midnight,
## UTC + GMT_Offset, no daylight saving) set in Stage 6; year, month, and dow
## are taken from it. GMT_Offset is kept next to the date.
##
## Input:   FILE_DAILY_CLASSIFIED (Stage 8 output)
## Follows: stage07_08_daily_classification.R
## =============================================================================

rm(list = ls())

library(data.table)
library(lubridate)
library(sf)
library(stringr)
library(dplyr)

source("config.R")

## ------------------------- Inputs / outputs ----------------------------------
infile_classified <- FILE_DAILY_CLASSIFIED
terrain_shp       <- FILE_AIRNOW_TERRAIN_SHP
usattrs_shp       <- FILE_AIRNOW_USATTRS_SHP
outfile           <- FILE_DAILY_INTEGRATED

## Metric column order for stable output schema (defined in config.R)
metric_cols <- METRIC_COLS

## ------------------------- Helper functions ----------------------------------
normalize_aqsid <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- dplyr::na_if(x, "'NA")
  x <- dplyr::na_if(x, "NA")
  x <- dplyr::na_if(x, "")
  x <- stringr::str_replace(x, "^'+", "")
  x <- stringr::str_trim(x)
  is_num <- !is.na(x) & stringr::str_detect(x, "^[0-9]+$")
  x[is_num] <- ifelse(
    nchar(x[is_num]) < 9,
    stringr::str_pad(x[is_num], width = 9, side = "left", pad = "0"),
    x[is_num]
  )
  x
}

add_join_key <- function(DT) {
  setDT(DT)
  DT[, AQSID_norm := normalize_aqsid(AQSID)]
  DT[, AQSID_join := AQSID_norm]
  DT[, AQSID := ifelse(is.na(AQSID_norm), NA_character_, paste0("'", AQSID_norm))]
  DT[]
}

collapse_by_key <- function(DT, keycols) {
  setDT(DT)
  DT[, lapply(.SD, function(v) if (is.numeric(v)) mean(v, na.rm = TRUE) else v[1L]),
     by = keycols]
}

## ------------------------- Read Stage 8 output -------------------------------
message("Reading: ", infile_classified)
daily <- fread(infile_classified, na.strings = c("", "NA"))
daily[, date := as.Date(date)]

daily <- add_join_key(daily)

## ------------------------- Load terrain attributes ---------------------------
terrain <- st_read(terrain_shp, quiet = TRUE) |>
  st_transform(4326) |>
  st_drop_geometry()
terrain <- as.data.table(terrain)

if (!"AQSID" %in% names(terrain)) {
  idcol <- names(terrain)[grepl("AQSID", names(terrain), ignore.case = TRUE)][1]
  setnames(terrain, idcol, "AQSID")
}
terrain <- add_join_key(terrain)
need_terr <- intersect(c("AQSID_join", "elev_m", "slope_deg", "aspect_deg"), names(terrain))
terrain <- terrain[, ..need_terr]
terrain <- collapse_by_key(terrain, "AQSID_join")

## ------------------------- Load US spatial attributes -----------------------
usattrs <- st_read(usattrs_shp, quiet = TRUE) |>
  st_transform(4326) |>
  st_drop_geometry()
usattrs <- as.data.table(usattrs)

if (!"AQSID" %in% names(usattrs)) {
  idcol2 <- names(usattrs)[grepl("AQSID", names(usattrs), ignore.case = TRUE)][1]
  setnames(usattrs, idcol2, "AQSID")
}
usattrs <- add_join_key(usattrs)
need_us <- intersect(
  c("AQSID_join", "urban", "ua_name", "statefp", "stusps", "st_name",
    "cntyfp", "cnty_name", "cnty_geoid"),
  names(usattrs)
)
usattrs <- usattrs[, ..need_us]
usattrs <- unique(usattrs, by = "AQSID_join", fromLast = FALSE)

## ------------------------- Merge attributes ----------------------------------
## Left joins: retain all sensor-days from Stage 8.
out <- merge(daily,  terrain, by = "AQSID_join", all.x = TRUE, sort = FALSE)
out <- merge(out,    usattrs, by = "AQSID_join", all.x = TRUE, sort = FALSE)

## Drop non-US sensors (Canada etc.) — identified by missing statefp after the
## spatial join. These sensors have no US state assignment and will never have
## valid MS2 matching covariates.
## US sensors with missing terrain or GridMET attributes are retained. Alaska,
## Hawaii, and Puerto Rico sensors are outside GridMET coverage: their GridMET
## columns (incl. tmmx, tmmx_p95_*, spei*) are NA, and stage07_08 classified
## their Heat categories and Baseline temperature condition with Daymet
## (tmax, tmax_p95_1991_2020). MS2 exact matching will drop sensors naturally
## if required covariates are absent.
##
## Diagnostic: sensors with NA statefp fall into two groups:
##   (a) genuine non-US (Canada) — expected
##   (b) US sensor that failed to match in usattrs_shp (join mismatch) — unexpected
## Report both so join failures can be caught early.
n_before <- nrow(out)
n_sensors_before <- uniqueN(out$AQSID_join)

n_na_statefp      <- out[is.na(statefp), uniqueN(AQSID_join)]
message(sprintf("US filter: %d sensor-days (%d sensors) before drop",
                n_before, n_sensors_before))
message(sprintf("  Sensors with NA statefp (non-US or join mismatch): %d",
                n_na_statefp))

out <- out[!is.na(statefp)]

message(sprintf("  Retained: %d sensor-days (%d sensors)",
                nrow(out), uniqueN(out$AQSID_join)))

## ------------------------- Add calendar fields -------------------------------
out[, `:=`(
  year  = as.integer(year(date)),
  month = as.integer(month(date)),
  dow   = as.character(wday(date, label = TRUE, abbr = TRUE))
)]

## ------------------------- Stable column order -------------------------------
key_cols  <- c("AQSID", "AQSID_join", "date", "GMT_Offset")
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

## ------------------------- Write output --------------------------------------
fwrite(out, outfile, compress = "gzip")

message("Total sensor-days written: ", nrow(out))
message("Hazard mix:")
print(out[, .N, by = hazard_cat][order(-N)])
message("Wrote: ", outfile)
