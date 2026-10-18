## =============================================================================
## util_sensor_location_shp.R
## Utility — AirNow PM2.5 sensor location point shapefile
##
## Writes one point per AQSID that reports PM2.5 in the combined AirNow file
## and has coordinates in the location file. Ported from the author's earlier
## script (2_1_airnow_PM25_cumulative.R, lines 26–47) with config.R paths.
##
## Rules
##   - Sensor universe: unique AQSIDs of FILE_AIRNOW_PM25 (first SiteName kept).
##   - Coordinates: FILE_AIRNOW_LOCS (lat_numeric, long_numeric). IDs are matched
##     after normalisation (leading apostrophe removed, numeric IDs padded to
##     9 digits), as in the stage scripts. Where the location file lists an
##     AQSID more than once, the first row with coordinates is used (the same
##     rule as stage02 and stage06).
##   - The AQSID written is the string in the PM2.5 file.
##   - No spatial filter here; US/Canada and US-only filters are applied later
##     (stage06, util_sensor_us_attrs.R).
##
## Re-run behaviour
##   If FILE_AIRNOW_LOC_SHP already exists, the script does NOT overwrite it:
##   it compares the points it would write with the existing file and reports
##   the differences. Run with the argument --overwrite to replace the file.
##   (The GEE extractions were made from the existing points; replacing the
##   file only matters if extractions are re-run.)
##
## Input:   FILE_AIRNOW_PM25 (acq_airnow_hourly_combine.R)
##          FILE_AIRNOW_LOCS (acq_airnow_station_metadata.R, finalised by hand)
## Output:  FILE_AIRNOW_LOC_SHP — fields AQSID, SiteName, lat, lon (WGS84)
## Used by: GEE extractions (uploaded as the sensor asset), util_terrain_extract.R,
##          util_sensor_us_attrs.R
##
## Usage:   Rscript util_sensor_location_shp.R [--overwrite]
## =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
})

source("config.R")

overwrite <- "--overwrite" %in% commandArgs(trailingOnly = TRUE)

## Same rule as normalize_aqsid() in the stage scripts
normalize_aqsid <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("'NA", "NA", "")] <- NA_character_
  x <- trimws(sub("^'+", "", x))
  num <- !is.na(x) & grepl("^[0-9]+$", x) & nchar(x) < 9
  x[num] <- paste0(strrep("0", 9 - nchar(x[num])), x[num])
  x
}

## ---------------------------------------------------------------------------
## 1. Sensors that report PM2.5
## ---------------------------------------------------------------------------
message("Reading AQSID and SiteName from: ", FILE_AIRNOW_PM25)
pm <- fread(FILE_AIRNOW_PM25, select = c("AQSID", "SiteName"),
            colClasses = list(character = c("AQSID", "SiteName")))
pm <- unique(pm, by = "AQSID")
pm[, id := normalize_aqsid(AQSID)]
pm <- pm[!is.na(id)]
pm <- unique(pm, by = "id")
message(sprintf("  %d PM2.5 sensor IDs", nrow(pm)))

## ---------------------------------------------------------------------------
## 2. Coordinates from the location file
## ---------------------------------------------------------------------------
loc <- fread(FILE_AIRNOW_LOCS, colClasses = list(character = "AQSID"))
loc[, `:=`(id  = normalize_aqsid(AQSID),
           lat = suppressWarnings(as.numeric(lat_numeric)),
           lon = suppressWarnings(as.numeric(long_numeric)))]
n_rep <- loc[!is.na(id), sum(duplicated(id))]
loc <- loc[!is.na(id) & !is.na(lat) & !is.na(lon)]
loc <- unique(loc, by = "id")                      # first row with coordinates
message(sprintf("  Location file: %d IDs with coordinates (%d repeated ID rows in the file)",
                nrow(loc), n_rep))

pts <- merge(pm[, .(id, AQSID, SiteName)], loc[, .(id, lat, lon)], by = "id")
n_nocoord <- nrow(pm) - nrow(pts)
message(sprintf("  Points: %d | PM2.5 IDs without coordinates (not written): %d",
                nrow(pts), n_nocoord))
setorder(pts, id)

pts_sf <- st_as_sf(pts[, .(AQSID, SiteName, lat, lon)],
                   coords = c("lon", "lat"), crs = 4326, remove = FALSE)

## ---------------------------------------------------------------------------
## 3. Write, or compare with the existing file
## ---------------------------------------------------------------------------
if (file.exists(FILE_AIRNOW_LOC_SHP) && !overwrite) {
  old <- st_drop_geometry(st_read(FILE_AIRNOW_LOC_SHP, quiet = TRUE))
  setDT(old)
  idcol <- names(old)[grepl("AQSID", names(old), ignore.case = TRUE)][1]
  old[, id := normalize_aqsid(get(idcol))]
  cc <- st_coordinates(st_read(FILE_AIRNOW_LOC_SHP, quiet = TRUE))
  old[, `:=`(lon_old = cc[, 1], lat_old = cc[, 2])]
  cmp <- merge(pts[, .(id, lat, lon)], old[, .(id, lat_old, lon_old)], by = "id", all = TRUE)
  moved <- cmp[!is.na(lat) & !is.na(lat_old) &
                 (abs(lat - lat_old) > 1e-5 | abs(lon - lon_old) > 1e-5)]
  message("\nExisting file kept: ", FILE_AIRNOW_LOC_SHP)
  message(sprintf("  points existing: %d | would be written: %d", nrow(old), nrow(pts)))
  message(sprintf("  repeated IDs in existing file: %d", sum(duplicated(old$id))))
  message(sprintf("  IDs only in existing file: %d | only in new: %d | coordinates differ (> 1e-5 deg): %d",
                  cmp[is.na(lat), uniqueN(id)], cmp[is.na(lat_old), uniqueN(id)], uniqueN(moved$id)))
  if (nrow(moved)) print(head(moved, 10))
  message("Run with --overwrite to replace the file.")
} else {
  dir.create(dirname(FILE_AIRNOW_LOC_SHP), recursive = TRUE, showWarnings = FALSE)
  st_write(pts_sf, FILE_AIRNOW_LOC_SHP, delete_layer = TRUE, quiet = TRUE)
  message("Wrote: ", FILE_AIRNOW_LOC_SHP, " (", nrow(pts_sf), " points)")
}
