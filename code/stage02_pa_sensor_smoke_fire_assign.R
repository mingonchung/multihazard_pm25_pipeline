## =============================================================================
## stage02_pa_sensor_smoke_fire_assign.R
## Assign HMS smoke plume and FIRED fire detections to PurpleAir sensor
## locations for the PA study period (2020–2025).
##
## Stage 01 (HMS×FIRED spatial join) does NOT need to be re-run for PurpleAir.
## It produced daily hms_smoke_fired_YYYYMMDD.shp files that are spatial data,
## independent of monitoring network. This stage simply reads those existing
## shapefiles and assigns them to PurpleAir sensor point locations.
##
## Processing:
##   1. Load PA sensor list (lat/lon → sf points)
##   2. For each day in PA study period (2020-2025), read the pre-computed
##      hms_smoke_fired shapefiles from DIR_HMS_SMOKE/fired/
##   3. Spatial intersection: flag each sensor × day with smoke density class
##      and FIRED fire event IDs
##   4. Write a single sensor-day-level output file for Stage 6 integration
##
## Output columns (matches AirNow stage02 schema):
##   STATION, date, density (Light/Medium/Heavy or NA), frd_d_s (FIRED IDs or NA)
##
##   - Only days where at least one sensor intersects a smoke polygon are output.
##   - Sensors not under any smoke polygon on a hit day receive NA/NA.
##   - density uses the same capitalisation as stage01 shapefiles (Heavy/Medium/Light).
##   - frd_d_s is a semicolon-separated character string of FIRED event IDs, or NA.
##
## Usage:
##   Rscript stage02_pa_sensor_smoke_fire_assign.R outdoor
##   Rscript stage02_pa_sensor_smoke_fire_assign.R indoor
##
## Stage 01 must have been run first (AirNow pipeline) to produce shapefiles.
## Both outdoor and indoor sensors receive identical spatial smoke treatment.
##
## Sensor list schema (sensor_list_{type}.csv): STATION, lat, lon, elevation
## =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(data.table)
  library(lubridate)
  library(stringr)
})

sf_use_s2(FALSE)

source("config_pa.R")

## ---------------------------------------------------------------------------
## 1. Parse argument
## ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
TYPE <- if (length(args) >= 1) tolower(trimws(args[1])) else stop(
  "Usage: Rscript stage02_pa_sensor_smoke_fire_assign.R <outdoor|indoor>"
)
if (!TYPE %in% c("outdoor", "indoor"))
  stop("type must be 'outdoor' or 'indoor'")

OUT_FILE <- pa_stage_file("smoke", TYPE)

if (file.exists(OUT_FILE)) {
  message("Output already exists, skipping: ", OUT_FILE)
  quit(save = "no", status = 0)
}

message(sprintf("Stage 02 PA | type: %s | period: %s to %s",
                TYPE, PA_STUDY_START, PA_STUDY_END))

## ---------------------------------------------------------------------------
## 2. Load PA sensor locations and build sf point layer
##
## sensor_list_{type}.csv schema: STATION, lat, lon, elevation
## ---------------------------------------------------------------------------
sensor_file <- pa_sensor_list_file(TYPE)
if (!file.exists(sensor_file))
  stop("Sensor list not found: ", sensor_file,
       "\nRun acq_pa_hourly_combine.R first.")

sensors_dt <- fread(sensor_file, select = c("STATION", "lat", "lon"))
sensors_dt[, STATION := as.character(STATION)]
sensors_dt <- sensors_dt[!is.na(lat) & !is.na(lon)]
sensors_dt <- unique(sensors_dt, by = "STATION")

sensors_sf <- st_as_sf(sensors_dt,
                       coords = c("lon", "lat"),
                       crs    = 4326,
                       remove = FALSE)

message(sprintf("Loaded %d %s sensors.", nrow(sensors_sf), TYPE))

## ---------------------------------------------------------------------------
## 3. Locate pre-computed HMS×FIRED shapefiles (from Stage 01)
##
## Stage 01 writes: DIR_HMS_SMOKE/fired/hms_smoke_fired_YYYYMMDD.shp
## Shapefile columns used here:
##   Density  — "Heavy" / "Medium" / "Light" (capitalised, from HMS)
##   frd_d_s  — semicolon-separated FIRED event IDs (character), or NA
## ---------------------------------------------------------------------------
fired_dir <- file.path(DIR_HMS_SMOKE, "fired")

if (!dir.exists(fired_dir))
  stop("HMS×FIRED shapefile directory not found: ", fired_dir,
       "\nRun stage01_hms_fired_join.R (AirNow pipeline) first.")

shp_files <- list.files(fired_dir,
                        pattern   = "^hms_smoke_fired_\\d{8}\\.shp$",
                        full.names = TRUE)

if (length(shp_files) == 0L)
  stop("No hms_smoke_fired shapefiles found in: ", fired_dir)

## Filter shapefiles to PA study period only (2020–2025)
shp_dates <- as.Date(str_extract(basename(shp_files), "\\d{8}"), "%Y%m%d")
in_period <- !is.na(shp_dates) &
             shp_dates >= PA_STUDY_START &
             shp_dates <= PA_STUDY_END
shp_files <- shp_files[in_period]
shp_dates <- shp_dates[in_period]

message(sprintf("Found %d daily HMS×FIRED shapefiles in study period.",
                length(shp_files)))

## ---------------------------------------------------------------------------
## 4. Spatial assignment loop: sensor × day
##
## Logic mirrors stage02_sensor_smoke_fire_assign.R (AirNow):
##   - Sparse st_intersects; only sensors that hit at least one polygon processed
##   - density arrays pre-filled with NA_character_
##   - frd_d_s: semicolon-separated FIRED IDs from the winning-density polygon(s)
##   - Days with zero sensor hits are skipped entirely (no rows written)
## ---------------------------------------------------------------------------
all_rows <- vector("list", length(shp_files))

for (i in seq_along(shp_files)) {

  this_date <- shp_dates[i]

  smoke_sf <- tryCatch(
    st_read(shp_files[i], quiet = TRUE),
    error = function(e) {
      warning(sprintf("%s: could not read shapefile — %s",
                      this_date, conditionMessage(e)))
      NULL
    }
  )

  if (is.null(smoke_sf) || nrow(smoke_sf) == 0L) next

  ## Align CRS
  if (!identical(st_crs(smoke_sf), st_crs(sensors_sf)))
    smoke_sf <- st_transform(smoke_sf, crs = st_crs(sensors_sf))

  smoke_sf <- st_make_valid(smoke_sf)

  ## Sparse point-in-polygon: sensors (rows) × polygons (cols)
  intr    <- st_intersects(sensors_sf, smoke_sf, sparse = TRUE)
  hit_idx <- which(lengths(intr) > 0L)
  if (length(hit_idx) == 0L) next

  ## Pre-fill output arrays with NA for all sensors
  density <- rep(NA_character_, nrow(sensors_sf))
  frd     <- rep(NA_character_, nrow(sensors_sf))

  for (j in hit_idx) {

    hits <- intr[[j]]
    sub  <- smoke_sf[hits, ]

    ## Density priority: Heavy > Medium > Light (same as AirNow)
    dens <- dplyr::case_when(
      "Heavy"  %in% sub$Density ~ "Heavy",
      "Medium" %in% sub$Density ~ "Medium",
      "Light"  %in% sub$Density ~ "Light",
      TRUE                       ~ NA_character_
    )

    ## frd_d_s: take values from polygons matching the winning density only
    ## (same logic as AirNow stage02; frd_d_s is already a character string
    ##  of FIRED event IDs produced by stage01)
    raw_vals   <- sub$frd_d_s[sub$Density == dens]
    clean_vals <- raw_vals[!is.na(raw_vals)]

    if (length(clean_vals) == 0L) {
      fval <- NA_character_
    } else {
      txts <- format(clean_vals, scientific = FALSE, trim = TRUE)
      fval <- paste(sort(unique(txts)), collapse = "; ")
    }

    density[j] <- dens
    frd[j]     <- fval
  }

  ## Build daily table — one row per sensor location (all sensors, hit or not)
  ## Non-hit sensors remain NA/NA; matches AirNow output schema exactly
  tbl <- sensors_sf %>%
    st_set_geometry(NULL) %>%
    transmute(
      date    = this_date,
      STATION = STATION,
      density = density,
      frd_d_s = frd
    )

  all_rows[[i]] <- tbl

  if (i %% 100 == 0 || i == length(shp_files))
    message(sprintf("  [%d / %d]  %s  — %d sensor locations hit",
                    i, length(shp_files), this_date, length(hit_idx)))
}

## ---------------------------------------------------------------------------
## 5. Combine, deduplicate, and write
## ---------------------------------------------------------------------------
out <- bind_rows(Filter(Negate(is.null), all_rows))

if (nrow(out) == 0L)
  stop("No sensor-smoke intersections produced. ",
       "Check CRS consistency between shapefiles and sensor coordinates.")

## Keep highest-density row when a sensor appears twice for the same date
density_rank <- c("Heavy" = 3L, "Medium" = 2L, "Light" = 1L)
out <- out %>%
  mutate(.rank = dplyr::coalesce(density_rank[density], 0L)) %>%
  arrange(STATION, date, desc(.rank)) %>%
  distinct(STATION, date, .keep_all = TRUE) %>%
  select(-.rank)

message(sprintf(
  "Output: %d sensor \u00d7 date records | %d unique STATIONs | %d unique dates.",
  nrow(out), dplyr::n_distinct(out$STATION), dplyr::n_distinct(out$date)
))

dir.create(dirname(OUT_FILE), showWarnings = FALSE, recursive = TRUE)
write.csv(out, gzfile(OUT_FILE), row.names = FALSE)
message("\u2713 Wrote: ", OUT_FILE)
