## =============================================================================
## config_pa.R — PurpleAir pipeline configuration (R)
##
## HOW TO USE:
##   source("config_pa.R")   at the top of every PurpleAir R pipeline script.
##   This automatically sources config.R and inherits all shared constants
##   (ROOT_DIR, shared data dirs, classification parameters, etc.).
##
## SET ONCE: edit config.R for ROOT_DIR; nothing here needs changing.
## =============================================================================

source("config.R")   # inherit ROOT_DIR, all shared paths, and classification params

## -----------------------------------------------------------------------------
## 1. PurpleAir network identifier
## -----------------------------------------------------------------------------
PA_SENSOR_ID <- "STATION"

## -----------------------------------------------------------------------------
## 2. PurpleAir study date range (start of the RSIG PurpleAir archive)
##    Daily records are local standard dates (see config.R, section 3).
## -----------------------------------------------------------------------------
PA_STUDY_START <- as.Date("2020-01-01")
PA_STUDY_END   <- as.Date("2025-12-31")

## -----------------------------------------------------------------------------
## 3. PurpleAir data directories
## -----------------------------------------------------------------------------
DIR_PA_RAW        <- file.path(ROOT_DIR, "purpleair", "raw")
DIR_PA_COMBINED   <- file.path(ROOT_DIR, "purpleair", "combined")
DIR_PA_SHP        <- file.path(ROOT_DIR, "purpleair", "shp")

## -----------------------------------------------------------------------------
## 4. Raw data directory layout
##    /purpleair/raw/{outdoor|indoor}/US/pm25_corrected/  ← hourly .gz files
##    /purpleair/raw/{outdoor|indoor}/CAN/pm25_corrected/ ← optional Canada
##
##    Source of the reported records: EPA RSIG (daily files per region).
##    Alternative: PurpleAir API (acq_pa_download.R) writes per-sensor raw
##    files and one corrected hourly file per year
##    (purpleair_PAAPI_{YEAR}-hourly.gz) into the same pm25_corrected folder,
##    in the column layout that acq_pa_hourly_combine.R reads.
## -----------------------------------------------------------------------------
DIR_PA_RAW_OUTDOOR_US  <- file.path(DIR_PA_RAW, "outdoor", "US",  "pm25_corrected")
DIR_PA_RAW_OUTDOOR_CAN <- file.path(DIR_PA_RAW, "outdoor", "CAN", "pm25_corrected")
DIR_PA_RAW_INDOOR_US   <- file.path(DIR_PA_RAW, "indoor",  "US",  "pm25_corrected")
DIR_PA_RAW_INDOOR_CAN  <- file.path(DIR_PA_RAW, "indoor",  "CAN", "pm25_corrected")

## -----------------------------------------------------------------------------
## 5. Sensor list files
##    Produced by acq_pa_hourly_combine.R AFTER combining — NOT during download.
##    Derived from combined files: unique STATION, lat, lon, elevation.
##    Shared across all years once built; rebuilt each combine run to stay current.
##    Also read by stage06_pa to assign each sensor's local standard-time offset.
## -----------------------------------------------------------------------------
FILE_PA_SENSOR_LIST_OUTDOOR <- file.path(DIR_PA_RAW, "outdoor", "sensor_list_outdoor.csv")
FILE_PA_SENSOR_LIST_INDOOR  <- file.path(DIR_PA_RAW, "indoor",  "sensor_list_indoor.csv")

## Helper: return the correct sensor list path for a given type string
pa_sensor_list_file <- function(type) {
  switch(type,
    outdoor = FILE_PA_SENSOR_LIST_OUTDOOR,
    indoor  = FILE_PA_SENSOR_LIST_INDOOR,
    stop("type must be 'outdoor' or 'indoor'")
  )
}

## -----------------------------------------------------------------------------
## 6. Sensor location shapefiles
##     Base shp  : point locations only (produced by util_pa_sensor_location_shp.R)
##     US-attrs  : base + urban/rural flag, state FIPS, county FIPS
##                 (convention matches FILE_AIRNOW_USATTRS_SHP in config.R)
##     Stage 04 uses the _us_attrs shp for county FIPS lookup (pre-computed once).
## -----------------------------------------------------------------------------
FILE_PA_OUTDOOR_LOC_SHP     <- file.path(DIR_PA_SHP, "purpleair_outdoor_location.shp")
FILE_PA_OUTDOOR_USATTRS_SHP <- file.path(DIR_PA_SHP, "purpleair_outdoor_location_us_attrs.shp")
FILE_PA_INDOOR_LOC_SHP      <- file.path(DIR_PA_SHP, "purpleair_indoor_location.shp")
FILE_PA_INDOOR_USATTRS_SHP  <- file.path(DIR_PA_SHP, "purpleair_indoor_location_us_attrs.shp")

## Helper: return the correct location shp path for a given type + variant
pa_loc_shp_file <- function(type, attrs = FALSE) {
  key <- paste0(type, if (attrs) "_attrs" else "")
  files <- list(
    outdoor       = FILE_PA_OUTDOOR_LOC_SHP,
    outdoor_attrs = FILE_PA_OUTDOOR_USATTRS_SHP,
    indoor        = FILE_PA_INDOOR_LOC_SHP,
    indoor_attrs  = FILE_PA_INDOOR_USATTRS_SHP
  )
  f <- files[[key]]
  if (is.null(f)) stop("Unknown type: ", type, " (must be 'outdoor' or 'indoor')")
  f
}

## -----------------------------------------------------------------------------
## 7. Terrain shapefiles
##     Elevation (m), slope (°), aspect (°) extracted from AWS Terrain Tiles
##     (Mapzen) at zoom level 12 (~20–30 m) via elevatr.
##     Produced by util_pa_terrain_extract.R (one per type).
##     Used by: stage09_pa_attribute_join.R
## -----------------------------------------------------------------------------
FILE_PA_OUTDOOR_TERRAIN_SHP <- file.path(DIR_PA_SHP, "purpleair_outdoor_location_terrain.shp")
FILE_PA_INDOOR_TERRAIN_SHP  <- file.path(DIR_PA_SHP, "purpleair_indoor_location_terrain.shp")

## Helper: return the correct terrain shp path for a given type
pa_terrain_shp_file <- function(type) {
  switch(type,
    outdoor = FILE_PA_OUTDOOR_TERRAIN_SHP,
    indoor  = FILE_PA_INDOOR_TERRAIN_SHP,
    stop("type must be 'outdoor' or 'indoor'")
  )
}

## -----------------------------------------------------------------------------
## 8. Combined hourly files (produced by acq_pa_hourly_combine.R, per UTC year)
##    Template strings; format with sprintf(..., year) where needed.
##    stage06_pa also reads the boundary hours of the neighbouring years' files.
## -----------------------------------------------------------------------------
PA_COMBINED_TPL_OUTDOOR <- file.path(DIR_PA_COMBINED, "purpleair_outdoor_USCAN_%d.csv.gz")
PA_COMBINED_TPL_INDOOR  <- file.path(DIR_PA_COMBINED, "purpleair_indoor_USCAN_%d.csv.gz")

## -----------------------------------------------------------------------------
## 9. GEE extraction output templates (per-year for outdoor; fixed for indoor)
##    These mirror config_pa.py; used when R scripts read GEE CSV outputs.
## -----------------------------------------------------------------------------

## AOD (outdoor: per-year template; indoor: fixed path)
PA_FILE_AOD_047_OUTDOOR_TPL <- file.path(DIR_EXTRACT_CSV, "purpleair_aod_047_%d.csv.gz")
PA_FILE_AOD_055_OUTDOOR_TPL <- file.path(DIR_EXTRACT_CSV, "purpleair_aod_055_%d.csv.gz")
PA_FILE_AOD_047_INDOOR      <- file.path(DIR_EXTRACT_CSV, "purpleair_aod_047_indoor.csv.gz")
PA_FILE_AOD_055_INDOOR      <- file.path(DIR_EXTRACT_CSV, "purpleair_aod_055_indoor.csv.gz")

## GridMET (outdoor: per-year templates; indoor: fixed paths)
PA_FILE_GRIDMET_MAIN_OUTDOOR_TPL <- file.path(DIR_EXTRACT_CSV, "purpleair_gridmet_%d.csv.gz")
PA_FILE_GRIDMET_HIST_OUTDOOR_TPL <- file.path(DIR_EXTRACT_CSV, "purpleair_gridmet_temp_%d.csv.gz")
PA_FILE_GRIDMET_SPEI_OUTDOOR_TPL <- file.path(DIR_EXTRACT_CSV, "purpleair_gridmet_spei_%d.csv.gz")
PA_FILE_GRIDMET_MAIN_INDOOR      <- file.path(DIR_EXTRACT_CSV, "purpleair_gridmet_2020_2025_indoor.csv.gz")
PA_FILE_GRIDMET_HIST_INDOOR      <- file.path(DIR_EXTRACT_CSV, "purpleair_gridmet_temp_1980_2019_indoor.csv.gz")
PA_FILE_GRIDMET_SPEI_INDOOR      <- file.path(DIR_EXTRACT_CSV, "purpleair_gridmet_spei_2020_2025_indoor.csv.gz")

## DayMet (outdoor: per-year templates; indoor: fixed paths)
PA_FILE_DAYMET_MAIN_OUTDOOR_TPL <- file.path(DIR_EXTRACT_CSV, "purpleair_daymet_%d.csv.gz")
PA_FILE_DAYMET_HIST_OUTDOOR_TPL <- file.path(DIR_EXTRACT_CSV, "purpleair_daymet_temp_%d.csv.gz")
PA_FILE_DAYMET_MAIN_INDOOR      <- file.path(DIR_EXTRACT_CSV, "purpleair_daymet_2020_2025_indoor.csv.gz")
PA_FILE_DAYMET_HIST_INDOOR      <- file.path(DIR_EXTRACT_CSV, "purpleair_daymet_temp_1980_2019_indoor.csv.gz")

## -----------------------------------------------------------------------------
## 10. Stage output files
## -----------------------------------------------------------------------------

## Stage 02: smoke/fire sensor assignment
FILE_PA_SMOKE_OUTDOOR <- file.path(DIR_EXTRACT_OUT, "purpleair_outdoor_smoke_fired_2020_25.csv.gz")
FILE_PA_SMOKE_INDOOR  <- file.path(DIR_EXTRACT_OUT, "purpleair_indoor_smoke_fired_2020_25.csv.gz")

## Stage 03: p95 climatology tables
FILE_PA_GRIDMET_P95_OUTDOOR <- file.path(DIR_EXTRACT_CSV, "purpleair_gridmet_tmp_p95.csv.gz")
FILE_PA_GRIDMET_P95_INDOOR  <- file.path(DIR_EXTRACT_CSV, "purpleair_gridmet_tmp_p95_indoor.csv.gz")
FILE_PA_DAYMET_P95_OUTDOOR  <- file.path(DIR_EXTRACT_CSV, "purpleair_daymet_tmp_p95_outdoor.csv.gz")
FILE_PA_DAYMET_P95_INDOOR   <- file.path(DIR_EXTRACT_CSV, "purpleair_daymet_tmp_p95_indoor.csv.gz")

## Stage 04: NOAA Storm Events
FILE_PA_STORM_OUTDOOR <- file.path(DIR_EXTRACT_OUT, "purpleair_outdoor_noaa_storm_events_2020_25.csv.gz")
FILE_PA_STORM_INDOOR  <- file.path(DIR_EXTRACT_OUT, "purpleair_indoor_noaa_storm_events_2020_25.csv.gz")

## Stage 05: GVP volcanic
FILE_PA_GVP_OUTDOOR <- file.path(DIR_EXTRACT_OUT, "purpleair_outdoor_gvp_2020_25.csv.gz")
FILE_PA_GVP_INDOOR  <- file.path(DIR_EXTRACT_OUT, "purpleair_indoor_gvp_2020_25.csv.gz")

## Stage 06: master hourly integration (per local year)
##   Format with sprintf(..., year): e.g. sprintf(PA_FILE_INTEGRATED_OUTDOOR_TPL, 2020)
PA_FILE_INTEGRATED_OUTDOOR_TPL <- file.path(DIR_MERGE, "purpleair_outdoor_integrated_%d.csv.gz")
PA_FILE_INTEGRATED_INDOOR_TPL  <- file.path(DIR_MERGE, "purpleair_indoor_integrated_%d.csv.gz")

## Stage 07/08: daily classified output
FILE_PA_DAILY_CLASSIFIED_OUTDOOR <- file.path(DIR_MERGE, "purpleair_outdoor_daily_classified.csv.gz")
FILE_PA_DAILY_CLASSIFIED_INDOOR  <- file.path(DIR_MERGE, "purpleair_indoor_daily_classified.csv.gz")

## Stage 09: daily classified + US spatial attributes
FILE_PA_DAILY_INTEGRATED_OUTDOOR <- file.path(DIR_MERGE, "purpleair_outdoor_daily_classified_US.csv.gz")
FILE_PA_DAILY_INTEGRATED_INDOOR  <- file.path(DIR_MERGE, "purpleair_indoor_daily_classified_US.csv.gz")

## Helper: return the correct stage output file for a given stage + type
pa_stage_file <- function(stage, type) {
  key <- paste0(stage, "_", type)
  files <- list(
    smoke_outdoor  = FILE_PA_SMOKE_OUTDOOR,
    smoke_indoor   = FILE_PA_SMOKE_INDOOR,
    storm_outdoor  = FILE_PA_STORM_OUTDOOR,
    storm_indoor   = FILE_PA_STORM_INDOOR,
    gvp_outdoor    = FILE_PA_GVP_OUTDOOR,
    gvp_indoor     = FILE_PA_GVP_INDOOR
  )
  f <- files[[key]]
  if (is.null(f)) stop("Unknown stage/type combination: ", key)
  f
}

## -----------------------------------------------------------------------------
## 11. Metric columns for PA daily output
##    Identical to METRIC_COLS in config.R, so both networks share one schema.
##    Smoke/fire fields (density, frd_d_s) are hazard indicators, not metrics.
## -----------------------------------------------------------------------------
PA_METRIC_COLS <- METRIC_COLS
