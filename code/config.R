## =============================================================================
## config.R — shared configuration for all AirNow pipeline R scripts
##
## HOW TO USE:
##   source("config.R")   at the top of every R pipeline script.
##
## SET ONCE:
##   Change ROOT_DIR to your local project root. All other paths are derived.
## =============================================================================

## -----------------------------------------------------------------------------
## 1. Root directory (CHANGE THIS for your environment)
## -----------------------------------------------------------------------------
ROOT_DIR <- "/data001/projects/chungmin"

## -----------------------------------------------------------------------------
## 2. Derived directory structure
## -----------------------------------------------------------------------------

## --- Input data roots ---
DIR_AIRNOW_HOURLY  <- file.path(ROOT_DIR, "aqdata", "airnow")
DIR_AIRNOW_COMBINE <- file.path(ROOT_DIR, "aqdata", "airnow", "combine")
DIR_HMS_SMOKE      <- file.path(ROOT_DIR, "aqdata", "hms", "smoke_polygons")
DIR_HMS_FIRE       <- file.path(ROOT_DIR, "aqdata", "hms", "fire_points")
DIR_FIRED          <- file.path(ROOT_DIR, "aqdata", "fired", "usa")
DIR_GVP            <- file.path(ROOT_DIR, "aqdata", "gvp")
DIR_USCAN          <- file.path(ROOT_DIR, "aqdata", "uscan")
DIR_URBAN          <- file.path(ROOT_DIR, "aqdata", "urban")

## --- Intermediate / extracted data ---
DIR_EXTRACT_CSV    <- file.path(ROOT_DIR, "extraction", "output", "csv")
DIR_EXTRACT_OUT    <- file.path(ROOT_DIR, "extraction", "output")
DIR_MERGE          <- file.path(ROOT_DIR, "extraction", "output", "merge")
DIR_MATCHING       <- file.path(ROOT_DIR, "extraction", "output", "matching")
DIR_INPUT_SHP      <- file.path(ROOT_DIR, "extraction", "input", "shp")
DIR_STORM          <- file.path(ROOT_DIR, "aqdata", "storm_events")
DIR_STORM_RAW      <- file.path(DIR_STORM, "raw")

## --- Key input files ---
FILE_AIRNOW_LOCS        <- file.path(DIR_AIRNOW_HOURLY,  "airnow_aqsid_location_rev_010926.csv")
FILE_USCAN_SHP          <- file.path(DIR_USCAN,          "us_canada.shp")
FILE_URBAN_SHP          <- file.path(DIR_URBAN,          "cb_2020_us_ua20_500k_prj.shp")
## GVP: Holocene eruption list (no coordinates) and Holocene volcano list
##   (coordinates; downloaded .xls re-saved as .xlsx), joined by Volcano Number
##   (util_gvp_read.R)
FILE_GVP_XLSX           <- file.path(DIR_GVP,            "GVP_Eruption_List_Holocene_20260424.xlsx")
FILE_GVP_COORDS         <- file.path(DIR_GVP,            "GVP_Volcano_List_Holocene_202610091715.xlsx")
FILE_FIRED_EVENTS       <- file.path(DIR_FIRED,          "fired_usa_2000_to_2026_events.gpkg")
FILE_AIRNOW_PM25        <- file.path(DIR_AIRNOW_COMBINE, "airnow_PM2.5_2014_2025.csv.gz")

## --- NOAA Storm Events: zone-to-county resolution (stage04, stage04_pa) ---
##   Zone-coded events (CZ_TYPE = "Z") are resolved to counties with the NWS
##   zone-county correlation file (https://www.weather.gov/gis/ZoneCounty),
##   then with the lookup file for zones renumbered or redrawn since 2014.
##   See util_storm_zone_resolve.R.
FILE_ZONE_COUNTY        <- file.path(DIR_STORM, "bp18mr25.dbx")
FILE_ZONE_LEGACY        <- file.path(DIR_STORM, "storm_zone_county_legacy.csv")

## --- Sensor location shapefiles (utility outputs) ---
##   util_sensor_location_shp.R → FILE_AIRNOW_LOC_SHP
##   util_terrain_extract.R     → FILE_AIRNOW_TERRAIN_SHP
##   util_sensor_us_attrs.R     → FILE_AIRNOW_USATTRS_SHP
FILE_AIRNOW_LOC_SHP     <- file.path(DIR_INPUT_SHP, "airnow_pm25_location.shp")
FILE_AIRNOW_TERRAIN_SHP <- file.path(DIR_INPUT_SHP, "airnow_pm25_location_terrain.shp")
FILE_AIRNOW_USATTRS_SHP <- file.path(DIR_INPUT_SHP, "airnow_pm25_location_us_attrs.shp")

## --- GEE extraction outputs (CSV) ---
FILE_AOD_047        <- file.path(DIR_EXTRACT_CSV, "airnow_aod_047.csv.gz")
FILE_AOD_055        <- file.path(DIR_EXTRACT_CSV, "airnow_aod_055.csv.gz")
FILE_GRIDMET_MAIN   <- file.path(DIR_EXTRACT_CSV, "airnow_gridmet_2014_2025.csv.gz")
FILE_GRIDMET_HIST   <- file.path(DIR_EXTRACT_CSV, "airnow_gridmet_temp_1980_2013.csv.gz")
FILE_GRIDMET_SPEI   <- file.path(DIR_EXTRACT_CSV, "airnow_gridmet_spei_2014_2025.csv.gz")
FILE_GRIDMET_P95    <- file.path(DIR_EXTRACT_CSV, "airnow_gridmet_tmp_p95.csv.gz")
FILE_DAYMET_MAIN    <- file.path(DIR_EXTRACT_CSV, "airnow_daymet_2014_2025.csv.gz")
FILE_DAYMET_HIST    <- file.path(DIR_EXTRACT_CSV, "airnow_daymet_temp_1980_2013.csv.gz")
FILE_DAYMET_P95     <- file.path(DIR_EXTRACT_CSV, "airnow_daymet_tmp_p95.csv.gz")

## --- Stage outputs ---
FILE_HMS_FIRED_OUT  <- file.path(DIR_EXTRACT_OUT,  "airnow_smoke_fired_2014_25.csv.gz")
FILE_STORM_OUT      <- file.path(DIR_EXTRACT_OUT,  "airnow_noaa_storm_events_county_2014_25.csv.gz")
FILE_GVP_OUT        <- file.path(DIR_EXTRACT_OUT,  "airnow_gvp_select_2014_25.csv.gz")
FILE_PM25_AOD_MET   <- file.path(DIR_MERGE,         "airnow_pm25_aod_hms_fired_storm_gvp_met.csv.gz")

## --- Stage 7/8 output — single daily classified dataset ---
FILE_DAILY_CLASSIFIED <- file.path(DIR_MERGE, "airnow_daily_classified.csv.gz")

## --- Stage 9 output — with spatial attributes appended ---
FILE_DAILY_INTEGRATED <- file.path(DIR_MERGE, "airnow_daily_classified_US.csv.gz")

## -----------------------------------------------------------------------------
## 3. Study date range
## -----------------------------------------------------------------------------
## The hourly grid (stage06) covers the UTC hours of STUDY_START to STUDY_END.
## Daily records are local standard dates (UTC + GMT_Offset, no daylight
## saving), so the first and last local days of the period are partial and
## usually fail MIN_HOURS_PM.
STUDY_START <- as.Date("2014-01-01")
STUDY_END   <- as.Date("2025-12-31")

## Historical date range (for p95 climatology baseline extraction)
HIST_START  <- as.Date("1980-01-01")
HIST_END    <- as.Date("2013-12-31")

## -----------------------------------------------------------------------------
## 4. Sensor identifier field
## -----------------------------------------------------------------------------
SENSOR_ID <- "AQSID"

## -----------------------------------------------------------------------------
## 5. Classification parameters (MS1 Table 3)
## -----------------------------------------------------------------------------

## AOD non-hazard threshold (cln.thr): applied to wildfire, volcano, dust,
##   and baseline definition. NOT applied to storm or heat categories.
CLN_THR      <- 0.10

## PM2.5 daily mean cap (µg/m³): sensor-days with a daily mean above it are
##   removed from the daily output
PM_MAX       <- 1000

## Minimum valid hourly observations per daily mean: 18 of 24 hours (75%) of a
##   local-standard-time day (EPA 24-hour PM2.5 convention, 40 CFR 50 App. N)
MIN_HOURS_PM <- 18L

## Hourly PM2.5 floor (µg/m³): hourly values below it are set to NA before
##   daily aggregation
PM_FLOOR     <- -5

## Heat absolute floor in Kelvin (= 90°F / 32.2°C)
##   tmmx (GridMET) is in Kelvin. Where GridMET is missing (Alaska, Hawaii,
##   Puerto Rico), stage07_08 uses Daymet tmax (°C) and applies the same floor
##   as HEAT_ABS_THR_K - 273.15.
HEAT_ABS_THR_K <- 305.372

## Heat persistence: consecutive days required for Heat and Heat+Drought
##   (not applied to Wildfire+Heat or Wildfire+Heat+Drought)
HEAT_MIN_DAYS <- 2L

## Drought threshold: SPEI at 90-day accumulation
##   SPEI comes from GridMET only (CONUS); drought categories are not assigned
##   where it is missing.
SPEI_DROUGHT_THR <- -1.0

## Volcanic eruption spatial buffer (metres)
GVP_BUFFER_M <- 100000L

## -----------------------------------------------------------------------------
## 6. p95 climatology parameters (MS1 §2.1)
## -----------------------------------------------------------------------------

## Half-width of ±N-day moving window centred on each calendar day
P95_WINDOW_DAYS <- 7L

## Baseline periods used for p95 computation
##   1991_2020 is the canonical period used for hazard classification (MS1 §2.1)
##   1981_2010 is retained in the output table as supplementary reference only
P95_PERIODS <- list(
  `1981_2010` = c(1981L, 2010L),
  `1991_2020` = c(1991L, 2020L)
)

## Canonical baseline for classification (must match a key in P95_PERIODS)
##   stage03 writes one p95 table per temperature source. stage07_08 compares
##   GridMET tmmx with tmmx_p95_1991_2020 where GridMET is available, and
##   Daymet tmax with tmax_p95_1991_2020 where it is not.
P95_CLASSIFICATION_PERIOD <- "1991_2020"

## -----------------------------------------------------------------------------
## 7. Daily output column schema
##    Metric columns carried from the hourly integration table into daily outputs.
##    stage07_08 (aggregation/classification) and stage09 (attribute join)
##    reference this vector to ensure a consistent column set.
##    (date and GMT_Offset are key columns, not metrics.)
## -----------------------------------------------------------------------------
METRIC_COLS <- c(
  ## PM2.5 — AirNow regulatory only
  "PM25",
  ## Aerosol optical depth
  "AOD_047", "AOD_055",
  ## GridMET daily met
  "pr", "rmax", "rmin", "srad_gridmet", "th", "tmmn", "tmmx", "vs", "vpd",
  ## GridMET p95 climatological thresholds (both baselines retained);
  ## tmmx_p95_1991_2020 is the heat/Baseline threshold where GridMET is available
  "tmmx_p95_1981_2010", "tmmx_p95_1991_2020",
  "tmmn_p95_1981_2010", "tmmn_p95_1991_2020",
  ## DayMet daily met
  "prcp", "srad_daymet", "tmax", "tmin", "vp", "dayl",
  ## DayMet p95 climatological thresholds;
  ## tmax_p95_1991_2020 is the heat/Baseline threshold where GridMET is missing
  "tmax_p95_1981_2010", "tmax_p95_1991_2020",
  "tmin_p95_1981_2010", "tmin_p95_1991_2020",
  ## SPEI drought indices
  "spei30d", "spei90d"
)
