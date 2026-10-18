## =============================================================================
## stage05_pa_gvp_join.R
## Identify PurpleAir sensors within 100 km of active GVP eruption sites
## and record the sensor × eruption pairs with eruption date windows.
##
## Aligned with stage05_gvp_join.R (AirNow), with these PA-specific changes:
##   - Sensor source: sensor_list_{type}.csv (STATION, lat, lon)
##     instead of constructing stations from hourly PM2.5 records
##   - No join back to hourly PM2.5 data (PA hourly records are large yearly
##     files; temporal filtering happens in Stage 6 master integration)
##   - Output is a station × eruption reference table used in Stage 6
##
## Output columns:
##   STATION, lat, lon, Volcano_Number, Volcano_Name,
##   Eruption_Number, Eruption_Category, VEI, start_date, end_date
##
## Usage:
##   Rscript stage05_pa_gvp_join.R outdoor
##   Rscript stage05_pa_gvp_join.R indoor
##
## GVP files are shared with the AirNow pipeline (FILE_GVP_XLSX, FILE_GVP_COORDS),
## read with util_gvp_read.R.
##
## Sensor list schema (sensor_list_{type}.csv): STATION, lat, lon, elevation
## =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(sf)
  library(lubridate)
  library(data.table)
})

sf_use_s2(TRUE)

source("config_pa.R")

## ---------------------------------------------------------------------------
## 1. Parse argument
## ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
TYPE <- if (length(args) >= 1) tolower(trimws(args[1])) else stop(
  "Usage: Rscript stage05_pa_gvp_join.R <outdoor|indoor>"
)
if (!TYPE %in% c("outdoor", "indoor"))
  stop("type must be 'outdoor' or 'indoor'")

OUT_FILE <- pa_stage_file("gvp", TYPE)

if (file.exists(OUT_FILE)) {
  message("Output already exists, skipping: ", OUT_FILE)
  quit(save = "no", status = 0)
}

message(sprintf("Stage 05 PA | type: %s", TYPE))

## ---------------------------------------------------------------------------
## 2. Load PA sensor locations as sf point layer
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

sensors_sf <- st_as_sf(sensors_dt,
                       coords = c("lon", "lat"),
                       crs    = 4326,
                       remove = FALSE)

message(sprintf("Loaded %d %s sensors.", nrow(sensors_sf), TYPE))

## ---------------------------------------------------------------------------
## 3. Load and filter GVP eruption records (shared file)
## ---------------------------------------------------------------------------
source("util_gvp_read.R")
gvp <- read_gvp_eruptions(year(PA_STUDY_START), year(PA_STUDY_END))

message(sprintf("GVP eruptions in study window: %d", nrow(gvp)))

## ---------------------------------------------------------------------------
## 4. Spatial filter: eruptions within US/Canada boundary
## ---------------------------------------------------------------------------
us_can <- st_read(FILE_USCAN_SHP, quiet = TRUE) |> st_transform(4326)

gvp_sf <- st_as_sf(gvp,
                   coords = c("Longitude", "Latitude"),
                   crs    = 4326,
                   remove = FALSE)

gvp_uscan <- st_filter(gvp_sf, us_can, .predicate = st_within)

message(sprintf("GVP eruptions within US/Canada: %d", nrow(gvp_uscan)))

if (nrow(gvp_uscan) == 0L) {
  message("No eruptions in US/Canada during study period. Writing empty output.")
  empty <- data.table(
    STATION           = character(),
    lat               = numeric(),
    lon               = numeric(),
    Volcano_Number    = integer(),
    Volcano_Name      = character(),
    Eruption_Number   = integer(),
    Eruption_Category = character(),
    VEI               = integer(),
    start_date        = as.Date(NA),
    end_date          = as.Date(NA)
  )
  fwrite(empty, OUT_FILE, compress = "gzip")
  message("[DONE] Wrote empty table → ", OUT_FILE)
  quit(save = "no", status = 0)
}

## ---------------------------------------------------------------------------
## 5. Buffer 100 km around each eruption site; find sensors within buffer
##    (GVP_BUFFER_M = 100000 m, from config.R)
## ---------------------------------------------------------------------------
erupt_buffers <- st_buffer(gvp_uscan, dist = GVP_BUFFER_M)

## Sensors that fall within any eruption buffer
stations_near <- st_filter(sensors_sf, erupt_buffers, .predicate = st_within) |>
  st_join(
    erupt_buffers |> select(Volcano_Name),
    join = st_within
  ) |>
  st_drop_geometry() |>
  as.data.table()

if (nrow(stations_near) == 0L) {
  message("No sensors within 100 km of any eruption. Writing empty output.")
  empty <- data.table(
    STATION           = character(),
    lat               = numeric(),
    lon               = numeric(),
    Volcano_Number    = integer(),
    Volcano_Name      = character(),
    Eruption_Number   = integer(),
    Eruption_Category = character(),
    VEI               = integer(),
    start_date        = as.Date(NA),
    end_date          = as.Date(NA)
  )
  fwrite(empty, OUT_FILE, compress = "gzip")
  message("[DONE] Wrote empty table → ", OUT_FILE)
  quit(save = "no", status = 0)
}

## Join eruption metadata back to the nearby-station table
gvp_meta <- gvp_uscan |>
  st_drop_geometry() |>
  select(Volcano_Number, Volcano_Name, Eruption_Number,
         Eruption_Category, VEI, start_date, end_date)

result <- stations_near |>
  inner_join(gvp_meta, by = "Volcano_Name") |>
  ## STATION column already correct; no rename from sensor_index needed
  select(STATION, lat, lon,
         Volcano_Number, Volcano_Name, Eruption_Number,
         Eruption_Category, VEI, start_date, end_date) |>
  distinct() |>
  as.data.table()

setorder(result, STATION, start_date)

message(sprintf("Output: %d sensor×eruption pairs | %d unique sensors.",
                nrow(result), n_distinct(result$STATION)))

## ---------------------------------------------------------------------------
## 6. Write
## ---------------------------------------------------------------------------
fwrite(result, OUT_FILE, compress = "gzip")
message("[DONE] Wrote: ", OUT_FILE)
