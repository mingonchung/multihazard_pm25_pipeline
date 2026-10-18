## =============================================================================
## acq_pa_hourly_combine.R
## Combine hourly PurpleAir .gz files into a single yearly CSV per type, then
## extract the sensor list from the combined output.
##
## INPUT (production data): QA-screened, corrected hourly PM2.5 obtained from
## EPA's Remote Sensing Information Gateway (RSIG) archive of public PurpleAir
## sensors, one file per day and region:
##   {DIR_PA_RAW}/{outdoor|indoor}/US/pm25_corrected/
##   Files: purpleair_CONUS_YYYY-MM-DD.gz, purpleair_AL_YYYY-MM-DD.gz,
##          purpleair_HI_YYYY-MM-DD.gz  (tab-separated, hourly, corrected)
##   An optional CAN/pm25_corrected folder is included if present.
##
## ALTERNATIVE INPUT (PurpleAir API, acq_pa_download.R): writes one QA-screened,
## corrected hourly file per year (purpleair_PAAPI_YYYY-hourly.gz) into the
## same pm25_corrected folder, in the same column order. This script combines
## it in the same way. Do not mix the two sources in one folder and year.
##
## Two-step output per run:
##   1. Combined yearly file:
##      {DIR_PA_COMBINED}/purpleair_{outdoor|indoor}_USCAN_{YEAR}.csv.gz
##      Header preserved from source (tab -> comma). Column order:
##        Timestamp(UTC), longitude, latitude, elevation, STATION(-), count,
##        pm25_corrected_hourly(ug/m3)
##      Downstream scripts locate columns by position (sensor list, below) or
##      by name pattern (stage06_pa).
##
##   2. Sensor list (created/updated after each yearly combine):
##      {DIR_PA_RAW}/{outdoor|indoor}/sensor_list_{type}.csv
##      Columns: STATION, lat, lon, elevation
##      Deduplicated across all available combined years (re-built each run
##      to capture sensors that may appear only in some years).
##
## Usage (SLURM array, one task per year):
##   Rscript acq_pa_hourly_combine.R <array_index> <type>
##   array_index : 1=2020  2=2021  3=2022  4=2023  5=2024  6=2025
##   type        : outdoor | indoor
##
## Re-run safe: skips combined file if it already exists; always rebuilds
## the sensor list from all available combined years (idempotent).
## =============================================================================

rm(list = ls(all.names = TRUE))

suppressPackageStartupMessages(library(data.table))

## ---------------------------------------------------------------------------
## 0.  Resolve working directory for source("config_pa.R")
## ---------------------------------------------------------------------------
this_file <- sub("--file=", "",
  commandArgs(FALSE)[grepl("--file=", commandArgs(FALSE))][1])
script_dir <- if (!is.na(this_file) && nchar(this_file) > 0)
                dirname(normalizePath(this_file, mustWork = FALSE)) else getwd()
setwd(script_dir)
source("config_pa.R")

## ---------------------------------------------------------------------------
## 1.  Parse arguments
## ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2)
  stop("Usage: Rscript acq_pa_hourly_combine.R <array_index> <type>\n",
       "  array_index : 1=2020, ..., 6=2025\n",
       "  type        : outdoor | indoor")

array_index <- as.integer(args[1])
TYPE        <- tolower(trimws(args[2]))
if (!TYPE %in% c("outdoor", "indoor")) stop("type must be 'outdoor' or 'indoor'")

YEAR <- 2019L + array_index
cat(sprintf("Combining year: %d  |  Type: %s\n\n", YEAR, TYPE))

## ---------------------------------------------------------------------------
## 2.  Paths
## ---------------------------------------------------------------------------
## Input: hourly .gz files (RSIG daily files or the PurpleAir-API yearly file)
## Primary US folder; CAN folder included if it exists (optional)
US_RAW_DIR  <- file.path(DIR_PA_RAW, TYPE, "US",  "pm25_corrected")
CAN_RAW_DIR <- file.path(DIR_PA_RAW, TYPE, "CAN", "pm25_corrected")

## Output: combined yearly file
OUT_FILE <- sprintf(
  if (TYPE == "outdoor") PA_COMBINED_TPL_OUTDOOR else PA_COMBINED_TPL_INDOOR,
  YEAR
)

## Sensor list written after combine step
SENSOR_LIST_CSV <- pa_sensor_list_file(TYPE)

if (!dir.exists(DIR_PA_COMBINED)) dir.create(DIR_PA_COMBINED, recursive = TRUE)

## ---------------------------------------------------------------------------
## 3.  Combine hourly files for this year
## ---------------------------------------------------------------------------
if (file.exists(OUT_FILE)) {
  cat("Combined file already exists, skipping combine step:", OUT_FILE, "\n")
} else {

  ## Collect input folders (US always required; CAN optional)
  input_dirs <- c(US = US_RAW_DIR)
  if (dir.exists(CAN_RAW_DIR)) {
    input_dirs <- c(input_dirs, CAN = CAN_RAW_DIR)
    cat("CAN folder found; including Canada data.\n")
  } else {
    cat("CAN folder not found; US-only combine.\n")
  }

  ## Find all .gz files for this year across regions
  ## Matches purpleair_CONUS_YYYY-MM-DD.gz, purpleair_AL_*.gz, purpleair_HI_*.gz
  ## and purpleair_PAAPI_YYYY-hourly.gz
  pattern <- paste0("_", YEAR, "-.*\\.gz$")
  files   <- unlist(lapply(input_dirs, function(d) {
    if (!dir.exists(d)) {
      warning("Input folder not found: ", d)
      return(character())
    }
    list.files(d, pattern = pattern, full.names = TRUE)
  }))

  if (length(files) == 0L)
    stop("No .gz files found for year ", YEAR, " in:\n  ",
         paste(input_dirs, collapse = "\n  "),
         "\nPlace the RSIG hourly files in this folder, or run acq_pa_download.R first.")

  cat(sprintf("Found %d hourly files to combine.\n", length(files)))

  ## Read all files (tab-separated, header=TRUE, preserving original names)
  dt_list <- lapply(seq_along(files), function(i) {
    f  <- files[i]
    dt <- tryCatch(
      fread(cmd = paste("gunzip -c", shQuote(f)),
            sep = "\t", header = TRUE, showProgress = FALSE),
      error = function(e) {
        message(sprintf("  [WARN] Could not read %s: %s", basename(f), conditionMessage(e)))
        NULL
      }
    )
    if (i %% 100L == 0L)
      cat(sprintf("  Read %d / %d files...\n", i, length(files)))
    dt
  })

  dt_all <- rbindlist(Filter(Negate(is.null), dt_list),
                      use.names = TRUE, fill = TRUE)
  rm(dt_list); gc()

  cat(sprintf("Combined: %d rows from %d files.\n", nrow(dt_all), length(files)))

  ## Write combined file (comma-separated to match downstream expectations)
  fwrite(dt_all, OUT_FILE, sep = ",", compress = "gzip", bom = FALSE)
  cat(sprintf("Wrote: %s\n\n", OUT_FILE))
  rm(dt_all); gc()
}

## ---------------------------------------------------------------------------
## 4.  Build / refresh sensor list from ALL available combined years
##
## The sensor list is derived from the combined files, not from the API.
## It is rebuilt each run to capture sensors appearing in any year's data.
## Columns extracted: STATION (from `STATION(-)`), lat, lon, elevation.
## ---------------------------------------------------------------------------
cat("Building sensor list from all available combined years...\n")

## Identify all combined files of this type that exist on disk
all_years   <- 2020L:2025L
all_combined <- sprintf(
  if (TYPE == "outdoor") PA_COMBINED_TPL_OUTDOOR else PA_COMBINED_TPL_INDOOR,
  all_years
)
avail_combined <- all_combined[file.exists(all_combined)]

if (length(avail_combined) == 0L) {
  warning("No combined files found; sensor list not created.")
} else {
  cat(sprintf("  Reading sensor locations from %d combined year(s)...\n",
              length(avail_combined)))

  ## Read only the columns needed, by position (header names contain units)
  ## Column order: Timestamp(UTC), longitude, latitude, elevation, STATION(-), count, pm25_...
  ## We need: STATION(-)[5], lat[3], lon[2], elevation[4]
  sensor_list <- rbindlist(lapply(avail_combined, function(f) {
    tryCatch({
      dt <- fread(f, select = c(2L, 3L, 4L, 5L),  # lon, lat, elevation, STATION(-)
                  showProgress = FALSE)
      ## Standardise column names regardless of special chars in header
      setnames(dt, c("lon", "lat", "elevation", "STATION"))
      unique(dt, by = "STATION")
    }, error = function(e) {
      message(sprintf("  [WARN] Could not read %s for sensor list: %s",
                      basename(f), conditionMessage(e)))
      NULL
    })
  }), use.names = TRUE, fill = TRUE)

  ## Deduplicate across years; keep first occurrence (lat/lon stable for fixed sensors)
  sensor_list <- unique(sensor_list, by = "STATION", fromLast = FALSE)
  sensor_list <- sensor_list[!is.na(STATION) & STATION != ""]

  ## Column order matches downstream expectations
  setcolorder(sensor_list, c("STATION", "lat", "lon", "elevation"))
  setorder(sensor_list, STATION)

  dir.create(dirname(SENSOR_LIST_CSV), recursive = TRUE, showWarnings = FALSE)
  fwrite(sensor_list, SENSOR_LIST_CSV)
  cat(sprintf("  Sensor list: %d unique sensors → %s\n\n",
              nrow(sensor_list), SENSOR_LIST_CSV))
}

cat(sprintf("[DONE] Year %d | Type: %s\n", YEAR, TYPE))
