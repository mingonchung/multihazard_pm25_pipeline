## =============================================================================
## acq_pa_download.R
## Download hourly PurpleAir PM2.5 history from the PurpleAir API v1, then
## apply channel QA and the extended U.S.-wide correction.
##
## The PurpleAir API supports only ONE sensor at a time for historical data.
## This script loops sensor-by-sensor; runtime is ~11 hrs/year for outdoor.
##
## After this script, run acq_pa_hourly_combine.R to merge the corrected
## hourly files into annual compressed CSVs and build the sensor list. The
## sensor list is NOT generated here — it is always derived from combined
## output.
##
## Usage (SLURM array):
##   Rscript acq_pa_download.R <array_index> <type>
##   array_index : 1=2020  2=2021  3=2022  4=2023  5=2024  6=2025
##   type        : outdoor   (PA location_type = 0)
##                 indoor    (PA location_type = 1)
##
## Prerequisites:
##   export PA_READ_KEY=XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX
##   API keys: https://develop.purpleair.com/keys
##
## Output 1 (per-sensor raw history):
##   {DIR_PA_RAW}/{type}/{sensor_index}_{YEAR}.csv.gz
##   Columns: sensor_index, time_stamp,
##            pm2.5_cf_1_a, pm2.5_cf_1_b,
##            humidity_a, humidity_b, channel_flags
##
## Output 2 (QA-screened, corrected hourly PM2.5; input to the combine step):
##   {DIR_PA_RAW}/{type}/US/pm25_corrected/purpleair_PAAPI_{YEAR}-hourly.gz
##   Tab-separated, one row per sensor-hour, columns in the order read by
##   acq_pa_hourly_combine.R and stage06_pa_master_hourly_integration.R:
##     Timestamp(UTC), LONGITUDE(deg), LATITUDE(deg), ELEVATION(m),
##     STATION(-), COUNT(-), pm25_corrected_hourly(ug/m3)
##   ELEVATION and COUNT are not available from this route and are NA.
##
## QA and correction (Barkjohn et al. 2022, Sensors 22, 9669), applied to the
## hourly averages returned by the API:
##   1. Both channels reported; channel_flags == 0 where the field is returned.
##   2. Channel agreement: |A - B| <= 5 ug/m3  OR  |A - B| / mean(A, B) < 70%.
##   3. PA_cf1 = mean(A, B) >= 0; sensor RH within 0-100%.
##   4. Extended U.S.-wide correction, three regimes on PA_cf1:
##        PA_cf1 < 570:           PM2.5 = 0.524*PA_cf1 - 0.0862*RH + 5.75      (Eq 1)
##        570 <= PA_cf1 < 611:    PM2.5 = w*Eq3 + (1-w)*Eq1,
##                                w = 0.0244*PA_cf1 - 13.9                     (Eq 2)
##        PA_cf1 >= 611:          PM2.5 = 4.21e-4*PA_cf1^2 + 0.392*PA_cf1 + 3.44 (Eq 3)
##
## Re-run safe: per-sensor raw files are skipped if they already exist. The
## corrected yearly file is rebuilt from the raw files on every run.
##
## API rate limit  : >= 1 second between calls (PA guideline).
## Retry logic     : up to MAX_ATTEMPTS per request before skipping.
##
## PurpleAir API documentation:
##   https://api.purpleair.com
##   https://community.purpleair.com/t/loop-api-calls-for-historical-data/4623
## =============================================================================

rm(list = ls(all.names = TRUE))

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(data.table)
})

options(timeout = 120)

## ---------------------------------------------------------------------------
## 0.  Resolve working directory
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
  stop("Usage: Rscript acq_pa_download.R <array_index> <type>\n",
       "  array_index : 1=2020, 2=2021, ..., 6=2025\n",
       "  type        : outdoor | indoor")

array_index <- as.integer(args[1])
TYPE        <- tolower(trimws(args[2]))
if (!TYPE %in% c("outdoor", "indoor")) stop("type must be 'outdoor' or 'indoor'")

YEAR       <- 2019L + array_index
START_DATE <- as.POSIXct(paste0(YEAR, "-01-01 00:00:00"), tz = "UTC")
END_DATE   <- as.POSIXct(paste0(YEAR, "-12-31 23:59:59"), tz = "UTC")
cat(sprintf("Year: %d  |  Type: %s\n\n", YEAR, TYPE))

## ---------------------------------------------------------------------------
## 2.  Configuration
## ---------------------------------------------------------------------------
PA_READ_KEY <- Sys.getenv("PA_READ_KEY")
if (nchar(PA_READ_KEY) == 0)
  stop("PA_READ_KEY environment variable is not set.\n",
       "Export it before running: export PA_READ_KEY=your-read-key\n",
       "API keys: https://develop.purpleair.com/keys")

PA_BASE_URL  <- "https://api.purpleair.com/v1"
AVERAGE      <- 60L        # 1-hour averaging period
CHUNK_DAYS   <- 179L       # stay under 180-day per-call window limit
SLEEP_SEC    <- 1.0        # >= 1 sec between calls (PA rate-limit guideline)
MAX_ATTEMPTS <- 3L

## PA location_type: 0 = outside, 1 = inside
LOCATION_TYPE <- ifelse(TYPE == "outdoor", 0L, 1L)

## Raw CF=1 PM2.5 + humidity needed for the correction in Section 7
HIST_FIELDS <- "pm2.5_cf_1_a,pm2.5_cf_1_b,humidity_a,humidity_b,channel_flags"

## Bounding boxes (CONUS + AK + HI + Canada = USCAN coverage)
BBOXES <- list(
  CONUS  = list(nwlng = -126,   nwlat =  50,   selng =  -66,   selat = 24.0),
  Alaska = list(nwlng = -170,   nwlat =  71,   selng = -130,   selat = 51.0),
  Hawaii = list(nwlng = -161.5, nwlat =  23,   selng = -154.5, selat = 18.5),
  Canada = list(nwlng = -141,   nwlat =  83,   selng =  -52,   selat = 42.0)
)

## Output: per-sensor files in type subfolder
PA_TYPE_RAW_DIR <- file.path(DIR_PA_RAW, TYPE)
if (!dir.exists(PA_TYPE_RAW_DIR)) dir.create(PA_TYPE_RAW_DIR, recursive = TRUE)

cat(sprintf("Output dir: %s\n\n", PA_TYPE_RAW_DIR))

## Channel-agreement criteria and correction breakpoints (Barkjohn et al. 2022)
AB_MAX_DIFF  <- 5      # ug/m3
AB_MAX_RPD   <- 0.70   # relative difference, |A - B| / mean(A, B)
CF1_LINEAR   <- 570    # below: Eq 1
CF1_QUAD     <- 611    # at or above: Eq 3

## Output: corrected hourly file, in the folder read by acq_pa_hourly_combine.R
PA_CORR_DIR  <- file.path(DIR_PA_RAW, TYPE, "US", "pm25_corrected")
PA_CORR_FILE <- file.path(PA_CORR_DIR, sprintf("purpleair_PAAPI_%d-hourly.gz", YEAR))

## Do not mix this route with hourly files obtained from another source:
## the combine step reads every file in this folder for the year.
other_src <- setdiff(
  list.files(PA_CORR_DIR, pattern = paste0("_", YEAR, "-.*\\.gz$")),
  basename(PA_CORR_FILE)
)
if (length(other_src))
  stop("Other hourly files for ", YEAR, " already exist in ", PA_CORR_DIR,
       " (e.g. ", other_src[1], ").\nRemove them or use a separate folder ",
       "before building the PurpleAir-API corrected file.")

## ---------------------------------------------------------------------------
## 3.  Helper: GET with retry
## ---------------------------------------------------------------------------
pa_get <- function(url, query_params, label = "") {
  for (attempt in seq_len(MAX_ATTEMPTS)) {
    resp <- tryCatch(
      GET(url, add_headers("X-API-Key" = PA_READ_KEY),
          query = query_params, timeout(90)),
      error = function(e) {
        message(sprintf("  [WARN] %s attempt %d/%d — %s",
                        label, attempt, MAX_ATTEMPTS, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(resp) && status_code(resp) == 200L) return(resp)
    if (!is.null(resp))
      message(sprintf("  [WARN] %s attempt %d/%d — HTTP %d",
                      label, attempt, MAX_ATTEMPTS, status_code(resp)))
    if (attempt < MAX_ATTEMPTS) Sys.sleep(SLEEP_SEC * 5)
  }
  NULL
}

## ---------------------------------------------------------------------------
## 3b. Helpers: channel QA and extended U.S.-wide correction
## ---------------------------------------------------------------------------

## Extended U.S.-wide correction (Barkjohn et al. 2022), vectorised.
##   pa : mean of channels A and B, CF=1 (ug/m3);  rh : sensor RH (%)
pa_correct <- function(pa, rh) {
  eq1 <- 0.524 * pa - 0.0862 * rh + 5.75                 # Eq 1
  eq3 <- 4.21e-4 * pa^2 + 0.392 * pa + 3.44              # Eq 3
  w   <- 0.0244 * pa - 13.9                              # blend weight, Eq 2
  fifelse(pa < CF1_LINEAR, eq1,
          fifelse(pa < CF1_QUAD, w * eq3 + (1 - w) * eq1, eq3))
}

## Convert the API time_stamp (POSIXct, epoch seconds, or ISO-8601 text) to UTC
pa_time_utc <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  if (is.numeric(x)) return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
  as.POSIXct(sub("Z$", "", as.character(x)),
             format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
}

## Apply QA and correction to one sensor's raw hourly history.
## Returns a data.table in the column layout read by the combine step.
pa_qc_correct <- function(raw, sidx, lon, lat) {
  col <- function(nm) if (nm %in% names(raw)) as.numeric(raw[[nm]]) else
                        rep(NA_real_, nrow(raw))
  a   <- col("pm2.5_cf_1_a")
  b   <- col("pm2.5_cf_1_b")
  ha  <- col("humidity_a")
  hb  <- col("humidity_b")
  rh  <- rowMeans(cbind(ha, hb), na.rm = TRUE)   # NaN when both are missing
  pa  <- (a + b) / 2
  dif <- abs(a - b)

  ok <- !is.na(a) & !is.na(b)                                   # both channels
  if ("channel_flags" %in% names(raw)) {
    fl <- as.numeric(raw[["channel_flags"]])
    ok <- ok & !is.na(fl) & fl == 0                             # no flagged channel
  }
  ok <- ok & (dif <= AB_MAX_DIFF | dif / pa < AB_MAX_RPD)       # channel agreement
  ok <- ok & pa >= 0                                            # valid PM2.5
  ok <- ok & !is.na(rh) & rh >= 0 & rh <= 100                   # valid RH
  ok[is.na(ok)] <- FALSE

  ts <- pa_time_utc(raw[["time_stamp"]])
  ok <- ok & !is.na(ts)

  data.table(
    `Timestamp(UTC)`               = format(ts[ok], "%Y-%m-%dT%H:%M:%S-0000", tz = "UTC"),
    `LONGITUDE(deg)`               = lon,
    `LATITUDE(deg)`                = lat,
    `ELEVATION(m)`                 = NA_real_,
    `STATION(-)`                   = sidx,
    `COUNT(-)`                     = NA_integer_,
    `pm25_corrected_hourly(ug/m3)` = pa_correct(pa[ok], rh[ok])
  )
}

## ---------------------------------------------------------------------------
## 4.  Get sensor list via PA API (for download routing only)
##     This list is NOT saved — the canonical sensor list is always
##     derived from combined files in acq_pa_hourly_combine.R.
## ---------------------------------------------------------------------------
cat("Fetching sensor list from PA API (for download routing)...\n")
all_sensors <- list()

for (region_name in names(BBOXES)) {
  bb <- BBOXES[[region_name]]
  cat("  Region:", region_name, "\n")

  resp <- pa_get(
    paste0(PA_BASE_URL, "/sensors"),
    query_params = list(
      fields        = "sensor_index,latitude,longitude",
      location_type = LOCATION_TYPE,
      nwlng = bb$nwlng, nwlat = bb$nwlat,
      selng = bb$selng, selat = bb$selat
    ),
    label = paste("sensor list", region_name)
  )

  if (is.null(resp)) {
    warning("  Could not retrieve sensor list for region: ", region_name)
    Sys.sleep(SLEEP_SEC)
    next
  }

  parsed   <- fromJSON(rawToChar(resp$content), simplifyVector = TRUE)
  data_mat <- parsed$data
  if (is.null(data_mat) || length(data_mat) == 0) {
    cat("  No sensors in region:", region_name, "\n")
    Sys.sleep(SLEEP_SEC)
    next
  }

  dt <- as.data.table(data_mat)
  setnames(dt, as.character(parsed$fields))
  all_sensors[[region_name]] <- dt
  cat(sprintf("  -> %d sensors\n", nrow(dt)))
  Sys.sleep(SLEEP_SEC)
}

sensors_dt <- rbindlist(all_sensors, use.names = TRUE, fill = TRUE)
sensors_dt <- unique(sensors_dt, by = "sensor_index")
sensors_dt[, sensor_index := as.integer(sensor_index)]
sensors_dt[, latitude     := as.numeric(latitude)]
sensors_dt[, longitude    := as.numeric(longitude)]
sensors_dt <- sensors_dt[!is.na(latitude) & !is.na(longitude)]

cat(sprintf("Sensors to download: %d\n\n", nrow(sensors_dt)))

## ---------------------------------------------------------------------------
## 5.  Build date chunks (179-day windows to stay under 180-day API limit)
## ---------------------------------------------------------------------------
chunk_starts <- seq(START_DATE, END_DATE, by = paste(CHUNK_DAYS, "days"))
chunks <- data.frame(
  start_ts = as.integer(chunk_starts),
  end_ts   = pmin(
    as.integer(chunk_starts + as.difftime(CHUNK_DAYS, units = "days")),
    as.integer(END_DATE)
  ),
  stringsAsFactors = FALSE
)
cat(sprintf("Year %d: %d chunk(s) x up to %d days.\n\n",
            YEAR, nrow(chunks), CHUNK_DAYS))

## ---------------------------------------------------------------------------
## 6.  Download per-sensor history
## ---------------------------------------------------------------------------
n_sensors <- nrow(sensors_dt)
n_success <- 0L; n_skipped <- 0L; n_failed <- 0L

for (ii in seq_len(n_sensors)) {
  sidx     <- sensors_dt$sensor_index[ii]
  out_file <- file.path(PA_TYPE_RAW_DIR, sprintf("%d_%d.csv.gz", sidx, YEAR))

  if (file.exists(out_file)) { n_skipped <- n_skipped + 1L; next }

  cat(sprintf("[%d/%d] Sensor %d ... ", ii, n_sensors, sidx))

  chunk_list <- vector("list", nrow(chunks))
  for (ci in seq_len(nrow(chunks))) {
    Sys.sleep(SLEEP_SEC)
    resp <- pa_get(
      paste0(PA_BASE_URL, "/sensors/", sidx, "/history/csv"),
      query_params = list(
        start_timestamp = chunks$start_ts[ci],
        end_timestamp   = chunks$end_ts[ci],
        average         = AVERAGE,
        fields          = HIST_FIELDS
      ),
      label = sprintf("sensor %d chunk %d", sidx, ci)
    )
    if (is.null(resp)) next
    csv_text <- rawToChar(resp$content)
    if (nchar(trimws(csv_text)) < 10 || !grepl(",", csv_text)) next
    df_chunk <- tryCatch(
      fread(text = csv_text, sep = ",", header = TRUE, showProgress = FALSE),
      error = function(e) NULL
    )
    if (!is.null(df_chunk) && nrow(df_chunk) > 0L)
      chunk_list[[ci]] <- df_chunk
  }

  sensor_dt <- rbindlist(chunk_list, use.names = TRUE, fill = TRUE)
  if (nrow(sensor_dt) == 0L) {
    cat("no data.\n"); n_failed <- n_failed + 1L; next
  }

  sensor_dt[, sensor_index := sidx]
  setcolorder(sensor_dt, c("sensor_index", setdiff(names(sensor_dt), "sensor_index")))
  setorder(sensor_dt, time_stamp)
  fwrite(sensor_dt, out_file, compress = "gzip")
  cat(sprintf("%d rows saved.\n", nrow(sensor_dt)))
  n_success <- n_success + 1L
}

## ---------------------------------------------------------------------------
## 7.  Channel QA + extended U.S.-wide correction
##     Reads every per-sensor raw file for this year and type, applies
##     pa_qc_correct(), and writes one tab-separated yearly file in the folder
##     and column layout that acq_pa_hourly_combine.R reads.
## ---------------------------------------------------------------------------
if (!dir.exists(PA_CORR_DIR)) dir.create(PA_CORR_DIR, recursive = TRUE)

cat("\nApplying channel QA and correction ...\n")
tmp_tsv <- sub("\\.gz$", "", PA_CORR_FILE)
if (file.exists(tmp_tsv))      file.remove(tmp_tsv)
if (file.exists(PA_CORR_FILE)) file.remove(PA_CORR_FILE)

n_raw <- 0; n_kept <- 0; n_sens_out <- 0L; first_write <- TRUE

for (ii in seq_len(n_sensors)) {
  sidx     <- sensors_dt$sensor_index[ii]
  raw_file <- file.path(PA_TYPE_RAW_DIR, sprintf("%d_%d.csv.gz", sidx, YEAR))
  if (!file.exists(raw_file)) next

  raw <- tryCatch(fread(raw_file, showProgress = FALSE), error = function(e) {
    message(sprintf("  [WARN] could not read %s — %s",
                    basename(raw_file), conditionMessage(e)))
    NULL
  })
  if (is.null(raw) || nrow(raw) == 0L || !"time_stamp" %in% names(raw)) next

  corr <- pa_qc_correct(raw, sidx,
                        lon = sensors_dt$longitude[ii],
                        lat = sensors_dt$latitude[ii])
  n_raw  <- n_raw  + nrow(raw)
  n_kept <- n_kept + nrow(corr)
  if (nrow(corr) == 0L) next

  fwrite(corr, tmp_tsv, sep = "\t", append = !first_write, col.names = first_write)
  first_write <- FALSE
  n_sens_out  <- n_sens_out + 1L
}

if (file.exists(tmp_tsv)) {
  system2("gzip", c("-f", shQuote(tmp_tsv)))
  cat(sprintf("  Sensor-hours: %.0f raw | %.0f kept (%.1f%%) | %d sensors\n",
              n_raw, n_kept, 100 * n_kept / max(n_raw, 1), n_sens_out))
  cat(sprintf("  Wrote: %s\n", PA_CORR_FILE))
} else {
  warning("No corrected records produced; corrected file not written.")
}

## ---------------------------------------------------------------------------
## 8.  Summary
## ---------------------------------------------------------------------------
cat(sprintf("\n[COMPLETE] Year %d | Type: %s\n", YEAR, TYPE))
cat(sprintf("  Downloaded : %d\n  Skipped    : %d (already existed)\n  Failed     : %d\n",
            n_success, n_skipped, n_failed))
cat("\nNext step: run acq_pa_hourly_combine.R to combine files and build sensor list.\n")
