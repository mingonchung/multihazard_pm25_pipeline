## =============================================================================
## stage06_pa_master_hourly_integration.R
## MS1 pipeline — Stage 6 (PurpleAir)
##
## Integrates all data streams onto the PurpleAir hourly PM2.5 spine.
## Processes one year at a time (per-year loop) to stay within memory limits.
##
## Time base (same as AirNow stage06)
##   - RSIG timestamps are UTC. `datetime` (UTC) is the hourly key; hourly
##     sources (Storm Events) are joined on it.
##   - GMT_Offset = local STANDARD time offset of each sensor (no daylight
##     saving), from its coordinates (lutz time-zone lookup; offset taken in
##     mid-January). Local standard time = UTC + GMT_Offset.
##   - `date` is the LOCAL STANDARD date of each hour. Daily sources (HMS/FIRED,
##     MAIAC AOD, GVP, GridMET, SPEI, Daymet) and the p95 month-day are joined on
##     it, and stage07_08_pa aggregates by it.
##   - A year's run covers the LOCAL year. The combined files are split by UTC
##     year, so the run also reads the last hours of the previous year's file
##     and the first hours of the next year's file, and keeps the hours whose
##     local date falls in the run year. Without a 2026 file, local
##     31 December 2025 is incomplete west of UTC.
##
## Key differences from stage06_master_hourly_integration.R (AirNow):
##   - Combined file columns: Timestamp(UTC), STATION(-), pm25_corrected_hourly(ug/m3)
##     (EPA RSIG format produced by acq_pa_hourly_combine.R)
##   - STATION replaces AQSID; no AQSID_rev logic
##   - Storm events from stage04_pa are a compact interval table
##     (STATION, EVENT_ID, EVENT_TYPE, begin_datetime, end_datetime);
##     hourly expansion happens here, per year, to bound memory usage
##   - GVP is a STATION×eruption reference table; date intervals expanded here
##   - Outdoor: per-year AOD/GridMET/SPEI/DayMet files
##   - Indoor: fixed combined AOD/GridMET/SPEI/DayMet files (loaded once)
##   - DayMet skipped for years > 2025 (collection ends 2025)
##
## Requires: lutz (time-zone lookup from coordinates), sf
##
## Usage (one task = one type × one year; driven by SLURM array):
##   Rscript stage06_pa_master_hourly_integration.R outdoor 2020
##   Rscript stage06_pa_master_hourly_integration.R indoor  2021
##
## Array mapping (sub_pa_stage06.sub):
##   Tasks 1-6  → outdoor 2020-2025
##   Tasks 7-12 → indoor  2020-2025
##
## Output (written to DIR_MERGE):
##   purpleair_{outdoor|indoor}_integrated_{year}.csv.gz
##   Columns added: date (local standard), md, GMT_Offset, srad_gridmet, srad_daymet
## =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(sf)
  library(stringr)
  library(tidyr)
})
if (!requireNamespace("lutz", quietly = TRUE))
  stop("Package 'lutz' is required (time-zone lookup from sensor coordinates).")

source("config_pa.R")

## ---------------------------------------------------------------------------
## 1. Parse arguments: <type> <year>
## ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2)
  stop("Usage: Rscript stage06_pa_master_hourly_integration.R <outdoor|indoor> <year>")

TYPE <- tolower(trimws(args[1]))
if (!TYPE %in% c("outdoor", "indoor"))
  stop("type must be 'outdoor' or 'indoor'")

yr <- as.integer(args[2])
if (is.na(yr) || yr < 2020L || yr > 2025L)
  stop("year must be an integer between 2020 and 2025")

if (!dir.exists(DIR_MERGE)) dir.create(DIR_MERGE, recursive = TRUE)
message(sprintf("Stage 06 PA | type: %s | year: %d", TYPE, yr))

## ---------------------------------------------------------------------------
## 2. Load fixed data (not year-specific)
## ---------------------------------------------------------------------------

## -- Standard-time offset per sensor (from coordinates in the sensor list) --
## Sensor list columns (acq_pa_hourly_combine.R): STATION, lat, lon, elevation
sensor_file <- pa_sensor_list_file(TYPE)
if (!file.exists(sensor_file)) stop("Missing sensor list: ", sensor_file)
sensors <- fread(sensor_file, select = c("STATION", "lat", "lon"))
sensors[, STATION := as.character(STATION)]
sensors <- unique(sensors[!is.na(lat) & !is.na(lon)], by = "STATION")
sensors[, tz := lutz::tz_lookup_coords(lat, lon, method = "accurate", warn = FALSE)]

## Standard offset of a time zone: UTC offset in mid-January (no daylight saving
## in the Northern Hemisphere then). Base R, using the system time-zone database.
std_offset_h <- function(tz_name) {
  t0 <- as.POSIXct("2021-01-15 12:00:00", tz = "UTC")
  local_clock <- format(t0, tz = tz_name, usetz = FALSE)
  as.numeric(difftime(as.POSIXct(local_clock, tz = "UTC"), t0, units = "hours"))
}
tz_tab <- data.table(tz = unique(na.omit(sensors$tz)))
tz_tab[, GMT_Offset := vapply(tz, std_offset_h, numeric(1))]
sensors <- tz_tab[sensors, on = "tz"]
n_no_tz <- sensors[is.na(GMT_Offset), .N]
if (n_no_tz > 0L)
  message(sprintf("  %d sensors outside any time-zone polygon: offset = round(lon / 15)", n_no_tz))
sensors[is.na(GMT_Offset), GMT_Offset := round(lon / 15)]
sensor_offset <- sensors[, .(STATION, GMT_Offset)]
setkey(sensor_offset, STATION)
message("Sensor offsets: ", nrow(sensor_offset), " sensors | ",
        paste(sensor_offset[, .N, by = GMT_Offset][order(GMT_Offset),
              sprintf("%+g h: %d", GMT_Offset, N)], collapse = ", "))
rm(sensors, tz_tab)

## -- Smoke/fire (daily; from stage02_pa) --
## Output schema: date, STATION, density, frd_d_s
smoke_file <- pa_stage_file("smoke", TYPE)
if (!file.exists(smoke_file)) stop("Missing smoke/fire file: ", smoke_file)
smoke_dt <- fread(smoke_file,
                  select = c("STATION", "date", "density", "frd_d_s"),
                  colClasses = c(STATION = "character",
                                 date    = "character",
                                 density = "character",
                                 frd_d_s = "character"))
smoke_dt[, STATION := as.character(STATION)]
smoke_dt[, date    := as.Date(date)]
setkey(smoke_dt, STATION, date)
message("Smoke/fire loaded: ", nrow(smoke_dt), " rows")

## -- Storm events (compact interval table; from stage04_pa) --
## Output schema: STATION, EVENT_ID, EVENT_TYPE, begin_datetime, end_datetime
## Hourly expansion deferred to per-year loop below.
storm_file <- pa_stage_file("storm", TYPE)
if (!file.exists(storm_file)) stop("Missing storm file: ", storm_file)
storm_dt <- fread(storm_file,
                  colClasses = c(STATION    = "character",
                                 EVENT_ID   = "character",
                                 EVENT_TYPE = "character",
                                 begin_datetime = "character",
                                 end_datetime   = "character"))
storm_dt[, STATION        := as.character(STATION)]
## Timestamps are ISO 8601 as written by fwrite (e.g. "2021-01-01T02:00:00Z").
## as.POSIXct() without a format keeps only the date of such strings in some
## R versions (checked: R 4.3.3), so they are parsed explicitly.
storm_dt[, begin_datetime := ymd_hms(begin_datetime, tz = "UTC")]
storm_dt[, end_datetime   := ymd_hms(end_datetime,   tz = "UTC")]
if (storm_dt[, anyNA(begin_datetime) | anyNA(end_datetime)])
  stop("Unparsed storm event times in ", storm_file)
storm_dt <- storm_dt[!(is.na(EVENT_ID) & is.na(EVENT_TYPE))]
message("Storm events loaded: ", nrow(storm_dt), " rows (interval format)")

## -- GVP (station×eruption reference; from stage05_pa) --
gvp_file <- pa_stage_file("gvp", TYPE)
if (!file.exists(gvp_file)) stop("Missing GVP file: ", gvp_file)
gvp_ref <- fread(gvp_file,
                 select = c("STATION", "Volcano_Number", "Eruption_Number",
                            "start_date", "end_date"))
gvp_ref[, STATION    := as.character(STATION)]
gvp_ref[, start_date := as.Date(start_date)]
gvp_ref[, end_date   := as.Date(end_date)]
gvp_ref <- gvp_ref[!is.na(Volcano_Number)]
message("GVP reference loaded: ", nrow(gvp_ref), " rows")

## -- p95 climatology tables (fixed: STATION × md) --
gridmet_p95 <- fread(
  if (TYPE == "outdoor") FILE_PA_GRIDMET_P95_OUTDOOR else FILE_PA_GRIDMET_P95_INDOOR,
  select = c("STATION", "md",
             "tmmx_p95_1981_2010", "tmmx_p95_1991_2020",
             "tmmn_p95_1981_2010", "tmmn_p95_1991_2020")
)[, STATION := as.character(STATION)]
gridmet_p95 <- unique(gridmet_p95, by = c("STATION", "md"))

daymet_p95 <- fread(
  if (TYPE == "outdoor") FILE_PA_DAYMET_P95_OUTDOOR else FILE_PA_DAYMET_P95_INDOOR,
  select = c("STATION", "md",
             "tmax_p95_1981_2010", "tmax_p95_1991_2020",
             "tmin_p95_1981_2010", "tmin_p95_1991_2020")
)[, STATION := as.character(STATION)]
daymet_p95 <- unique(daymet_p95, by = c("STATION", "md"))

## -- Indoor: load combined met files and filter to this year only --
## (Full indoor files span 2020-2025; subsetting to yr reduces peak RAM
##  per task now that each array task processes a single year.)
yr_start_date <- as.Date(paste0(yr, "-01-01"))
yr_end_date   <- as.Date(paste0(yr, "-12-31"))

if (TYPE == "indoor") {
  aod_clean <- {
    a47 <- fread(PA_FILE_AOD_047_INDOOR,
                 select = c("STATION", "time", "AOD_047")
                 )[, `:=`(STATION = as.character(STATION),
                           date    = as.Date(time))][, time := NULL
                 ][date >= yr_start_date & date <= yr_end_date]
    a55 <- fread(PA_FILE_AOD_055_INDOOR,
                 select = c("STATION", "time", "AOD_055")
                 )[, `:=`(STATION = as.character(STATION),
                           date    = as.Date(time))][, time := NULL
                 ][date >= yr_start_date & date <= yr_end_date]
    merge(a47, a55, by = c("STATION", "date"), all = TRUE) |>
      unique(by = c("STATION", "date"))
  }

  gridmet_clean <- fread(PA_FILE_GRIDMET_MAIN_INDOOR,
    select = c("STATION", "time", "pr", "rmax", "rmin", "srad", "th",
               "tmmn", "tmmx", "vs", "vpd")
  )[, `:=`(STATION = as.character(STATION), date = as.Date(time))][, time := NULL
  ][!is.na(pr) & date >= yr_start_date & date <= yr_end_date]
  setnames(gridmet_clean, "srad", "srad_gridmet")
  gridmet_clean <- unique(gridmet_clean, by = c("STATION", "date"))

  spei_clean <- fread(PA_FILE_GRIDMET_SPEI_INDOOR,
    select = c("STATION", "time", "spei30d", "spei90d")
  )[, `:=`(STATION = as.character(STATION), date = as.Date(time))][, time := NULL
  ][date >= yr_start_date & date <= yr_end_date]
  spei_clean <- unique(spei_clean, by = c("STATION", "date"))

  daymet_clean <- fread(PA_FILE_DAYMET_MAIN_INDOOR,
    select = c("STATION", "time", "dayl", "prcp", "srad", "tmax", "tmin", "vp")
  )[, `:=`(STATION = as.character(STATION), date = as.Date(time))][, time := NULL
  ][!is.na(prcp) & date >= yr_start_date & date <= yr_end_date]
  setnames(daymet_clean, "srad", "srad_daymet")
  daymet_clean <- unique(daymet_clean, by = c("STATION", "date"))

  message(sprintf("Indoor fixed met files loaded and filtered to %d.", yr))
}

## ---------------------------------------------------------------------------
## 3. Process single year
## ---------------------------------------------------------------------------
## Each SLURM array task handles exactly one type × year combination.
## Fixed data (smoke, storm, GVP, p95) loaded above is subsetted inline.

  out_file <- sprintf(
    if (TYPE == "outdoor") PA_FILE_INTEGRATED_OUTDOOR_TPL else PA_FILE_INTEGRATED_INDOOR_TPL,
    yr
  )

  if (file.exists(out_file)) {
    message(sprintf("[%d] Output already exists, nothing to do: %s", yr, basename(out_file)))
    quit(save = "no", status = 0)
  }

  message(sprintf("\n===== Year %d | type: %s =====", yr, TYPE))

  year_start_date <- yr_start_date   # local-year bounds (dates)
  year_end_date   <- yr_end_date

  ## ---- 3a. PM2.5 hourly spine from combined files ----
  ## EPA RSIG combined file columns (from acq_pa_hourly_combine.R):
  ##   Timestamp(UTC), lon, lat, elevation, STATION(-), count,
  ##   pm25_corrected_hourly(ug/m3)
  ## Column names contain special chars; locate by pattern match on header.
  combined_tpl  <- if (TYPE == "outdoor") PA_COMBINED_TPL_OUTDOOR else PA_COMBINED_TPL_INDOOR
  combined_file <- sprintf(combined_tpl, yr)
  if (!file.exists(combined_file))
    stop(sprintf("[%d] Combined file missing: %s", yr, combined_file))

  ## Read header first to find column positions robustly
  hdr     <- names(fread(combined_file, nrows = 0L, showProgress = FALSE))
  col_ts  <- which(grepl("Timestamp|timestamp", hdr, ignore.case = TRUE))[1]
  col_st  <- which(grepl("^STATION",            hdr, ignore.case = TRUE))[1]
  col_pm  <- which(grepl("pm25_corrected",      hdr, ignore.case = TRUE))[1]

  if (any(is.na(c(col_ts, col_st, col_pm))))
    stop(sprintf("  [%d] Expected columns not found in combined file.\n  Headers: %s",
                 yr, paste(hdr, collapse = ", ")))
  if (col_ts != 1L)
    stop(sprintf("  [%d] Timestamp must be the first column of the combined file.", yr))

  read_spine <- function(f, keep_regex = NULL) {
    if (is.null(keep_regex)) {
      d <- fread(f, select = c(col_ts, col_st, col_pm), showProgress = FALSE)
    } else {
      ## Only the header and the rows whose timestamp (first field) matches
      cmd <- sprintf("gunzip -c %s | grep -E '^\"?([Tt]imestamp|%s)'",
                     shQuote(f), keep_regex)
      d <- tryCatch(fread(cmd = cmd, select = c(col_ts, col_st, col_pm),
                          showProgress = FALSE),
                    error = function(e) NULL)
      if (is.null(d) || !nrow(d)) {
        message(sprintf("  [%d] No boundary hours read from %s", yr, basename(f)))
        return(NULL)
      }
    }
    setnames(d, c("datetime", "STATION", "PM25"))
    d[, STATION  := as.character(STATION)]
    ## fread reads the RSIG timestamps (e.g. "2020-01-01T00:00:00-0000") as
    ## POSIXct; parse explicitly if they arrive as text
    if (!inherits(d$datetime, "POSIXct")) d[, datetime := ymd_hms(datetime, tz = "UTC")]
    if (anyNA(d$datetime)) stop("Unparsed timestamps in ", basename(f))
    d
  }

  ## This UTC year, plus the boundary hours that belong to the local year:
  ## previous file from 31 Dec 12:00 UTC (offsets up to +12 h),
  ## next file to 1 Jan 11:00 UTC (offsets down to -12 h)
  prev_file <- sprintf(combined_tpl, yr - 1L)
  next_file <- sprintf(combined_tpl, yr + 1L)
  pa_raw <- rbindlist(list(
    if (file.exists(prev_file))
      read_spine(prev_file, sprintf("%d-12-31T(1[2-9]|2[0-3])", yr - 1L)),
    read_spine(combined_file),
    if (file.exists(next_file))
      read_spine(next_file, sprintf("%d-01-01T(0[0-9]|1[01])", yr + 1L))
  ), use.names = TRUE)
  if (!file.exists(next_file))
    message(sprintf("  [%d] No combined file for %d: the last local day of %d is incomplete west of UTC.",
                    yr, yr + 1L, yr))

  ## Floor to hour and average (handle any sub-hourly records)
  pa_raw[, datetime := floor_date(datetime, "hour")]
  pa_hourly <- pa_raw[
    !is.na(PM25),
    .(PM25 = mean(PM25, na.rm = TRUE)),
    by = .(STATION, datetime)
  ]
  rm(pa_raw)

  ## Local standard date; keep the hours of this local year
  pa_hourly <- sensor_offset[pa_hourly, on = "STATION"]
  n_no_off  <- pa_hourly[is.na(GMT_Offset), uniqueN(STATION)]
  if (n_no_off > 0L)
    message(sprintf("  [%d] %d stations not in the sensor list (no offset): dropped", yr, n_no_off))
  pa_hourly <- pa_hourly[!is.na(GMT_Offset)]
  pa_hourly[, date := as.Date(datetime + GMT_Offset * 3600, tz = "UTC")]
  pa_hourly <- pa_hourly[date >= year_start_date & date <= year_end_date]
  pa_hourly[, md := format(date, "%m-%d")]
  setcolorder(pa_hourly, c("STATION", "datetime", "date", "GMT_Offset", "PM25", "md"))

  ## UTC span of this local year's spine (used to clip storm events)
  year_start_dt <- min(pa_hourly$datetime)
  year_end_dt   <- max(pa_hourly$datetime)

  stations_yr <- unique(pa_hourly$STATION)
  if (!length(stations_yr)) stop(sprintf("[%d] No stations found in combined file.", yr))
  message(sprintf("  [%d] PM25 spine: %d rows | %d stations | UTC %s to %s",
                  yr, nrow(pa_hourly), length(stations_yr),
                  format(year_start_dt, "%Y-%m-%d %H:%M"), format(year_end_dt, "%Y-%m-%d %H:%M")))

  ## ---- 3b. Smoke / fire (daily join on local date) ----
  ## Columns from stage02_pa: STATION, date, density, frd_d_s
  out <- merge(
    pa_hourly,
    smoke_dt[STATION %in% stations_yr],
    by = c("STATION", "date"),
    all.x = TRUE
  )
  message(sprintf("  [%d] + smoke/fire: %d rows", yr, nrow(out)))

  ## ---- 3c. AOD (daily join on local date) ----
  if (TYPE == "outdoor") {
    a47_f <- sprintf(PA_FILE_AOD_047_OUTDOOR_TPL, yr)
    a55_f <- sprintf(PA_FILE_AOD_055_OUTDOOR_TPL, yr)
    a47 <- if (file.exists(a47_f))
      fread(a47_f, select = c("STATION","time","AOD_047"))[
        , `:=`(STATION = as.character(STATION), date = as.Date(time))][, time := NULL]
    else data.table(STATION = character(), date = as.Date(character()), AOD_047 = numeric())
    a55 <- if (file.exists(a55_f))
      fread(a55_f, select = c("STATION","time","AOD_055"))[
        , `:=`(STATION = as.character(STATION), date = as.Date(time))][, time := NULL]
    else data.table(STATION = character(), date = as.Date(character()), AOD_055 = numeric())
    aod_yr <- merge(a47, a55, by = c("STATION","date"), all = TRUE)[
      STATION %in% stations_yr] |> unique(by = c("STATION","date"))
    rm(a47, a55)
  } else {
    aod_yr <- aod_clean[STATION %in% stations_yr]
  }
  out <- merge(out, aod_yr, by = c("STATION","date"), all.x = TRUE)
  message(sprintf("  [%d] + AOD: %d rows", yr, nrow(out)))

  ## ---- 3d. Storm events (hourly join on UTC datetime) ----
  ## stage04_pa outputs compact interval rows: STATION, EVENT_ID, EVENT_TYPE,
  ## begin_datetime, end_datetime.  Expand to hourly here, within the UTC span
  ## of this local year, to bound peak memory to one year's worth of
  ## events × sensors.
  storm_yr_ref <- storm_dt[
    STATION %in% stations_yr &
    begin_datetime <= year_end_dt & end_datetime >= year_start_dt
  ]

  if (nrow(storm_yr_ref) > 0L) {
    ## Clip event intervals to the spine's UTC span, floor to hour
    storm_yr_ref[, begin_clip := floor_date(pmax(begin_datetime, year_start_dt), "hour")]
    storm_yr_ref[, end_clip   := floor_date(pmin(end_datetime,   year_end_dt),   "hour")]
    storm_yr_ref <- storm_yr_ref[begin_clip <= end_clip]

    ## Expand each event × station to a sequence of hourly datetimes
    ## Group by STATION+EVENT_ID+EVENT_TYPE+begin/end clip (each group = 1 row)
    storm_hourly <- storm_yr_ref[
      , .(datetime = seq(begin_clip, end_clip, by = "hour")),
      by = .(STATION, EVENT_ID, EVENT_TYPE, begin_clip, end_clip)
    ][, c("begin_clip", "end_clip") := NULL]

    ## Collapse multiple events at the same STATION × hour
    storm_hourly <- storm_hourly[, .(
      EVENT_ID   = paste(sort(unique(EVENT_ID)),   collapse = "; "),
      EVENT_TYPE = paste(sort(unique(EVENT_TYPE)), collapse = "; ")
    ), by = .(STATION, datetime)]

    out <- merge(out, storm_hourly, by = c("STATION", "datetime"), all.x = TRUE)
    rm(storm_hourly, storm_yr_ref)
  } else {
    out[, `:=`(EVENT_ID = NA_character_, EVENT_TYPE = NA_character_)]
    rm(storm_yr_ref)
  }
  message(sprintf("  [%d] + storm: %d rows", yr, nrow(out)))

  ## ---- 3e. GVP (expand date intervals for this local year; daily join) ----
  gvp_yr_ref <- gvp_ref[
    STATION %in% stations_yr &
    start_date <= year_end_date & end_date >= year_start_date
  ]
  if (nrow(gvp_yr_ref) > 0L) {
    ## Expand row-by-row to avoid if(s <= e) failing when a STATION has
    ## multiple eruption rows in the same year (s and e become length > 1
    ## vectors inside a by=STATION j-expression).
    ## gvp_ref has ~30 rows total so lapply over rows is trivially fast.
    gvp_rows <- lapply(seq_len(nrow(gvp_yr_ref)), function(k) {
      s <- pmax(gvp_yr_ref$start_date[k], year_start_date)
      e <- pmin(gvp_yr_ref$end_date[k],   year_end_date)
      if (s > e) return(NULL)
      data.table(
        STATION         = gvp_yr_ref$STATION[k],
        date            = seq.Date(s, e, by = "day"),
        Volcano_Number  = as.character(gvp_yr_ref$Volcano_Number[k]),
        Eruption_Number = as.character(gvp_yr_ref$Eruption_Number[k])
      )
    })
    gvp_daily <- rbindlist(Filter(Negate(is.null), gvp_rows), use.names = TRUE)
    rm(gvp_rows)

    if (nrow(gvp_daily) > 0L) {
      ## Collapse multiple eruptions at the same STATION × date
      gvp_daily <- gvp_daily[, .(
        Volcano_Number  = paste(unique(Volcano_Number),  collapse = ";"),
        Eruption_Number = paste(unique(Eruption_Number), collapse = ";")
      ), by = .(STATION, date)]
      out <- merge(out, gvp_daily, by = c("STATION","date"), all.x = TRUE)
    } else {
      out[, `:=`(Volcano_Number = NA_character_, Eruption_Number = NA_character_)]
    }
    rm(gvp_daily)
  } else {
    out[, `:=`(Volcano_Number = NA_character_, Eruption_Number = NA_character_)]
  }
  rm(gvp_yr_ref)
  message(sprintf("  [%d] + GVP: %d rows", yr, nrow(out)))

  ## ---- 3f. GridMET (daily join on local date) ----
  if (TYPE == "outdoor") {
    gm_f <- sprintf(PA_FILE_GRIDMET_MAIN_OUTDOOR_TPL, yr)
    gm_yr <- if (file.exists(gm_f)) {
      g <- fread(gm_f, select = c("STATION","time","pr","rmax","rmin","srad","th",
                                  "tmmn","tmmx","vs","vpd"))[
        , `:=`(STATION = as.character(STATION), date = as.Date(time))][, time := NULL][
          !is.na(pr)][STATION %in% stations_yr] |> unique(by = c("STATION","date"))
      setnames(g, "srad", "srad_gridmet")
      g
    } else data.table(STATION = character(), date = as.Date(character()))
  } else {
    gm_yr <- gridmet_clean[STATION %in% stations_yr]
  }
  out <- merge(out, gm_yr, by = c("STATION","date"), all.x = TRUE)
  message(sprintf("  [%d] + GridMET: %d rows", yr, nrow(out)))

  ## ---- 3g. GridMET p95 (md join; md of the local date) ----
  out <- merge(out, gridmet_p95[STATION %in% stations_yr],
               by = c("STATION","md"), all.x = TRUE)

  ## ---- 3h. SPEI (sparse → densify daily → local date join) ----
  if (TYPE == "outdoor") {
    sp_f <- sprintf(PA_FILE_GRIDMET_SPEI_OUTDOOR_TPL, yr)
    spei_sparse <- if (file.exists(sp_f))
      fread(sp_f, select = c("STATION","time","spei30d","spei90d"))[
        , `:=`(STATION = as.character(STATION), date = as.Date(time))][, time := NULL][
          STATION %in% stations_yr] |> unique(by = c("STATION","date"))
    else data.table(STATION = character(), date = as.Date(character()),
                    spei30d = numeric(), spei90d = numeric())
  } else {
    spei_sparse <- spei_clean[STATION %in% stations_yr]
  }

  full_dates_yr <- seq.Date(year_start_date, year_end_date, by = "day")
  spei_filled <- spei_sparse[
    , tidyr::complete(.SD, date = full_dates_yr), by = STATION
  ][order(STATION, date)
  ][, `:=`(
    spei30d = nafill(nafill(spei30d, type = "locf"), type = "nocb"),
    spei90d = nafill(nafill(spei90d, type = "locf"), type = "nocb")
  ), by = STATION]

  out <- merge(out, spei_filled, by = c("STATION","date"), all.x = TRUE)
  rm(spei_filled, spei_sparse)
  message(sprintf("  [%d] + SPEI: %d rows", yr, nrow(out)))

  ## ---- 3i. DayMet (daily join on local date; skip year > 2025) ----
  if (yr <= 2025) {
    if (TYPE == "outdoor") {
      dm_f <- sprintf(PA_FILE_DAYMET_MAIN_OUTDOOR_TPL, yr)
      dm_yr <- if (file.exists(dm_f)) {
        g <- fread(dm_f, select = c("STATION","time","dayl","prcp","srad","tmax","tmin","vp"))[
          , `:=`(STATION = as.character(STATION), date = as.Date(time))][, time := NULL][
            !is.na(prcp)][STATION %in% stations_yr] |> unique(by = c("STATION","date"))
        setnames(g, "srad", "srad_daymet")
        g
      } else data.table(STATION = character(), date = as.Date(character()))
    } else {
      dm_yr <- daymet_clean[STATION %in% stations_yr]
    }
    out <- merge(out, dm_yr, by = c("STATION","date"), all.x = TRUE)
    if (exists("dm_yr")) rm(dm_yr)
  }

  ## ---- 3j. DayMet p95 (md join; md of the local date) ----
  out <- merge(out, daymet_p95[STATION %in% stations_yr],
               by = c("STATION","md"), all.x = TRUE)

  ## ---- 3k. Column order ----
  setcolorder(out, intersect(c("STATION", "datetime", "date", "GMT_Offset", "PM25"), names(out)))
  setorder(out, STATION, datetime)

  ## ---- 3l. Write per-year output ----
  fwrite(out, out_file, compress = "gzip", na = "")
  message(sprintf("  [%d] Wrote %d rows → %s", yr, nrow(out), basename(out_file)))

  rm(out, pa_hourly, aod_yr, gm_yr)
  if (exists("storm_hourly")) rm(storm_hourly)
  if (exists("gvp_daily"))    rm(gvp_daily)
  gc()

message(sprintf("\n[DONE] Stage 06 PA complete | type: %s | year: %d", TYPE, yr))
