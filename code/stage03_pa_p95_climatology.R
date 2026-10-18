## =============================================================================
## stage03_pa_p95_climatology.R
## Compute station-specific 95th-percentile temperature climatology tables
## for PurpleAir sensors, using the ±7-day moving window method consistent
## with NOAA U.S. Climate Normals (MS1 §2.1, Arguez et al. 2012).
##
## Replaces / supersedes:
##   3_1_historic_temp_heat_1980_25_gridmet.R  (PA sections only)
##   3_2_historic_temp_heat_1980_25_daymet.R   (PA sections only)
## Key improvements: sources config_pa.R (no hardcoded paths), adds --type arg.
##
## Two meteorological sources, selected via --source argument:
##   gridmet  Uses GEE GridMET tmmx/tmmn (K); CONUS only
##   daymet   Uses GEE DayMet tmax/tmin (°C); includes AK/HI/PR
##
## Two sensor types, selected via --type argument:
##   indoor   Fixed combined files → compute in-memory (manageable dataset)
##   outdoor  Per-year annual files → chunked by CHUNK_SIZE stations to
##            avoid RAM exhaustion (~20,000+ sensors × 46 years)
##
## Usage:
##   Rscript stage03_pa_p95_climatology.R --type outdoor --source gridmet
##   Rscript stage03_pa_p95_climatology.R --type indoor  --source gridmet
##   Rscript stage03_pa_p95_climatology.R --type outdoor --source daymet
##   Rscript stage03_pa_p95_climatology.R --type indoor  --source daymet
##
## Output: one p95 lookup table per type × source combination (see config_pa.R)
##   sensor_id, lon, lat, md (MM-DD), doy, t[mm]x_p95_1981_2010,
##   t[mm]x_p95_1991_2020, t[mm]n_p95_1981_2010, t[mm]n_p95_1991_2020
##
## NOTE: This stage reads GEE extraction CSV files (GridMET / DayMet), NOT
##   sensor_list_{type}.csv directly. Those GEE files already use STATION,
##   lon, lat as column names — no schema changes required relative to other
##   stages that were patched for the STATION/lat/lon correction.
## =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

source("config_pa.R")

## ---------------------------------------------------------------------------
## 1. Parse arguments
## ---------------------------------------------------------------------------
args   <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- which(args == flag)
  if (length(i) == 0) return(default)
  if (i == length(args)) stop("Flag '", flag, "' requires a value.")
  args[i + 1]
}

TYPE   <- tolower(get_arg("--type",   "outdoor"))
SOURCE <- tolower(get_arg("--source", "gridmet"))

if (!TYPE   %in% c("outdoor", "indoor"))  stop("--type must be 'outdoor' or 'indoor'")
if (!SOURCE %in% c("gridmet", "daymet"))  stop("--source must be 'gridmet' or 'daymet'")

message(sprintf("Stage 03 PA | type: %s | source: %s", TYPE, SOURCE))

## ---------------------------------------------------------------------------
## 2. Shared parameters from config
## ---------------------------------------------------------------------------
win_days   <- P95_WINDOW_DAYS    # 7
periods    <- P95_PERIODS        # list(1981_2010=..., 1991_2020=...)
CHUNK_SIZE <- 1000L              # stations per chunk for outdoor

## ---------------------------------------------------------------------------
## 3. Helper functions (identical logic to AirNow stage03)
## ---------------------------------------------------------------------------
clean_id <- function(x) gsub("^'+", "", trimws(as.character(x)))

parse_time_to_date <- function(x) {
  suppressWarnings({
    d1 <- mdy(x, quiet = TRUE)
    d2 <- ymd(x, quiet = TRUE)
  })
  as.Date(ifelse(!is.na(d1), as.character(d1), as.character(d2)))
}

make_md_calendar <- function() {
  d <- seq.Date(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")
  data.table(md = format(d, "%m-%d"), doy = seq_along(d))
}

build_md_window_map <- function(win = 7L) {
  cal <- make_md_calendar()
  n   <- nrow(cal)
  map <- CJ(target_doy = 1:n, donor_doy = 1:n)
  map[, dist := pmin(abs(target_doy - donor_doy), n - abs(target_doy - donor_doy))]
  map <- map[dist <= win, .(target_doy, donor_doy)]
  map <- merge(map, cal[, .(target_doy = doy, target_md = md)], by = "target_doy")
  map <- merge(map, cal[, .(donor_doy  = doy, donor_md  = md)], by = "donor_doy")
  map[, .(donor_md, target_md)]
}

prep_temp <- function(dt, id_col, tcols, time_col = "time") {
  dt <- as.data.table(dt)
  dt[, (id_col) := clean_id(get(id_col))]
  dt[, date := parse_time_to_date(get(time_col))]
  dt <- dt[!is.na(date)]
  dt[, year := year(date)]
  dt[, md   := format(date, "%m-%d")]
  dt[, .SD, .SDcols = c(id_col, "lon", "lat", "year", "md", tcols)]
}

## Generic p95 computer; works for both gridmet (tmmx/tmmn) and daymet (tmax/tmin)
compute_p95 <- function(dt, id_col, tcol_max, tcol_min, periods_list,
                        md_map, cal, win = 7L) {

  coords <- dt[, .(
    lon = suppressWarnings(first(na.omit(lon))),
    lat = suppressWarnings(first(na.omit(lat)))
  ), by = .(id = get(id_col))]

  out_list <- list()

  for (nm in names(periods_list)) {
    s <- periods_list[[nm]][1]
    e <- periods_list[[nm]][2]

    base <- dt[year >= s & year <= e,
               .(id = as.character(get(id_col)),
                 donor_md = md,
                 vmax = get(tcol_max),
                 vmin = get(tcol_min))]

    if (!nrow(base)) {
      out_list[[nm]] <- data.table(id = character(), md = character())
      next
    }

    x <- base[md_map, on = .(donor_md), allow.cartesian = TRUE]

    p95_max <- x[!is.na(vmax),
                 .(p95 = as.numeric(quantile(vmax, 0.95, na.rm = TRUE, type = 7))),
                 by = .(id, md = target_md)]
    setnames(p95_max, "p95", paste0(tcol_max, "_p95_", nm))

    p95_min <- x[!is.na(vmin),
                 .(p95 = as.numeric(quantile(vmin, 0.95, na.rm = TRUE, type = 7))),
                 by = .(id, md = target_md)]
    setnames(p95_min, "p95", paste0(tcol_min, "_p95_", nm))

    out_list[[nm]] <- merge(p95_max, p95_min, by = c("id", "md"), all = TRUE)
  }

  out_list2 <- Filter(function(z) ncol(z) > 2 || nrow(z) > 0, out_list)
  if (!length(out_list2)) return(NULL)

  p95_all <- Reduce(function(a, b) merge(a, b, by = c("id", "md"), all = TRUE), out_list2)

  grid    <- CJ(id = sort(unique(dt[[id_col]])), md = cal$md)
  p95_all <- merge(grid, p95_all, by = c("id", "md"), all.x = TRUE)
  p95_all <- merge(p95_all, cal,    by = "md", all.x = TRUE)
  p95_all <- merge(p95_all, coords, by = "id", all.x = TRUE)
  setorder(p95_all, id, doy)

  ## Ensure all four p95 columns present
  need <- c(paste0(tcol_max, "_p95_", names(periods_list)),
            paste0(tcol_min, "_p95_", names(periods_list)))
  for (cc in need) if (!cc %in% names(p95_all)) p95_all[, (cc) := as.numeric(NA)]

  p95_all
}

## ---------------------------------------------------------------------------
## 4. Source-specific settings
## ---------------------------------------------------------------------------
if (SOURCE == "gridmet") {
  TCOL_MAX <- "tmmx"; TCOL_MIN <- "tmmn"
  if (TYPE == "indoor") {
    HIST_FILE <- PA_FILE_GRIDMET_HIST_INDOOR
    MAIN_FILE <- PA_FILE_GRIDMET_MAIN_INDOOR
    OUT_FILE  <- FILE_PA_GRIDMET_P95_INDOOR
    HIST_COLS <- c("STATION", "lon", "lat", "time", "tmmn", "tmmx")
    MAIN_COLS <- c("STATION", "lon", "lat", "time", "tmmn", "tmmx")
  } else {
    ## outdoor: per-year files
    HIST_FILES <- sprintf(PA_FILE_GRIDMET_HIST_OUTDOOR_TPL, 1980:2019)
    MAIN_FILES <- sprintf(PA_FILE_GRIDMET_MAIN_OUTDOOR_TPL, 2020:2025)
    OUT_FILE   <- FILE_PA_GRIDMET_P95_OUTDOOR
    HIST_COLS  <- c("STATION", "lon", "lat", "time", "tmmn", "tmmx")
    MAIN_COLS  <- c("STATION", "lon", "lat", "time", "tmmn", "tmmx")
  }
} else {
  TCOL_MAX <- "tmax"; TCOL_MIN <- "tmin"
  if (TYPE == "indoor") {
    HIST_FILE <- PA_FILE_DAYMET_HIST_INDOOR
    MAIN_FILE <- PA_FILE_DAYMET_MAIN_INDOOR
    OUT_FILE  <- FILE_PA_DAYMET_P95_INDOOR
    HIST_COLS <- c("STATION", "lon", "lat", "time", "tmax", "tmin")
    MAIN_COLS <- c("STATION", "lon", "lat", "time", "tmax", "tmin")
  } else {
    HIST_FILES <- sprintf(PA_FILE_DAYMET_HIST_OUTDOOR_TPL, 1980:2019)
    MAIN_FILES <- sprintf(PA_FILE_DAYMET_MAIN_OUTDOOR_TPL, 2020:2025)  # DayMet ends 2025
    OUT_FILE   <- FILE_PA_DAYMET_P95_OUTDOOR
    HIST_COLS  <- c("STATION", "lon", "lat", "time", "tmax", "tmin")
    MAIN_COLS  <- c("STATION", "lon", "lat", "time", "tmax", "tmin")
  }
}

if (file.exists(OUT_FILE)) {
  message("Output already exists, skipping: ", OUT_FILE)
  quit(save = "no", status = 0)
}

## Pre-build shared objects (reused across chunks)
cal    <- make_md_calendar()
md_map <- build_md_window_map(win_days)

## ---------------------------------------------------------------------------
## 5a. INDOOR: in-memory processing
## ---------------------------------------------------------------------------
if (TYPE == "indoor") {

  message("Reading indoor historical file ...")
  hist_raw <- fread(HIST_FILE, select = HIST_COLS)
  message("Reading indoor main file ...")
  main_raw <- fread(MAIN_FILE, select = MAIN_COLS)

  hist_dt <- prep_temp(hist_raw, "STATION", c(TCOL_MAX, TCOL_MIN))
  main_dt <- prep_temp(main_raw, "STATION", c(TCOL_MAX, TCOL_MIN))

  dt_all <- rbindlist(list(hist_dt, main_dt), use.names = TRUE, fill = TRUE)

  result <- compute_p95(dt_all, "STATION", TCOL_MAX, TCOL_MIN, periods, md_map, cal, win_days)

  if (is.null(result)) stop("No p95 results computed for indoor sensors.")

  out_cols <- c("STATION", "lon", "lat", "md", "doy",
                paste0(TCOL_MAX, "_p95_", names(periods)),
                paste0(TCOL_MIN, "_p95_", names(periods)))
  setnames(result, "id", "STATION")

  fwrite(result[, ..out_cols], OUT_FILE, compress = "gzip")
  message(sprintf("[DONE] Indoor p95 → %s", OUT_FILE))
}

## ---------------------------------------------------------------------------
## 5b. OUTDOOR: chunked processing (large sensor count)
## ---------------------------------------------------------------------------
if (TYPE == "outdoor") {

  ## Check files exist
  missing_h <- HIST_FILES[!file.exists(HIST_FILES)]
  missing_m <- MAIN_FILES[!file.exists(MAIN_FILES)]
  if (length(missing_h)) stop("Missing hist files: ", paste(basename(missing_h), collapse = ", "))
  if (length(missing_m)) stop("Missing main files: ", paste(basename(missing_m), collapse = ", "))

  ## Build station universe from MAIN years only
  message("Reading outdoor station universe from main files ...")
  stations_all <- sort(unique(rbindlist(lapply(MAIN_FILES, function(f)
    fread(f, select = "STATION")[, .(STATION = clean_id(STATION))]
  ))$STATION))
  message("Total outdoor stations: ", length(stations_all))

  n_chunks <- ceiling(length(stations_all) / CHUNK_SIZE)
  out_csv  <- sub("\\.gz$", "", OUT_FILE)   # write plain CSV; gzip at end
  out_gz   <- OUT_FILE

  if (file.exists(out_csv)) file.remove(out_csv)
  if (file.exists(out_gz))  file.remove(out_gz)
  first_write <- TRUE

  out_cols <- c("STATION", "lon", "lat", "md", "doy",
                paste0(TCOL_MAX, "_p95_", names(periods)),
                paste0(TCOL_MIN, "_p95_", names(periods)))

  for (k in seq_len(n_chunks)) {
    idx1     <- (k - 1L) * CHUNK_SIZE + 1L
    idx2     <- min(k * CHUNK_SIZE, length(stations_all))
    st_chunk <- stations_all[idx1:idx2]
    message(sprintf("\n[Outdoor chunk %d/%d] stations %d–%d", k, n_chunks, idx1, idx2))

    read_chunk <- function(files, cols) {
      rbindlist(lapply(files, function(f) {
        if (!file.exists(f)) return(NULL)
        dt <- fread(f, select = cols)
        dt[, STATION := clean_id(STATION)]
        dt <- dt[STATION %chin% st_chunk]
        if (!nrow(dt)) return(NULL)
        prep_temp(dt, "STATION", c(TCOL_MAX, TCOL_MIN))
      }), use.names = TRUE, fill = TRUE)
    }

    hist_dt  <- read_chunk(HIST_FILES, HIST_COLS)
    main_dt  <- read_chunk(MAIN_FILES, MAIN_COLS)
    dt_chunk <- rbindlist(list(hist_dt, main_dt), use.names = TRUE, fill = TRUE)

    if (!nrow(dt_chunk)) { message("  No data; skipping."); next }

    result <- compute_p95(dt_chunk, "STATION", TCOL_MAX, TCOL_MIN,
                          periods, md_map, cal, win_days)

    if (is.null(result)) { message("  No p95 computed; skipping."); next }

    setnames(result, "id", "STATION")
    for (cc in out_cols) if (!cc %in% names(result)) result[, (cc) := NA_real_]

    fwrite(result[, ..out_cols], out_csv,
           append = !first_write, col.names = first_write)
    first_write <- FALSE

    rm(list = intersect(ls(), c("hist_dt", "main_dt", "dt_chunk", "result")))
    gc()
  }

  if (file.exists(out_csv)) {
    message("Compressing → ", out_gz)
    system2("gzip", c("-f", out_csv))
    message(sprintf("[DONE] Outdoor p95 → %s", out_gz))
  } else {
    warning("No output CSV produced; nothing to gzip.")
  }
}
