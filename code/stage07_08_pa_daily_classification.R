## =============================================================================
## stage07_08_pa_daily_classification.R
## MS1 pipeline — Stages 7 and 8 (PurpleAir)
##
## Stage 7: Daily aggregation of per-year hourly integration files.
## Stage 8: Hazard classification (hierarchical algorithm, MS1 §2.3).
##
## Aligned with stage07_08_daily_classification.R (AirNow); key differences:
##   - STATION replaces AQSID
##   - Year loop over per-year stage06 files (not a single all-years file)
##   - Smoke/fire uses density + frd_d_s (same as AirNow; from stage02_pa)
##     → Wildfire: AOD > cln.thr AND (!is.na(density) | !is.na(frd_d_s))
##     → Baseline: is.na(density) & is.na(frd_d_s) & is.na(EVENT_ID) & ...
##   - No AQSID_rev join logic
##   - Classification parameters from config_pa.R (inherited from config.R)
##
## Temperature source (Heat categories and the Baseline temperature condition)
##   GridMET tmmx and its p95 where GridMET is available; Daymet tmax and its
##   p95 where it is not (Alaska, Hawaii, Puerto Rico). Drought (SPEI) is
##   GridMET only. Same rule as AirNow stage07_08.
##
## Day definition
##   `date` comes from Stage 6 and is the LOCAL STANDARD date of each hour
##   (UTC + GMT_Offset, no daylight saving): days run midnight to midnight
##   local standard time, as in EPA's 24-hour PM2.5 convention. Hourly pools
##   are keyed by the UTC hour (`datetime`); daily records by `date`.
##   GMT_Offset (one value per sensor) is carried into the daily output.
##
## Usage:
##   Rscript stage07_08_pa_daily_classification.R outdoor
##   Rscript stage07_08_pa_daily_classification.R indoor
##
## Input:   per-year purpleair_{type}_integrated_{year}.csv.gz (Stage 6 output)
## Output:  purpleair_{type}_daily_classified.csv.gz (DIR_MERGE)
## =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

source("config_pa.R")

## ---------------------------------------------------------------------------
## 1. Parse argument
## ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
TYPE <- if (length(args) >= 1) tolower(trimws(args[1])) else stop(
  "Usage: Rscript stage07_08_pa_daily_classification.R <outdoor|indoor>"
)
if (!TYPE %in% c("outdoor", "indoor"))
  stop("type must be 'outdoor' or 'indoor'")

YEARS <- 2020:2025

cln.thr      <- CLN_THR
pm_max       <- PM_MAX
min_hours_pm <- MIN_HOURS_PM
for (.p in c("PM_FLOOR", "HEAT_MIN_DAYS"))
  if (!exists(.p)) stop(.p, " must be defined in config.R")
pm_floor      <- PM_FLOOR
heat_min_days <- HEAT_MIN_DAYS
heat_abs_thr_c <- HEAT_ABS_THR_K - 273.15   # absolute heat floor for Daymet tmax (deg C)
metric_cols  <- PA_METRIC_COLS

outfile_classified <- if (TYPE == "outdoor")
  FILE_PA_DAILY_CLASSIFIED_OUTDOOR else FILE_PA_DAILY_CLASSIFIED_INDOOR

if (!dir.exists(DIR_MERGE)) dir.create(DIR_MERGE, recursive = TRUE)
message(sprintf("Stage 07/08 PA | type: %s", TYPE))

## ---------------------------------------------------------------------------
## 2. Helpers (identical logic to AirNow stage07_08; STATION replaces AQSID)
## ---------------------------------------------------------------------------
make_hazard_days <- function(dt_hourly, label) {
  if (!nrow(dt_hourly)) return(data.table())
  unique(dt_hourly[, .(STATION, date, hazard_cat = label)])   # local standard date
}

filter_duration <- function(haz_dt, min_days) {
  if (!nrow(haz_dt)) return(haz_dt)
  d <- copy(haz_dt)
  setorder(d, STATION, date)
  d[, gap       := as.integer(date - shift(date)), by = STATION]
  d[, new_spell := is.na(gap) | gap > 1L]
  d[, spell_id  := cumsum(new_spell), by = STATION]
  spell_len <- d[, .(len = .N), by = .(STATION, spell_id)]
  d <- d[spell_len, on = .(STATION, spell_id)][len >= min_days]
  d[, c("gap", "new_spell", "spell_id", "len") := NULL]
  d[]
}

remove_assigned <- function(rem_dt, assigned_dt) {
  if (!nrow(assigned_dt)) return(rem_dt)
  keys <- unique(assigned_dt[, .(STATION, datetime)])
  setkey(keys, STATION, datetime)
  rem_dt[!keys]
}

## ---------------------------------------------------------------------------
## 3. Year loop: accumulate daily records
## ---------------------------------------------------------------------------
daily_list <- vector("list", length(YEARS))

for (yi in seq_along(YEARS)) {
  yr <- YEARS[yi]
  infile <- sprintf(
    if (TYPE == "outdoor") PA_FILE_INTEGRATED_OUTDOOR_TPL else PA_FILE_INTEGRATED_INDOOR_TPL,
    yr
  )

  if (!file.exists(infile)) {
    message(sprintf("  [%d] Missing integrated file, skipping: %s", yr, basename(infile)))
    next
  }

  message(sprintf("\n===== Year %d =====", yr))
  input_yr <- fread(infile, na.strings = c("", "NA"))
  setDT(input_yr)

  ## Required columns sanity check (fail fast if Stage 6 schema changed)
  need_cols <- c("STATION", "datetime", "date", "GMT_Offset", "PM25", "AOD_047",
                 "density", "frd_d_s", "EVENT_ID", "EVENT_TYPE",
                 "tmmx", "tmmx_p95_1991_2020", "tmax", "tmax_p95_1991_2020")
  miss <- setdiff(need_cols, names(input_yr))
  if (length(miss))
    stop(sprintf("  [%d] Missing required columns in integrated file: %s",
                 yr, paste(miss, collapse = ", ")))

  input_yr[, STATION  := as.character(STATION)]
  ## datetime: fread reads the ISO-8601 UTC timestamps as POSIXct; parse
  ## explicitly if they arrive as text (as.POSIXct() without a format keeps
  ## only the date of "2020-01-01T05:00:00Z" in some R versions).
  if (!inherits(input_yr$datetime, "POSIXct"))
    input_yr[, datetime := ymd_hms(datetime, tz = "UTC")]
  input_yr[, date := as.IDate(date)]                 # local standard date (Stage 6)
  input_yr[PM25 < pm_floor, PM25 := NA_real_]

  ## -----------------------------------------------------------------------
  ## Temperature indicators (Heat categories and Baseline condition 6);
  ## identical to AirNow stage07_08.
  ##   GridMET  where tmmx is available: tmmx (K) against tmmx_p95_1991_2020
  ##            and HEAT_ABS_THR_K.
  ##   Daymet   where tmmx is missing (Alaska, Hawaii, Puerto Rico, points
  ##            outside the GridMET grid): tmax (deg C) against
  ##            tmax_p95_1991_2020 and the same floor in deg C.
  ##   heat_hr  daily maximum above its p95 and above the absolute floor
  ##   temp_ok  Baseline temperature condition: daily maximum <= its p95, or
  ##            no temperature from either source
  ## Drought (spei90d) is GridMET only. No column is added to the output.
  ## -----------------------------------------------------------------------
  input_yr[, heat_hr := fcoalesce(
    fifelse(!is.na(tmmx),
            tmmx > tmmx_p95_1991_2020 & tmmx > HEAT_ABS_THR_K,
            tmax > tmax_p95_1991_2020 & tmax > heat_abs_thr_c),
    FALSE)]
  input_yr[, temp_ok := fcoalesce(
    fifelse(!is.na(tmmx),
            tmmx <= tmmx_p95_1991_2020,
            is.na(tmax) | tmax <= tmax_p95_1991_2020),
    FALSE)]

  message(sprintf("  [%d] Temperature source (sensor-hours with PM2.5):", yr))
  print(input_yr[!is.na(PM25),
                 .(sensor_hours = .N, heat_hr = sum(heat_hr), sensors = uniqueN(STATION)),
                 keyby = .(source = fifelse(!is.na(tmmx), "GridMET",
                                    fifelse(!is.na(tmax), "Daymet", "none")))])

  setkey(input_yr, STATION, datetime)

  ## -----------------------------------------------------------------------
  ## Stage 8: hourly hazard pool assignment
  ##
  ## Smoke/fire columns from stage02_pa (via stage06_pa):
  ##   density  — "Heavy" / "Medium" / "Light" / NA  (character)
  ##   frd_d_s  — semicolon-separated FIRED event IDs / NA  (character)
  ## Matches AirNow stage07_08 exactly: !is.na(density) | !is.na(frd_d_s)
  ## -----------------------------------------------------------------------

  ## Master hourly hazard pool: PM2.5 present + at least one hazard indicator
  all_disaster_hr <- input_yr[
    !is.na(PM25) &
      (!is.na(density) | !is.na(frd_d_s) | !is.na(EVENT_ID) | !is.na(Volcano_Number) |
         heat_hr)
  ]

  rem <- copy(all_disaster_hr)
  setkey(rem, STATION, datetime)

  ## Single-hazard base pools
  ## Pool order: Volcano > Wildfire > Dust
  ##   Volcano first: vog is optically indistinguishable from smoke; eruption
  ##     days must be assigned before Wildfire can claim them.
  ##   Wildfire before Dust: days with real smoke AND dust record go to Wildfire.

  ## 1) Volcanic activity: elevated AOD + active GVP eruption within buffer
  all_volcano_hr <- rem[AOD_047 > cln.thr & !is.na(Volcano_Number)]
  rem <- remove_assigned(rem, all_volcano_hr)

  ## 2) Wildfire: elevated AOD + HMS smoke plume or FIRED detection
  all_wildfire_hr <- rem[AOD_047 > cln.thr & (!is.na(density) | !is.na(frd_d_s))]
  rem <- remove_assigned(rem, all_wildfire_hr)

  ## 3) Dust storm: elevated AOD + NOAA Storm Events dust storm record
  all_dust_hr <- rem[
    AOD_047 > cln.thr & grepl("Dust Storm", EVENT_TYPE, ignore.case = TRUE)
  ]
  rem <- remove_assigned(rem, all_dust_hr)

  ## 4) Extreme heat: daily maximum temperature exceeds the station-specific
  ##    p95 and the absolute floor (heat_hr; GridMET, or Daymet where GridMET
  ##    is missing)
  all_heat_hr <- rem[heat_hr == TRUE]
  rem <- remove_assigned(rem, all_heat_hr)

  ## 5) Tornado
  all_tornado_hr <- rem[grepl("Tornado", EVENT_TYPE, ignore.case = TRUE)]
  rem <- remove_assigned(rem, all_tornado_hr)

  ## 6) Hurricane
  all_hurricane_hr <- rem[grepl("Hurricane", EVENT_TYPE, ignore.case = TRUE)]
  rem <- remove_assigned(rem, all_hurricane_hr)

  ## 7) Tropical storm
  all_tropical_hr <- rem[grepl("Tropical", EVENT_TYPE, ignore.case = TRUE)]
  rem <- remove_assigned(rem, all_tropical_hr)

  ## 8) Flood (exclusive remainder of storm events)
  all_flood_hr <- rem[grepl("Flood", EVENT_TYPE, ignore.case = TRUE)]

  ## Compound pools (derived from base pools above)
  all_wildfire_heat_drought_hr <- all_wildfire_hr[
    heat_hr == TRUE &
    !is.na(spei90d) & spei90d <= SPEI_DROUGHT_THR
  ]
  all_wildfire_heat_hr <- all_wildfire_hr[heat_hr == TRUE]
  all_wildfire_drought_hr <- all_wildfire_hr[
    !is.na(spei90d) & spei90d <= SPEI_DROUGHT_THR
  ]
  all_heat_drought_hr <- all_heat_hr[
    !is.na(spei90d) & spei90d <= SPEI_DROUGHT_THR
  ]

  ## -----------------------------------------------------------------------
  ## Stage 7: daily aggregation (all hours, including non-hazard)
  ## -----------------------------------------------------------------------
  keep_sdcols <- intersect(setdiff(metric_cols, "PM25"), names(input_yr))

  daily_yr <- input_yr[
    ,
    c(
      .(n_hours_pm25 = sum(!is.na(PM25)),
        PM25         = mean(PM25, na.rm = TRUE)),
      if (length(keep_sdcols)) lapply(.SD, mean, na.rm = TRUE) else NULL
    ),
    by      = .(STATION, date, GMT_Offset),
    .SDcols = keep_sdcols
  ][n_hours_pm25 >= min_hours_pm & !is.na(PM25) & PM25 <= pm_max]

  ## -----------------------------------------------------------------------
  ## Stage 8 (continued): assign daily hazard category
  ## -----------------------------------------------------------------------
  prio_levels <- c(
    "Volcano",
    "Wildfire+Heat+Drought", "Wildfire+Heat", "Wildfire+Drought",
    "Wildfire", "Dust",
    "Heat+Drought", "Heat",
    "Tornado", "Hurricane", "TropicalStorm", "Flood"
  )

  hazard_days_all <- rbindlist(list(
    make_hazard_days(all_wildfire_heat_drought_hr, "Wildfire+Heat+Drought"),
    make_hazard_days(all_wildfire_heat_hr,         "Wildfire+Heat"),
    make_hazard_days(all_wildfire_drought_hr,      "Wildfire+Drought"),
    filter_duration(make_hazard_days(all_heat_drought_hr, "Heat+Drought"), heat_min_days),
    make_hazard_days(all_wildfire_hr,              "Wildfire"),
    make_hazard_days(all_volcano_hr,               "Volcano"),
    make_hazard_days(all_dust_hr,                  "Dust"),
    filter_duration(make_hazard_days(all_heat_hr,  "Heat"), heat_min_days),
    make_hazard_days(all_tornado_hr,               "Tornado"),
    make_hazard_days(all_hurricane_hr,             "Hurricane"),
    make_hazard_days(all_tropical_hr,              "TropicalStorm"),
    make_hazard_days(all_flood_hr,                 "Flood")
  ), use.names = TRUE, fill = TRUE)

  daily_yr[, hazard_cat := NA_character_]
  if (nrow(hazard_days_all)) {
    hazard_days_all[, prio := match(hazard_cat, prio_levels)]
    setorder(hazard_days_all, STATION, date, prio)
    hazard_days_all <- hazard_days_all[!duplicated(hazard_days_all[, .(STATION, date)])]
    hazard_days_all[, prio := NULL]
    setkey(hazard_days_all, STATION, date)
    daily_yr[hazard_days_all, on = .(STATION, date), hazard_cat := i.hazard_cat]
  }

  ## -----------------------------------------------------------------------
  ## Baseline: independent positive classification (NOT the residual of hazards)
  ##
  ## Conditions (identical to AirNow §2.3, using density/frd_d_s):
  ##   1. No HMS smoke plume (density is NA)
  ##   2. No FIRED fire detection (frd_d_s is NA)
  ##   3. No NOAA Storm Event (EVENT_ID is NA)
  ##   4. No GVP volcanic activity (Volcano_Number is NA)
  ##   5. AOD_047 missing OR <= cln.thr
  ##   6. Daily maximum temperature <= station-specific p95 threshold
  ##      (temp_ok): GridMET tmmx where available, otherwise Daymet tmax; the
  ##      condition is met where neither source has a temperature
  ## -----------------------------------------------------------------------
  baseline_hr <- input_yr[
    !is.na(PM25) &
    is.na(density) & is.na(frd_d_s) &
    is.na(EVENT_ID) & is.na(Volcano_Number) &
    (is.na(AOD_047) | AOD_047 <= cln.thr) &
    temp_ok
  ]

  keep_sdcols_bl <- intersect(setdiff(metric_cols, "PM25"), names(baseline_hr))
  baseline_daily_yr <- baseline_hr[
    ,
    c(
      .(n_hours_pm25 = sum(!is.na(PM25)),
        PM25         = mean(PM25, na.rm = TRUE)),
      if (length(keep_sdcols_bl)) lapply(.SD, mean, na.rm = TRUE) else NULL
    ),
    by      = .(STATION, date),
    .SDcols = keep_sdcols_bl
  ][n_hours_pm25 >= min_hours_pm & !is.na(PM25) & PM25 <= pm_max]

  if (nrow(baseline_daily_yr)) {
    bl_keys <- unique(baseline_daily_yr[, .(STATION, date)])
    daily_yr[bl_keys, on = .(STATION, date),
             hazard_cat := fifelse(is.na(hazard_cat), "Baseline", hazard_cat)]
  }

  daily_list[[yi]] <- daily_yr
  message(sprintf("  [%d] Daily rows: %d | Hazard mix:", yr, nrow(daily_yr)))
  print(daily_yr[, .N, by = hazard_cat][order(-N)])

  rm(input_yr, all_disaster_hr, rem,
     all_wildfire_hr, all_volcano_hr, all_dust_hr, all_heat_hr,
     all_tornado_hr, all_hurricane_hr, all_tropical_hr, all_flood_hr,
     all_wildfire_heat_drought_hr, all_wildfire_heat_hr,
     all_wildfire_drought_hr, all_heat_drought_hr,
     hazard_days_all, baseline_hr, baseline_daily_yr)
  gc()
}

## ---------------------------------------------------------------------------
## 4. Combine across years and write
## ---------------------------------------------------------------------------
daily_all <- rbindlist(Filter(Negate(is.null), daily_list),
                       use.names = TRUE, fill = TRUE)

fwrite(daily_all, outfile_classified, compress = "gzip")
message("\nTotal sensor-days written: ", nrow(daily_all))
message("Overall hazard mix:")
print(daily_all[, .N, by = hazard_cat][order(-N)])
message("[DONE] Wrote: ", outfile_classified)
