## =============================================================================
## stage07_08_daily_classification.R
## MS1 pipeline — Stages 7 and 8
##
## Stage 7: Daily aggregation
##   Aggregates the master hourly integration table (Stage 6 output) to daily
##   sensor-level records. Applies minimum hourly completeness filter
##   (MIN_HOURS_PM) and PM2.5 sanity cap (PM_MAX).
##
## Stage 8: Hazard classification
##   Assigns each sensor-day to one of 12 mutually exclusive hazard categories
##   or "Baseline" using the hierarchical algorithm described in MS1 Section 2.3.
##   Hazard indicators are evaluated at the hourly level; each daily record is
##   assigned the single highest-priority category active on that day.
##   No spell construction or pre/post windows are applied here — those are
##   MS2 analytical design choices applied to this dataset downstream.
##
## Temperature source (Heat categories and the Baseline temperature condition)
##   GridMET tmmx and its p95 where GridMET is available; Daymet tmax and its
##   p95 where it is not (Alaska, Hawaii, Puerto Rico). Drought (SPEI) is
##   GridMET only. See "Temperature indicators" below.
##
## Day definition
##   `date` comes from Stage 6 and is the LOCAL STANDARD date of each hour
##   (UTC + GMT_Offset, no daylight saving): days run midnight to midnight
##   local standard time, as in EPA's 24-hour PM2.5 convention. Hourly pools
##   are keyed by the UTC hour (`datetime`); daily records by `date`.
##   GMT_Offset (one value per site) is carried into the daily output.
##
## Output (written to DIR_MERGE):
##   airnow_daily_classified.csv.gz  — every sensor-day with hazard_cat column
##                                     (12 hazard categories + "Baseline")
##
## Inputs:  FILE_PM25_AOD_MET (Stage 6 output)
## Follows: stage06_master_hourly_integration.R
## Precedes: stage09_attribute_join.R
## =============================================================================

rm(list = ls())

library(data.table)
library(lubridate)
library(sf)
library(stringr)
library(dplyr)

source("config.R")

## ------------------------- Run options --------------------------------------
## All classification parameters come from config.R:
##   CLN_THR, PM_MAX, MIN_HOURS_PM, HEAT_ABS_THR_K, SPEI_DROUGHT_THR,
##   PM_FLOOR (hourly values below it are set to NA), HEAT_MIN_DAYS
##   (consecutive days required for Heat and Heat+Drought).
## Parameter sensitivity runs: ms1_table7_sensitivity_analyses_rev.R.

infile       <- FILE_PM25_AOD_MET

cln.thr      <- CLN_THR
pm_max       <- PM_MAX
min_hours_pm <- MIN_HOURS_PM
for (.p in c("PM_FLOOR", "HEAT_MIN_DAYS"))
  if (!exists(.p)) stop(.p, " must be defined in config.R")
pm_floor      <- PM_FLOOR
heat_min_days <- HEAT_MIN_DAYS

if (!dir.exists(DIR_MERGE)) dir.create(DIR_MERGE, recursive = TRUE)

outfile_classified <- FILE_DAILY_CLASSIFIED

## Metric columns carried through daily aggregation (defined in config.R)
metric_cols <- METRIC_COLS

## =============================================================================
## STAGE 7: Daily aggregation
## =============================================================================

message("Reading: ", infile)
input_all <- fread(infile, na.strings = c("", "NA"))
setDT(input_all)

# Required fields sanity check (fail fast if Stage 6 schema changed)
need_cols <- c("AQSID", "datetime", "date", "GMT_Offset",
               "PM25", "AOD_047", "EVENT_ID", "EVENT_TYPE",
               "tmmx", "tmmx_p95_1991_2020", "tmax", "tmax_p95_1991_2020")
miss <- setdiff(need_cols, names(input_all))
if (length(miss)) stop("Missing required columns in infile: ", paste(miss, collapse = ", "))

# Housekeeping
## datetime: fread reads the ISO-8601 UTC timestamps as POSIXct; parse
## explicitly if they arrive as text (as.POSIXct() without a format keeps only
## the date of "2020-01-01T05:00:00Z" in some R versions).
if (!inherits(input_all$datetime, "POSIXct"))
  input_all[, datetime := ymd_hms(datetime, tz = "UTC")]
input_all[, date := as.IDate(date)]                 # local standard date (Stage 6)
input_all[PM25 < pm_floor, PM25 := NA_real_]

## =============================================================================
## Temperature indicators (Heat categories and Baseline condition 6)
## =============================================================================
## Source, by sensor-hour:
##   GridMET  where tmmx is available: tmmx (K) against tmmx_p95_1991_2020 (K)
##            and HEAT_ABS_THR_K.
##   Daymet   where tmmx is missing: tmax (deg C) against tmax_p95_1991_2020
##            (deg C) and the same floor in deg C. GridMET covers CONUS only, so
##            this applies to Alaska, Hawaii, Puerto Rico, and any point outside
##            the GridMET grid.
## Both p95 tables come from stage03 (same +/-7-day window and 1991-2020 period).
##   heat_hr  daily maximum above its p95 and above the absolute floor
##   temp_ok  Baseline temperature condition: daily maximum <= its p95, or no
##            temperature from either source
## Drought (spei90d) is GridMET only: Wildfire+Drought, Heat+Drought and
## Wildfire+Heat+Drought are not assigned where GridMET is missing.
## No column is added to the output; rows classified with Daymet are those with
## tmmx missing and tmax present.
heat_abs_thr_c <- HEAT_ABS_THR_K - 273.15

input_all[, heat_hr := fcoalesce(
  fifelse(!is.na(tmmx),
          tmmx > tmmx_p95_1991_2020 & tmmx > HEAT_ABS_THR_K,
          tmax > tmax_p95_1991_2020 & tmax > heat_abs_thr_c),
  FALSE)]
input_all[, temp_ok := fcoalesce(
  fifelse(!is.na(tmmx),
          tmmx <= tmmx_p95_1991_2020,
          is.na(tmax) | tmax <= tmax_p95_1991_2020),
  FALSE)]

message("Temperature source (sensor-hours with PM2.5; heat_hr = above p95 and floor):")
print(input_all[!is.na(PM25),
                .(sensor_hours = .N, heat_hr = sum(heat_hr), sensors = uniqueN(AQSID)),
                keyby = .(source = fifelse(!is.na(tmmx), "GridMET",
                                   fifelse(!is.na(tmax), "Daymet", "none")))])

setkey(input_all, AQSID, datetime)

## =============================================================================
## STAGE 8: Hazard classification — hourly pool assignment
## =============================================================================

# Master hourly hazard pool: PM2.5 present + at least one hazard indicator
all_disaster_hr <- input_all[
  !is.na(PM25) &
    (!is.na(density) | !is.na(frd_d_s) | !is.na(EVENT_ID) | !is.na(Volcano_Number) |
       heat_hr)
]

rem <- copy(all_disaster_hr)
setkey(rem, AQSID, datetime)

# Helper: remove already-assigned rows from the remainder pool
remove_assigned <- function(rem_dt, assigned_dt) {
  if (!nrow(assigned_dt)) return(rem_dt)
  assigned_keys <- unique(assigned_dt[, .(AQSID, datetime)])
  setkey(assigned_keys, AQSID, datetime)
  rem_dt[!assigned_keys]
}

# Single-hazard base pools (evaluated in priority order; each row removed once assigned)
#
# Pool assignment order rationale (smoke/AOD/Volcano_Number are daily signals
# broadcast to all 24 hours — pool assignment is effectively a day-level decision):
#   Volcano > Wildfire: HMS/MAIAC cannot distinguish vog from wildfire smoke
#     optically. Assigning Volcano first ensures Big Island eruption days are
#     not misclassified as Wildfire.
#   Wildfire > Dust: a day with real smoke AND a dust event record goes to
#     Wildfire. Dust is only reached when no smoke/FIRED signal is present.
#
# 1) Volcanic activity: elevated AOD + active GVP eruption within buffer
all_volcano_hr <- rem[AOD_047 > cln.thr & !is.na(Volcano_Number)]
rem <- remove_assigned(rem, all_volcano_hr)

# 2) Wildfire: elevated AOD + HMS smoke plume or FIRED detection
all_wildfire_hr <- rem[AOD_047 > cln.thr & (!is.na(density) | !is.na(frd_d_s))]
rem <- remove_assigned(rem, all_wildfire_hr)

# 3) Dust storm: elevated AOD + NOAA Storm Events dust storm record
all_dust_hr <- rem[
  AOD_047 > cln.thr &
    grepl("Dust Storm", EVENT_TYPE, ignore.case = TRUE)
]
rem <- remove_assigned(rem, all_dust_hr)

# 4) Extreme heat: daily maximum temperature exceeds the station-specific p95
#    and the absolute floor (heat_hr; GridMET, or Daymet where GridMET is missing)
#    (persistence of HEAT_MIN_DAYS consecutive days enforced via filter_duration)
all_heat_hr <- rem[heat_hr == TRUE]
rem <- remove_assigned(rem, all_heat_hr)

# 5) Tornado
all_tornado_hr <- rem[grepl("Tornado", EVENT_TYPE, ignore.case = TRUE)]
rem <- remove_assigned(rem, all_tornado_hr)

# 6) Hurricane
all_hurricane_hr <- rem[grepl("Hurricane", EVENT_TYPE, ignore.case = TRUE)]
rem <- remove_assigned(rem, all_hurricane_hr)

# 7) Tropical storm
all_tropical_hr <- rem[grepl("Tropical", EVENT_TYPE, ignore.case = TRUE)]
rem <- remove_assigned(rem, all_tropical_hr)

# 8) Flood (exclusive remainder of storm events)
all_flood_hr <- rem[grepl("Flood", EVENT_TYPE, ignore.case = TRUE)]

# Compound hazard pools (derived from base pools above)
all_wildfire_heat_drought_hr <- all_wildfire_hr[
  heat_hr == TRUE &
  (!is.na(spei90d) & spei90d <= SPEI_DROUGHT_THR)
]
all_wildfire_heat_hr    <- all_wildfire_hr[heat_hr == TRUE]
all_wildfire_drought_hr <- all_wildfire_hr[!is.na(spei90d) & spei90d <= SPEI_DROUGHT_THR]
all_heat_drought_hr     <- all_heat_hr[!is.na(spei90d) & spei90d <= SPEI_DROUGHT_THR]

## Daily aggregation (Stage 7) — all hourly records including non-hazard
keep_sdcols <- intersect(setdiff(metric_cols, "PM25"), names(input_all))

daily_all <- input_all[
  ,
  c(
    .(n_hours_pm25 = sum(!is.na(PM25)),
      PM25         = mean(PM25, na.rm = TRUE)),
    if (length(keep_sdcols)) lapply(.SD, mean, na.rm = TRUE) else NULL
  ),
  by    = .(AQSID, date, GMT_Offset),
  .SDcols = keep_sdcols
][ n_hours_pm25 >= min_hours_pm & !is.na(PM25) & PM25 <= pm_max ]

## =============================================================================
## STAGE 8 (continued): Daily hazard category assignment
## =============================================================================
## Each sensor-day is assigned to the highest-priority hazard category
## active on that day based on hourly indicator pools constructed above.
## No spell construction or pre/post windows are applied — these are MS2
## analytical decisions.  filter_duration() is retained only for Heat and
## Heat+Drought because the persistence requirement (HEAT_MIN_DAYS) is part of the MS1
## hazard *definition* (§2.3), not an event-study design choice.

make_hazard_days <- function(dt_hourly, label) {
  if (!nrow(dt_hourly)) return(data.table())
  unique(dt_hourly[, .(AQSID, date, hazard_cat = label)])   # local standard date
}

filter_duration <- function(haz_dt, min_days) {
  ## Keep only sensor × spells where the hazard persists for >= min_days
  ## consecutive days (with no gap bridging — purely contemporaneous).
  if (!nrow(haz_dt)) return(haz_dt)
  d <- copy(haz_dt)
  setorder(d, AQSID, date)
  d[, gap      := as.integer(date - shift(date)), by = AQSID]
  d[, new_spell := is.na(gap) | gap > 1L]
  d[, spell_id  := cumsum(new_spell), by = AQSID]
  spell_len <- d[, .(len = .N), by = .(AQSID, spell_id)]
  d <- d[spell_len, on = .(AQSID, spell_id)][len >= min_days]
  d[, c("gap", "new_spell", "spell_id", "len") := NULL]
  d[]
}

## Priority order (highest to lowest)
##
## Volcano leads: consistent with pool assignment; eruption days must not be
##   overridden even as a safety net for daily aggregation edge cases.
## Wildfire compounds follow: derived subsets of all_wildfire_hr, mutually
##   exclusive with Volcano by pool assignment.
## Wildfire before Heat+Drought: Wildfire is the stronger PM2.5 driver.
## Dust before Heat+Drought: AOD-confirmed dust event takes priority over
##   meteorological-only classification.
prio_levels <- c(
  "Volcano",
  "Wildfire+Heat+Drought",
  "Wildfire+Heat",
  "Wildfire+Drought",
  "Wildfire",
  "Dust",
  "Heat+Drought",
  "Heat",
  "Tornado",
  "Hurricane",
  "TropicalStorm",
  "Flood"
)

hazard_days_list <- list(
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
)

hazard_days_all <- rbindlist(hazard_days_list, use.names = TRUE, fill = TRUE)

## Assign priority rank; when the same sensor-day appears in multiple pools
## (e.g. a Wildfire day that also meets Heat criteria before compound pools
## absorbed it), keep the highest-priority category.
if (nrow(hazard_days_all)) {
  hazard_days_all[, prio := match(hazard_cat, prio_levels)]
  setorder(hazard_days_all, AQSID, date, prio)
  hazard_days_all <- hazard_days_all[!duplicated(hazard_days_all[, .(AQSID, date)])]
  hazard_days_all[, prio := NULL]
  setkey(hazard_days_all, AQSID, date)
}

## Join hazard category onto the full daily table (unmatched rows stay NA)
daily_all[, hazard_cat := NA_character_]
if (nrow(hazard_days_all)) {
  daily_all[hazard_days_all, on = .(AQSID, date), hazard_cat := i.hazard_cat]
}

## =============================================================================
## Baseline: independent positive classification (NOT the residual of hazards)
## =============================================================================
## A day qualifies as Baseline only when ALL non-hazard conditions are
## simultaneously satisfied at the hourly level (MS1 §2.3).
## Days that fail a hazard threshold but also fail a Baseline condition
## (e.g. single-day heat, elevated AOD with no smoke/storm) are neither
## a hazard nor a Baseline — they remain NA and are excluded from both pools.
##
## Conditions (from original baseline_hr):
##   1. No HMS smoke plume (density is NA)
##   2. No FIRED fire detection (frd_d_s is NA)
##   3. No NOAA Storm Event (EVENT_ID is NA — covers all event types)
##   4. No GVP volcanic activity (Volcano_Number is NA)
##   5. AOD_047 missing OR <= cln.thr
##   6. Daily maximum temperature <= station-specific p95 threshold (temp_ok):
##      GridMET tmmx where available, otherwise Daymet tmax; the condition is
##      met where neither source has a temperature
baseline_hr <- input_all[
  !is.na(PM25) &
    is.na(density) & is.na(frd_d_s) & is.na(EVENT_ID) & is.na(Volcano_Number) &
    (is.na(AOD_047) | AOD_047 <= cln.thr) &
    temp_ok
]

keep_sdcols_bl <- intersect(setdiff(metric_cols, "PM25"), names(baseline_hr))
baseline_daily <- baseline_hr[
  ,
  c(
    .(n_hours_pm25 = sum(!is.na(PM25)),
      PM25         = mean(PM25, na.rm = TRUE)),
    if (length(keep_sdcols_bl)) lapply(.SD, mean, na.rm = TRUE) else NULL
  ),
  by    = .(AQSID, date),
  .SDcols = keep_sdcols_bl
][ n_hours_pm25 >= min_hours_pm & !is.na(PM25) & PM25 <= pm_max ]

## Tag qualifying baseline days in daily_all (only where hazard_cat is still NA)
if (nrow(baseline_daily)) {
  baseline_keys <- unique(baseline_daily[, .(AQSID, date)])
  daily_all[baseline_keys, on = .(AQSID, date),
            hazard_cat := fifelse(is.na(hazard_cat), "Baseline", hazard_cat)]
}

## Clean up temporary columns used for keying
for (.col in c("start", "end", "row_id")) {
  if (.col %in% names(daily_all)) daily_all[, (.col) := NULL]
}

## =============================================================================
## Write output
## =============================================================================
fwrite(daily_all, outfile_classified, compress = "gzip")

message("Total sensor-days written: ", nrow(daily_all))
message("Hazard mix:")
print(daily_all[, .N, by = hazard_cat][order(-N)])
message("Wrote: ", outfile_classified)
