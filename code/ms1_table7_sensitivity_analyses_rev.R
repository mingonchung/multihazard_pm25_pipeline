## =============================================================================
## ms1_table7_sensitivity_analyses_rev.R
## Section 5.2 — Sensitivity analyses across classification parameters
## (results reported in MS1 Table 7)
##
## classify_daily() below is a copy of the stage07_08 classification with the
## parameters as arguments. It must stay identical to
## stage07_08_daily_classification.R; with default arguments it reproduces the
## pipeline's daily classes for US sensors (Stage 9 filter applied here).
## Day definition: `date` from Stage 6 = local standard date (UTC + GMT_Offset).
## PM_FLOOR and HEAT_MIN_DAYS come from config.R and are not perturbed.
## Temperature source, as in stage07_08: GridMET tmmx and its p95 where GridMET
## is available, Daymet tmax and its p95 where it is not (Alaska, Hawaii, and
## points outside the GridMET grid). The heat-floor run applies the same floor
## to both sources.
## Sensors counted: US sensors of the Stage 9 file, without US territories
## (CONUS, Alaska, and Hawaii), as in the other MS1 tables and figures.
## PM2.5 statistics per category and run: mean (SD) and median of the daily
## means; percentage change from the default run for both. Table 7 reports the
## change in the median (as Table 5 reports medians); the mean columns are kept.
##
## Run modes
##   Rscript ms1_table7_sensitivity_analyses_rev.R
##       all runs: the 17 parameter runs (checkpoint written to the output
##       file), then the two GVP buffer runs
##   Rscript ms1_table7_sensitivity_analyses_rev.R --gvp-only
##       reads the 17 parameter runs from the checkpoint already in the output
##       file and runs the two GVP buffer runs only (after a job that ended
##       during the GVP runs). The checkpoint must match the current dataset.
##
## Run location: HPC (/data001/projects/chungmin)
## Source config: config.R  (defines all FILE_* and DIR_* paths)
##
## Primary input:
##   FILE_PM25_AOD_MET  →  extraction/output/merge/
##                          airnow_pm25_aod_hms_fired_storm_gvp_met.csv.gz
##
## GVP buffer sensitivity inputs (re-derived, no distance in FILE_GVP_OUT):
##   FILE_GVP_XLSX      →  aqdata/gvp/GVP_Eruption_List_Holocene_20260424.xlsx
##   FILE_GVP_COORDS    →  aqdata/gvp/GVP_Volcano_List_Holocene_202610091715.xlsx (coordinates)
##   FILE_AIRNOW_LOCS   →  aqdata/airnow/airnow_aqsid_location_rev_010926.csv
##   FILE_USCAN_SHP     →  aqdata/uscan/us_canada.shp
##   FILE_AIRNOW_USATTRS_SHP → (used for US-only spatial filter)
##   FILE_DAILY_INTEGRATED   → (Stage 9 file; check that the default run
##                              reproduces the dataset's category counts)
##
## Output:
##   DIR_MS1_OUT/ms1_table7_sensitivity_results.csv
##     (plain CSV — analysis output, not pipeline output)
## =============================================================================

rm(list = ls())

library(data.table)
library(lubridate)
library(sf)
library(readxl)
library(dplyr)
library(stringr)

## ---------------------------------------------------------------------------
## 0. Source pipeline config (all FILE_* / DIR_* / parameter defaults)
## ---------------------------------------------------------------------------
source("config.R")

DIR_MS1_OUT <- file.path(ROOT_DIR, "ms1")
if (!dir.exists(DIR_MS1_OUT)) dir.create(DIR_MS1_OUT, recursive = TRUE)

## Run mode (see header)
RESUME_FROM_GVP <- "--gvp-only" %in% commandArgs(trailingOnly = TRUE)

## State FIPS codes of US territories (AS, GU, MP, PR, VI): not counted in MS1
## tables and figures (same codes as in the figure scripts)
excl_terr <- c("60", "66", "69", "72", "78")

for (.p in c("PM_FLOOR", "HEAT_MIN_DAYS"))
  if (!exists(.p)) stop(.p, " must be defined in config.R")

## ---------------------------------------------------------------------------
## 1. Verify input files exist before reading (fail fast on HPC)
## ---------------------------------------------------------------------------
required_files <- c(
  FILE_PM25_AOD_MET,
  FILE_GVP_XLSX,
  FILE_GVP_COORDS,
  FILE_AIRNOW_LOCS,
  FILE_USCAN_SHP,
  FILE_AIRNOW_USATTRS_SHP
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop("Missing required input files:\n",
       paste(" ", missing_files, collapse = "\n"))
}

## ---------------------------------------------------------------------------
## 2a. Load master hourly integration file
## ---------------------------------------------------------------------------
message("Reading: ", FILE_PM25_AOD_MET)
hourly <- fread(FILE_PM25_AOD_MET, na.strings = c("", "NA"),
                colClasses = list(character = "AQSID"))   # keep IDs as text (GVP join below)
message("Rows loaded: ", format(nrow(hourly), big.mark = ","))

need_cols <- c("AQSID", "datetime", "date", "PM25", "AOD_047",
               "density", "frd_d_s", "EVENT_ID", "EVENT_TYPE",
               "Volcano_Number", "tmmx", "tmmx_p95_1991_2020",
               "tmax", "tmax_p95_1991_2020", "spei90d")
miss <- setdiff(need_cols, names(hourly))
if (length(miss)) stop("Missing columns in FILE_PM25_AOD_MET: ", paste(miss, collapse = ", "))

## datetime: fread reads the ISO-8601 UTC timestamps as POSIXct; parse
## explicitly if they arrive as text (as.POSIXct() without a format keeps only
## the date of "2020-01-01T05:00:00Z" in some R versions).
if (!inherits(hourly$datetime, "POSIXct"))
  hourly[, datetime := ymd_hms(datetime, tz = "UTC")]
hourly[, date := as.IDate(date)]                    # local standard date (Stage 6)
hourly[PM25 < PM_FLOOR, PM25 := NA_real_]

## ---------------------------------------------------------------------------
## 2b. Apply US-only spatial filter (matching Stage 9)
## ---------------------------------------------------------------------------
message("Applying US-only spatial filter from: ", FILE_AIRNOW_USATTRS_SHP)

normalize_aqsid <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- dplyr::na_if(x, "'NA")
  x <- dplyr::na_if(x, "NA")
  x <- dplyr::na_if(x, "")
  x <- stringr::str_replace(x, "^'+", "")
  x <- stringr::str_trim(x)
  is_num <- !is.na(x) & stringr::str_detect(x, "^[0-9]+$")
  x[is_num] <- ifelse(
    nchar(x[is_num]) < 9,
    stringr::str_pad(x[is_num], width = 9, side = "left", pad = "0"),
    x[is_num]
  )
  x
}

usattrs <- st_read(FILE_AIRNOW_USATTRS_SHP, quiet = TRUE) |>
  st_transform(4326) |>
  st_drop_geometry() |>
  as.data.table()

idcol <- names(usattrs)[grepl("AQSID", names(usattrs), ignore.case = TRUE)][1]
usattrs[, AQSID_norm := normalize_aqsid(get(idcol))]

## Valid US sensors are those with a non-NA statefp; sensors in US territories
## are left out of the counts (CONUS, Alaska, and Hawaii only)
usattrs[, statefp2 := sprintf("%02d", suppressWarnings(as.integer(as.character(statefp))))]
message(sprintf("Sensors in US territories excluded from the counts: %d",
                usattrs[!is.na(statefp) & statefp2 %in% excl_terr, uniqueN(AQSID_norm)]))
us_aqsids <- unique(usattrs[!is.na(statefp) & !statefp2 %in% excl_terr, AQSID_norm])

## Filter hourly data
hourly[, AQSID_norm := normalize_aqsid(AQSID)]
n_before <- nrow(hourly)
hourly <- hourly[AQSID_norm %in% us_aqsids]
hourly[, AQSID_norm := NULL]
setkey(hourly, AQSID, datetime)

message(sprintf("Filtered to US sensors: %s rows remaining (dropped %s rows)",
                format(nrow(hourly), big.mark=","),
                format(n_before - nrow(hourly), big.mark=",")))

## ---------------------------------------------------------------------------
## 3. Canonical category order
## ---------------------------------------------------------------------------
prio_levels <- c(
  "Volcano", "Wildfire+Heat+Drought", "Wildfire+Heat",
  "Wildfire+Drought", "Wildfire", "Dust",
  "Heat+Drought", "Heat",
  "Tornado", "Hurricane", "TropicalStorm", "Flood",
  "Baseline", "Unclassified (NA)"
)

## ---------------------------------------------------------------------------
## 4. Core classification function
## ---------------------------------------------------------------------------
classify_daily <- function(
    hourly_dt,
    cln_thr          = CLN_THR,
    pm_max           = PM_MAX,
    min_hours_pm     = MIN_HOURS_PM,
    heat_abs_thr_k   = HEAT_ABS_THR_K,
    spei_drought_thr = SPEI_DROUGHT_THR,
    aod_na_as_clean  = TRUE
) {
  keep_met <- intersect(METRIC_COLS[METRIC_COLS != "PM25"], names(hourly_dt))

  ## Temperature indicators (as in stage07_08): GridMET where tmmx is
  ## available, otherwise Daymet tmax (deg C) with the floor converted to deg C.
  ## The two helper columns are written to hourly_dt by reference and are
  ## recomputed on every call (heat_hr depends on heat_abs_thr_k); they are not
  ## in METRIC_COLS and do not reach the daily table.
  heat_abs_thr_c <- heat_abs_thr_k - 273.15
  hourly_dt[, heat_hr := fcoalesce(
    fifelse(!is.na(tmmx),
            tmmx > tmmx_p95_1991_2020 & tmmx > heat_abs_thr_k,
            tmax > tmax_p95_1991_2020 & tmax > heat_abs_thr_c),
    FALSE)]
  hourly_dt[, temp_ok := fcoalesce(
    fifelse(!is.na(tmmx),
            tmmx <= tmmx_p95_1991_2020,
            is.na(tmax) | tmax <= tmax_p95_1991_2020),
    FALSE)]

  all_haz_hr <- hourly_dt[
    !is.na(PM25) &
      (!is.na(density) | !is.na(frd_d_s) | !is.na(EVENT_ID) |
         !is.na(Volcano_Number) |
         heat_hr)
  ]

  rem <- copy(all_haz_hr)
  setkey(rem, AQSID, datetime)

  rm_assigned <- function(rem_dt, asgn_dt) {
    if (!nrow(asgn_dt)) return(rem_dt)
    keys <- unique(asgn_dt[, .(AQSID, datetime)])
    setkey(keys, AQSID, datetime)
    rem_dt[!keys]
  }

  p_volcano  <- rem[AOD_047 > cln_thr & !is.na(Volcano_Number)]
  rem <- rm_assigned(rem, p_volcano)

  p_wildfire <- rem[AOD_047 > cln_thr & (!is.na(density) | !is.na(frd_d_s))]
  rem <- rm_assigned(rem, p_wildfire)

  p_dust     <- rem[AOD_047 > cln_thr & grepl("Dust Storm", EVENT_TYPE, ignore.case = TRUE)]
  rem <- rm_assigned(rem, p_dust)

  p_heat     <- rem[heat_hr == TRUE]
  rem <- rm_assigned(rem, p_heat)

  p_tornado  <- rem[grepl("Tornado",   EVENT_TYPE, ignore.case = TRUE)]
  rem <- rm_assigned(rem, p_tornado)
  p_hurricane <- rem[grepl("Hurricane", EVENT_TYPE, ignore.case = TRUE)]
  rem <- rm_assigned(rem, p_hurricane)
  p_tropical  <- rem[grepl("Tropical",  EVENT_TYPE, ignore.case = TRUE)]
  rem <- rm_assigned(rem, p_tropical)
  p_flood     <- rem[grepl("Flood",     EVENT_TYPE, ignore.case = TRUE)]

  p_wf_hd <- p_wildfire[heat_hr == TRUE & !is.na(spei90d) & spei90d <= spei_drought_thr]
  p_wf_h  <- p_wildfire[heat_hr == TRUE]
  p_wf_d  <- p_wildfire[!is.na(spei90d) & spei90d <= spei_drought_thr]
  p_h_d   <- p_heat[!is.na(spei90d) & spei90d <= spei_drought_thr]

  daily_all <- hourly_dt[
    ,
    c(.(n_hours_pm25 = sum(!is.na(PM25)),
        PM25         = mean(PM25, na.rm = TRUE)),
      if (length(keep_met)) lapply(.SD, mean, na.rm = TRUE) else NULL),
    by      = .(AQSID, date),
    .SDcols = keep_met
  ][n_hours_pm25 >= min_hours_pm & !is.na(PM25) & PM25 <= pm_max]

  mkd <- function(dt, lbl) {
    if (!nrow(dt)) return(data.table())
    unique(dt[, .(AQSID, date, hazard_cat = lbl)])   # local standard date
  }

  flt_dur <- function(dt, min_d = HEAT_MIN_DAYS) {
    if (!nrow(dt)) return(dt)
    d <- copy(dt); setorder(d, AQSID, date)
    d[, gap       := as.integer(date - shift(date)), by = AQSID]
    d[, new_spell := is.na(gap) | gap > 1L]
    d[, spell_id  := cumsum(new_spell), by = AQSID]
    sl <- d[, .(len = .N), by = .(AQSID, spell_id)]
    d  <- d[sl, on = .(AQSID, spell_id)][len >= min_d]
    d[, c("gap", "new_spell", "spell_id", "len") := NULL]
    d[]
  }

  haz_all <- rbindlist(list(
    mkd(p_volcano,  "Volcano"),
    mkd(p_wf_hd,    "Wildfire+Heat+Drought"),
    mkd(p_wf_h,     "Wildfire+Heat"),
    mkd(p_wf_d,     "Wildfire+Drought"),
    mkd(p_wildfire, "Wildfire"),
    mkd(p_dust,     "Dust"),
    flt_dur(mkd(p_h_d,   "Heat+Drought")),
    flt_dur(mkd(p_heat,  "Heat")),
    mkd(p_tornado,  "Tornado"),
    mkd(p_hurricane,"Hurricane"),
    mkd(p_tropical, "TropicalStorm"),
    mkd(p_flood,    "Flood")
  ), fill = TRUE)

  if (nrow(haz_all)) {
    haz_all[, prio := match(hazard_cat, prio_levels)]
    setorder(haz_all, AQSID, date, prio)
    haz_all <- haz_all[!duplicated(haz_all[, .(AQSID, date)])]
    haz_all[, prio := NULL]
    setkey(haz_all, AQSID, date)
  }

  daily_all[, hazard_cat := NA_character_]
  if (nrow(haz_all))
    daily_all[haz_all, on = .(AQSID, date), hazard_cat := i.hazard_cat]

  baseline_hr <- if (aod_na_as_clean) {
    hourly_dt[
      !is.na(PM25) &
        is.na(density) & is.na(frd_d_s) & is.na(EVENT_ID) & is.na(Volcano_Number) &
        (is.na(AOD_047) | AOD_047 <= cln_thr) &
        temp_ok
    ]
  } else {
    hourly_dt[
      !is.na(PM25) &
        is.na(density) & is.na(frd_d_s) & is.na(EVENT_ID) & is.na(Volcano_Number) &
        (!is.na(AOD_047) & AOD_047 <= cln_thr) &
        temp_ok
    ]
  }

  keep_met_bl <- intersect(keep_met, names(baseline_hr))
  baseline_daily <- baseline_hr[
    , c(.(n_hours_pm25 = sum(!is.na(PM25)),
          PM25         = mean(PM25, na.rm = TRUE)),
        if (length(keep_met_bl)) lapply(.SD, mean, na.rm = TRUE) else NULL),
    by      = .(AQSID, date),
    .SDcols = keep_met_bl
  ][n_hours_pm25 >= min_hours_pm & !is.na(PM25) & PM25 <= pm_max]

  if (nrow(baseline_daily)) {
    bkeys <- unique(baseline_daily[, .(AQSID, date)])
    daily_all[bkeys, on = .(AQSID, date),
              hazard_cat := fifelse(is.na(hazard_cat), "Baseline", hazard_cat)]
  }

  daily_all[]
}

## ---------------------------------------------------------------------------
## 5. Summary function
## ---------------------------------------------------------------------------
summarize_run <- function(classified_dt, param_name, param_value, is_default = FALSE) {
  DT <- copy(classified_dt)
  DT[is.na(hazard_cat) | hazard_cat == "", hazard_cat := "Unclassified (NA)"]

  sm <- DT[, .(
    n_sensor_days = .N,
    n_sensors     = uniqueN(AQSID),
    mean_pm25     = round(mean(PM25, na.rm = TRUE), 3),
    sd_pm25       = round(sd(PM25,   na.rm = TRUE), 3),
    median_pm25   = round(median(PM25, na.rm = TRUE), 3)
  ), by = hazard_cat]

  sm[, `:=`(
    param_name  = param_name,
    param_value = as.character(param_value),
    is_default  = is_default
  )]
  sm[]
}

## ---------------------------------------------------------------------------
## 5b. Per-sensor summary: returns one row per sensor × category per run
## ---------------------------------------------------------------------------
summarize_run_by_sensor <- function(classified_dt, param_name, param_value, is_default = FALSE) {
  DT <- copy(classified_dt)
  DT[is.na(hazard_cat) | hazard_cat == "", hazard_cat := "Unclassified (NA)"]

  ## Per-sensor, per-category mean PM2.5
  sensor_sm <- DT[, .(
    sensor_mean_pm25 = mean(PM25, na.rm = TRUE),
    sensor_n_days    = .N
  ), by = .(AQSID, hazard_cat)]

  sensor_sm[, `:=`(
    param_name  = param_name,
    param_value = as.character(param_value),
    is_default  = is_default
  )]
  sensor_sm[]
}

## ---------------------------------------------------------------------------
## 5c. Compute per-sensor Δ% SD between a perturbation and default
## ---------------------------------------------------------------------------
compute_sensor_delta_sd <- function(sensor_default, sensor_perturb) {
  ## Inner join: only sensors present in BOTH runs for same category
  merged <- merge(
    sensor_default[, .(AQSID, hazard_cat, pm_default = sensor_mean_pm25)],
    sensor_perturb[, .(AQSID, hazard_cat, pm_perturb = sensor_mean_pm25,
                       param_name, param_value)],
    by = c("AQSID", "hazard_cat"),
    all = FALSE
  )

  ## Per-sensor Δ% (guard against division by zero)
  merged[, delta_pct := fifelse(
    abs(pm_default) < 0.001,
    NA_real_,
    (pm_perturb - pm_default) / abs(pm_default) * 100
  )]

  ## Aggregate: SD of per-sensor Δ% within each category
  out <- merged[, .(
    sd_pct_chg_mean_pm25  = round(sd(delta_pct, na.rm = TRUE), 2),
    n_sensors_paired      = sum(!is.na(delta_pct))
  ), by = .(param_name, param_value, hazard_cat)]

  out[]
}

## ---------------------------------------------------------------------------
## 6. Parameter grid
## ---------------------------------------------------------------------------
param_runs <- list(
  list(param = "CLN_THR", value = "0.05", is_default = FALSE, args = list(cln_thr = 0.05)),
  list(param = "CLN_THR", value = "0.10", is_default = TRUE,  args = list(cln_thr = 0.10)),
  list(param = "CLN_THR", value = "0.15", is_default = FALSE, args = list(cln_thr = 0.15)),
  list(param = "CLN_THR_AOD_MISSING", value = "NA_allowed (default)", is_default = TRUE,  args = list(cln_thr = 0.10, aod_na_as_clean = TRUE)),
  list(param = "CLN_THR_AOD_MISSING", value = "NA_excluded", is_default = FALSE, args = list(cln_thr = 0.10, aod_na_as_clean = FALSE)),
  list(param = "PM_MAX", value = "250", is_default = FALSE, args = list(pm_max = 250)),
  list(param = "PM_MAX", value = "500", is_default = FALSE, args = list(pm_max = 500)),
  list(param = "PM_MAX", value = "1000", is_default = TRUE,  args = list(pm_max = 1000)),
  list(param = "PM_MAX", value = "2000", is_default = FALSE, args = list(pm_max = 2000)),
  list(param = "MIN_HOURS_PM", value = "1", is_default = FALSE, args = list(min_hours_pm = 1L)),
  list(param = "MIN_HOURS_PM", value = "6", is_default = FALSE, args = list(min_hours_pm = 6L)),
  list(param = "MIN_HOURS_PM", value = "12", is_default = FALSE, args = list(min_hours_pm = 12L)),
  list(param = "MIN_HOURS_PM", value = "18", is_default = TRUE,  args = list(min_hours_pm = 18L)),
  list(param = "HEAT_ABS_THR_K", value = "302.594 (85F)", is_default = FALSE, args = list(heat_abs_thr_k = 302.594)),
  list(param = "HEAT_ABS_THR_K", value = "305.372 (90F)", is_default = TRUE,  args = list(heat_abs_thr_k = 305.372)),
  list(param = "SPEI_DROUGHT_THR", value = "-0.8", is_default = FALSE, args = list(spei_drought_thr = -0.8)),
  list(param = "SPEI_DROUGHT_THR", value = "-1.0", is_default = TRUE,  args = list(spei_drought_thr = -1.0))
)

## ---------------------------------------------------------------------------
## 7. Execute all non-GVP runs
## ---------------------------------------------------------------------------
out_path <- file.path(DIR_MS1_OUT, "ms1_table7_sensitivity_results.csv")

all_results        <- list()
all_sensor_results <- list()

if (RESUME_FROM_GVP) {

  ## --gvp-only: the 17 parameter runs come from the checkpoint
  if (!file.exists(out_path))
    stop("--gvp-only needs the checkpoint of the parameter runs: ", out_path)
  results_nonGVP <- fread(out_path, colClasses = list(character = "param_value"))[param_name != "GVP_BUFFER_M"]
  if (!"median_pm25" %in% names(results_nonGVP))
    stop("Checkpoint has no median_pm25 (written by an earlier version of this script): ",
         out_path, "\n  Run without --gvp-only.")
  runs_need <- vapply(param_runs, function(r) paste(r$param, r$value), character(1))
  runs_have <- unique(results_nonGVP[, paste(param_name, param_value)])
  if (!setequal(runs_need, runs_have))
    stop("Checkpoint does not hold the ", length(runs_need), " parameter runs of this script: ", out_path,
         "\n  missing: ", paste(setdiff(runs_need, runs_have), collapse = "; "),
         "\n  Run without --gvp-only.")
  message("--gvp-only: ", length(runs_need), " parameter runs read from ", out_path)

} else {

for (i in seq_along(param_runs)) {
  run <- param_runs[[i]]
  message(sprintf("[%02d/%02d] %-25s = %s%s", i, length(param_runs),
                  run$param, run$value, if (run$is_default) " [DEFAULT]" else ""))

  cls <- do.call(classify_daily, c(list(hourly_dt = hourly), run$args))
  sm  <- summarize_run(cls, run$param, run$value, run$is_default)
  all_results[[i]] <- sm

  ## Sensor-level summary for per-sensor Δ% SD

  sensor_sm <- summarize_run_by_sensor(cls, run$param, run$value, run$is_default)
  all_sensor_results[[i]] <- sensor_sm

  message(sprintf("  → %s sensor-days | hazard mix:", format(nrow(cls), big.mark = ",")))
}

results_nonGVP <- rbindlist(all_results, fill = TRUE)
sensor_all     <- rbindlist(all_sensor_results, fill = TRUE)

## Category-level Δ% (same as before)
defaults_ref_tmp <- results_nonGVP[
  is_default == TRUE,
  .(default_n = n_sensor_days, default_ns = n_sensors, default_pm = mean_pm25,
    default_md = median_pm25),
  by = .(param_name, hazard_cat)
]
results_nonGVP <- merge(results_nonGVP, defaults_ref_tmp, by = c("param_name", "hazard_cat"), all.x = TRUE)
results_nonGVP[, `:=`(
  pct_chg_n_sensor_days = round((n_sensor_days - default_n)  / default_n  * 100, 2),
  pct_chg_n_sensors     = round((n_sensors     - default_ns) / default_ns * 100, 2),
  pct_chg_mean_pm25     = round((mean_pm25     - default_pm) / abs(default_pm) * 100, 2),
  pct_chg_median_pm25   = round((median_pm25   - default_md) / abs(default_md) * 100, 2)
)]
results_nonGVP[, c("default_n", "default_ns", "default_pm", "default_md") := NULL]

## Per-sensor Δ% SD: join default vs perturbation at sensor level
delta_sd_list <- list()
for (pn in unique(sensor_all$param_name)) {
  sensor_def <- sensor_all[param_name == pn & is_default == TRUE]
  non_def    <- sensor_all[param_name == pn & is_default == FALSE]
  for (pv in unique(non_def$param_value)) {
    sensor_pert <- non_def[param_value == pv]
    dsd <- compute_sensor_delta_sd(sensor_def, sensor_pert)
    delta_sd_list[[length(delta_sd_list) + 1L]] <- dsd
  }
}
delta_sd_nonGVP <- rbindlist(delta_sd_list, fill = TRUE)

## Merge sd_pct_chg_mean_pm25 into category-level results
results_nonGVP <- merge(
  results_nonGVP, delta_sd_nonGVP,
  by = c("param_name", "param_value", "hazard_cat"),
  all.x = TRUE
)
results_nonGVP[is_default == TRUE, sd_pct_chg_mean_pm25 := 0]
results_nonGVP[is_default == TRUE, n_sensors_paired := n_sensors]

fwrite(results_nonGVP, out_path)
message("Checkpoint saved (non-GVP runs): ", out_path)
rm(cls, sensor_all); gc()

}   # end of the parameter runs (skipped with --gvp-only)

## Default run = dataset: the default classification must give the category
## counts and medians of the Stage 9 file (same sensors). With --gvp-only a mismatch means
## the checkpoint is from an earlier dataset, and the script stops.
if (file.exists(FILE_DAILY_INTEGRATED)) {
  ds <- fread(FILE_DAILY_INTEGRATED, select = c("hazard_cat", "statefp", "PM25"), na.strings = c("", "NA"))
  ds <- ds[!sprintf("%02d", suppressWarnings(as.integer(as.character(statefp)))) %in% excl_terr]
  ds[is.na(hazard_cat), hazard_cat := "Unclassified (NA)"]
  ## Counts and medians: the default run must also give the medians of Table 5
  cmp <- merge(ds[, .(n_dataset = .N, median_dataset = median(PM25, na.rm = TRUE)), by = hazard_cat],
               results_nonGVP[param_name == "CLN_THR" & is_default == TRUE,
                              .(hazard_cat, n_default_run = n_sensor_days,
                                median_default_run = median_pm25)],
               by = "hazard_cat", all = TRUE)
  bad <- cmp[is.na(n_dataset) | is.na(n_default_run) | n_dataset != n_default_run |
               abs(median_dataset - median_default_run) > 0.001]
  if (nrow(bad)) {
    message("Default run differs from the dataset (", FILE_DAILY_INTEGRATED, "):")
    print(bad)
    if (RESUME_FROM_GVP)
      stop("Checkpoint does not match the current dataset. Run without --gvp-only.")
    warning("Default run and dataset differ: check that stages 07-09 were run with the same code.")
  } else {
    message("Default run = dataset: category counts and medians identical (",
            format(sum(cmp$n_dataset), big.mark = ","), " sensor-days)")
  }
  rm(ds, cmp, bad)
} else {
  message("Stage 9 file not found; default run not compared with the dataset: ", FILE_DAILY_INTEGRATED)
  if (RESUME_FROM_GVP) stop("--gvp-only needs the Stage 9 file to validate the checkpoint.")
}

## ---------------------------------------------------------------------------
## 8. GVP_BUFFER_M sensitivity
## ---------------------------------------------------------------------------
sf_use_s2(TRUE)  ## ← match Stage 5; ensure st_buffer() interprets dist in meters
message("\n[GVP_BUFFER_M] Re-deriving sensor-eruption assignments from source files...")

source("util_gvp_read.R")
gvp2 <- read_gvp_eruptions(2014L, 2025L)[, .(Volcano_Number, Volcano_Name,
                                             Latitude, Longitude, start_date, end_date)]

gvp_sf <- st_as_sf(gvp2, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

us_can    <- st_read(FILE_USCAN_SHP, quiet = TRUE) |> st_transform(4326)
gvp_uscan <- st_filter(gvp_sf, us_can, .predicate = st_within)
message(sprintf("  GVP eruptions within US/Canada: %d", nrow(gvp_uscan)))

if (nrow(gvp_uscan) == 0) {
  warning("No GVP eruptions found within US/Canada — skipping GVP buffer sensitivity.")
} else {

  loc <- fread(FILE_AIRNOW_LOCS, colClasses = list(character = c("AQSID", "AQSID_rev")))
  loc[, AQSID := normalize_aqsid(AQSID)]
  loc[, AQSID_rev := normalize_aqsid(AQSID_rev)]

  ## 1. Create a clean dictionary mapping EVERY valid ID to its lat/lon
  loc1 <- loc[!is.na(AQSID), .(id = AQSID, lat = lat_numeric, lon = long_numeric)]
  loc2 <- loc[!is.na(AQSID_rev), .(id = AQSID_rev, lat = lat_numeric, lon = long_numeric)]
  loc_clean <- unique(rbind(loc1, loc2), by = "id")
  loc_clean <- loc_clean[!is.na(lat) & !is.na(lon)]

  ## 2. Get unique IDs from hourly data
  aq_ids <- unique(hourly[, .(AQSID)])
  aq_ids[, AQSID := as.character(AQSID)]

  ## 3. Merge cleanly (this avoids the NA-to-NA Cartesian explosion)
  stations <- merge(aq_ids, loc_clean, by.x = "AQSID", by.y = "id", all.x = FALSE)

  stations_sf <- st_as_sf(stations, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

  ## --- diagnostics (remove after confirming fix) ---
  message(sprintf("  Stations with valid coordinates: %d", nrow(stations_sf)))
  message(sprintf("  S2 active: %s", sf_use_s2()))
  bb <- st_bbox(stations_sf)
  message(sprintf("  Station bbox: lon [%.2f, %.2f], lat [%.2f, %.2f]",
                  bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"]))

  gvp_attrs <- st_drop_geometry(gvp_uscan) |>
    select(Volcano_Number, Volcano_Name, start_date, end_date) |>
    as.data.table()

  ## The eruption flag is rewritten in `hourly` itself for each buffer and the
  ## Stage 6 values are restored after the loop. (Copying the hourly table for
  ## each buffer held three copies in memory at the second buffer.)
  volcano_number_stage06 <- hourly$Volcano_Number

  for (buf_m in c(50000L, 100000L)) {
    is_def <- (buf_m == GVP_BUFFER_M)
    message(sprintf("[GVP_BUFFER_M] Buffer = %d m%s", buf_m, if (is_def) " [DEFAULT]" else ""))

    erupt_bufs <- st_buffer(gvp_uscan, dist = buf_m)

    ## --- diagnostics (remove after confirming fix) ---
    message(sprintf("  Buffer areas (first 3): %s",
                    paste(round(st_area(erupt_bufs)[1:min(3, nrow(erupt_bufs))]/1e6, 1),
                          "km²", collapse = ", ")))
    stations_in_buf <- st_filter(stations_sf, erupt_bufs, .predicate = st_within)
    message(sprintf("  Stations in buffer: %d", nrow(stations_in_buf)))

    if (nrow(stations_in_buf) == 0) {
      message("  No stations in buffer — skipping this buffer value.")
      next
    }

    stations_with_vname <- st_join(stations_in_buf, erupt_bufs |> select(Volcano_Name), join = st_within) |>
      st_drop_geometry() |> as.data.table()

    near_stations <- stations_with_vname[
      gvp_attrs,
      on = "Volcano_Name",
      nomatch = 0L,
      allow.cartesian = TRUE  ## <-- Tells data.table this expansion is safe and intentional
    ][, .(AQSID, Volcano_Number, start_date, end_date)]

    message(sprintf("  Station * eruption pairs: %d", nrow(near_stations)))

    hourly[, Volcano_Number := NA_character_]

    if (nrow(near_stations) > 0) {
      ## Prepare dates for high-speed non-equi join
      near_stations[, start_dt := as.POSIXct(paste0(start_date, " 00:00:00"), tz = "UTC")]
      near_stations[, end_dt   := as.POSIXct(paste0(end_date,   " 23:59:59"), tz = "UTC")]
      near_stations[, v_num_str := as.character(Volcano_Number)]

      ## Update assignment efficiently
      hourly[
        near_stations,
        on = .(AQSID, datetime >= start_dt, datetime <= end_dt),
        Volcano_Number := i.v_num_str
      ]
    }

    cls_gvp <- classify_daily(hourly_dt = hourly)
    sm_gvp  <- summarize_run(cls_gvp, param_name = "GVP_BUFFER_M", param_value = as.character(buf_m), is_default = is_def)
    all_results[[length(all_results) + 1L]] <- sm_gvp

    ## Sensor-level summary for per-sensor Δ% SD
    sensor_sm_gvp <- summarize_run_by_sensor(cls_gvp, param_name = "GVP_BUFFER_M", param_value = as.character(buf_m), is_default = is_def)
    all_sensor_results[[length(all_sensor_results) + 1L]] <- sensor_sm_gvp

    message(sprintf("  → %s sensor-days classified", format(nrow(cls_gvp), big.mark = ",")))
    rm(cls_gvp); gc()
  }

  hourly[, Volcano_Number := volcano_number_stage06]   # Stage 6 eruption flags restored
  rm(volcano_number_stage06)
}

## ---------------------------------------------------------------------------
## 9. Bind all results and finalise
## ---------------------------------------------------------------------------
results_nonGVP_final <- fread(out_path, colClasses = list(character = "param_value"))[param_name != "GVP_BUFFER_M"]
if ("hazard_order" %in% names(results_nonGVP_final)) results_nonGVP_final[, hazard_order := NULL]

gvp_result_list <- all_results[
  vapply(all_results, function(x) {
    is.data.table(x) && "param_name" %in% names(x) && any(x$param_name == "GVP_BUFFER_M")
  }, logical(1))
]

if (length(gvp_result_list) > 0) {
  results_gvp <- rbindlist(gvp_result_list, fill = TRUE)

  defaults_gvp <- results_gvp[
    is_default == TRUE,
    .(default_n = n_sensor_days, default_ns = n_sensors, default_pm = mean_pm25,
      default_md = median_pm25),
    by = .(param_name, hazard_cat)
  ]
  results_gvp <- merge(results_gvp, defaults_gvp, by = c("param_name", "hazard_cat"), all.x = TRUE)
  results_gvp[, `:=`(
    pct_chg_n_sensor_days = round((n_sensor_days - default_n)  / default_n  * 100, 2),
    pct_chg_n_sensors     = round((n_sensors     - default_ns) / default_ns * 100, 2),
    pct_chg_mean_pm25     = round((mean_pm25     - default_pm) / abs(default_pm) * 100, 2),
    pct_chg_median_pm25   = round((median_pm25   - default_md) / abs(default_md) * 100, 2)
  )]
  results_gvp[, c("default_n", "default_ns", "default_pm", "default_md") := NULL]

  ## Per-sensor Δ% SD for GVP runs
  gvp_sensor_list <- all_sensor_results[
    vapply(all_sensor_results, function(x) {
      is.data.table(x) && "param_name" %in% names(x) && any(x$param_name == "GVP_BUFFER_M")
    }, logical(1))
  ]
  if (length(gvp_sensor_list) > 0) {
    sensor_gvp_all <- rbindlist(gvp_sensor_list, fill = TRUE)
    sensor_gvp_def <- sensor_gvp_all[is_default == TRUE]
    delta_sd_gvp_list <- list()
    for (pv in unique(sensor_gvp_all[is_default == FALSE, param_value])) {
      sensor_pert <- sensor_gvp_all[param_value == pv & is_default == FALSE]
      dsd <- compute_sensor_delta_sd(sensor_gvp_def, sensor_pert)
      delta_sd_gvp_list[[length(delta_sd_gvp_list) + 1L]] <- dsd
    }
    if (length(delta_sd_gvp_list) > 0) {
      delta_sd_gvp <- rbindlist(delta_sd_gvp_list, fill = TRUE)
      results_gvp <- merge(results_gvp, delta_sd_gvp,
                           by = c("param_name", "param_value", "hazard_cat"),
                           all.x = TRUE)
    }
    results_gvp[is_default == TRUE, sd_pct_chg_mean_pm25 := 0]
    results_gvp[is_default == TRUE, n_sensors_paired := n_sensors]
  }

  results <- rbindlist(list(results_nonGVP_final, results_gvp), fill = TRUE)
  message("GVP runs appended: ", uniqueN(results_gvp[, paste(param_name, param_value)]), " runs")
} else {
  results <- results_nonGVP_final
  message("No GVP results to append — final output contains non-GVP runs only.")
}

results[, hazard_order := match(hazard_cat, prio_levels)]
results[is.na(hazard_order), hazard_order := 99L]

setcolorder(results, c(
  "param_name", "param_value", "is_default",
  "hazard_cat", "hazard_order",
  "n_sensor_days", "pct_chg_n_sensor_days",
  "n_sensors",     "pct_chg_n_sensors",
  "mean_pm25",     "sd_pm25", "pct_chg_mean_pm25",
  "median_pm25",   "pct_chg_median_pm25",
  "sd_pct_chg_mean_pm25", "n_sensors_paired"
))
setorderv(results, c("param_name", "param_value", "hazard_order"))

## ---------------------------------------------------------------------------
## 10. Save final
## ---------------------------------------------------------------------------
fwrite(results, out_path)

message("\n=== COMPLETE ===")
message("Output: ", out_path)
message("Rows:   ", nrow(results))
message("Params: ", uniqueN(results[, paste(param_name, param_value)]))

cat("\n=== N sensor-days: Wildfire + Baseline + NA by parameter run ===\n")
print(
  dcast(
    results[hazard_cat %in% c("Wildfire", "Baseline", "Unclassified (NA)")],
    param_name + param_value + is_default ~ hazard_cat,
    value.var = "n_sensor_days"
  )[order(param_name, param_value)]
)
