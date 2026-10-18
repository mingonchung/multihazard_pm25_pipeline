## =============================================================================
## ms1_table4_dataset_summary.R
## MS1 Table 5 (Section 4): dataset summary by hazard category and network
##
## For each hazard category (Table 2 order), Baseline, and Unclassified (NA):
##   N sensor-days, N sensors, mean (SD), median [IQR], and maximum of the
##   daily mean PM2.5 (µg/m³), for AirNow and PurpleAir outdoor.
##
## Sensors in US territories are not counted (CONUS, Alaska, and Hawaii only).
##
## Input:   FILE_DAILY_INTEGRATED             (AirNow, stage09 output)
##          FILE_PA_DAILY_INTEGRATED_OUTDOOR  (PurpleAir outdoor, stage09_pa output)
## Output:  DIR_MS1_OUT/ms1_table4_dataset_summary.csv
## =============================================================================

rm(list = ls())

library(data.table)

source("config_pa.R")   # also sources config.R

DIR_MS1_OUT <- file.path(ROOT_DIR, "ms1")
## Optional local (laptop) paths: if ms1_local_paths.R sits next to this
## script, it overrides the input paths and the output folder set above.
if (file.exists("ms1_local_paths.R")) source("ms1_local_paths.R")
if (!dir.exists(DIR_MS1_OUT)) dir.create(DIR_MS1_OUT, recursive = TRUE)

airnow    <- fread(FILE_DAILY_INTEGRATED, na.strings = c("", "NA"),
                   colClasses = list(character = c("AQSID", "AQSID_join")))
purpleair <- fread(FILE_PA_DAILY_INTEGRATED_OUTDOOR, na.strings = c("", "NA"),
                   colClasses = list(character = "STATION"))

## ---------------------------------------------------------------------------
## 0. Territories: not counted in MS1 tables and figures (CONUS, Alaska, Hawaii)
##    State FIPS codes of AS, GU, MP, PR, VI, as in the figure scripts.
## ---------------------------------------------------------------------------
excl_terr <- c("60", "66", "69", "72", "78")
drop_territories <- function(dt, id_col, label) {
  st   <- sprintf("%02d", suppressWarnings(as.integer(as.character(dt$statefp))))
  terr <- st %in% excl_terr
  message(sprintf("%s: %s sensor-days at %d sensors in US territories excluded from the table",
                  label, format(sum(terr), big.mark = ","), uniqueN(dt[[id_col]][terr])))
  dt[!terr]
}
airnow    <- drop_territories(airnow,    "AQSID",   "AirNow")
purpleair <- drop_territories(purpleair, "STATION", "PurpleAir outdoor")

## ---------------------------------------------------------------------------
## 1. Normalize hazard_cat
##    - Empty strings → NA (unclassified)
##    - "TropicalStorm" → "Tropical Storm" (display label)
## ---------------------------------------------------------------------------
normalize_cat <- function(dt) {
  dt[hazard_cat == "" | is.na(hazard_cat), hazard_cat := NA_character_]
  dt[hazard_cat == "TropicalStorm",        hazard_cat := "Tropical Storm"]
  invisible(dt)
}

normalize_cat(airnow)
normalize_cat(purpleair)

## ---------------------------------------------------------------------------
## 2. Canonical category order (Table 2 order; table row order)
## ---------------------------------------------------------------------------
cat_order <- c(
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
  "Tropical Storm",
  "Flood",
  "Baseline",
  NA   # Unclassified — handled separately below
)

## ---------------------------------------------------------------------------
## 3. Summary function: N sensor-days, N unique sensors, PM2.5 distribution
## ---------------------------------------------------------------------------
pm_stats <- function(x) {
  q <- quantile(x, c(0.25, 0.50, 0.75), na.rm = TRUE, names = FALSE, type = 7)
  list(mean_pm25   = round(mean(x, na.rm = TRUE), 2),
       sd_pm25     = round(sd(x,   na.rm = TRUE), 2),
       median_pm25 = round(q[2], 2),
       q1_pm25     = round(q[1], 2),
       q3_pm25     = round(q[3], 2),
       max_pm25    = round(max(x, na.rm = TRUE), 2))
}

summarize_network <- function(dt, id_col) {
  dt[, c(list(n_sensor_days = .N,
              n_sensors     = uniqueN(get(id_col))),
         pm_stats(PM25)),
     keyby = hazard_cat]
}

an_sum <- summarize_network(airnow,    "AQSID")
pa_sum <- summarize_network(purpleair, "STATION")

## ---------------------------------------------------------------------------
## 4. Merge networks and impose canonical row order
## ---------------------------------------------------------------------------
# Replace NA key with sentinel for merge, then restore
sentinel <- "__UNCLASSIFIED__"

set_sentinel <- function(dt) {
  dt[is.na(hazard_cat), hazard_cat := sentinel]
}
set_sentinel(an_sum)
set_sentinel(pa_sum)

tbl <- merge(an_sum, pa_sum,
             by      = "hazard_cat",
             all     = TRUE,
             suffixes = c("_airnow", "_purpleair"))

# Build ordered key vector (NAs replaced with sentinel)
cat_order_key <- fifelse(is.na(cat_order), sentinel, cat_order)
tbl[, row_order := match(hazard_cat, cat_order_key)]
setorder(tbl, row_order)
tbl[, row_order := NULL]

# Restore display label for unclassified
tbl[hazard_cat == sentinel, hazard_cat := "Unclassified (NA)"]

## ---------------------------------------------------------------------------
## 5. Totals row
## ---------------------------------------------------------------------------
tot <- function(dt, id_col, suffix) {
  x <- c(list(n_sensor_days = nrow(dt), n_sensors = uniqueN(dt[[id_col]])),
         pm_stats(dt$PM25))
  setNames(x, paste0(names(x), suffix))
}
totals <- as.data.table(c(list(hazard_cat = "Total"),
                          tot(airnow,    "AQSID",   "_airnow"),
                          tot(purpleair, "STATION", "_purpleair")))

tbl5 <- rbindlist(list(tbl, totals), use.names = TRUE, fill = TRUE)

## ---------------------------------------------------------------------------
## 6. Display strings: mean (SD) and median [Q1, Q3]
## ---------------------------------------------------------------------------
fmt_mean_sd <- function(m, s) {
  ifelse(is.na(m), NA_character_, sprintf("%.2f (%.2f)", m, s))
}
fmt_med_iqr <- function(m, a, b) {
  ifelse(is.na(m), NA_character_, sprintf("%.2f [%.2f, %.2f]", m, a, b))
}

for (nw in c("airnow", "purpleair")) {
  col <- function(v) paste0(v, "_", nw)
  tbl5[, (col("mean_sd_pm25"))    := fmt_mean_sd(get(col("mean_pm25")), get(col("sd_pm25")))]
  tbl5[, (col("median_iqr_pm25")) := fmt_med_iqr(get(col("median_pm25")),
                                                 get(col("q1_pm25")), get(col("q3_pm25")))]
}

keep <- c("hazard_cat",
          "n_sensor_days_airnow",    "n_sensors_airnow",
          "mean_sd_pm25_airnow",     "median_iqr_pm25_airnow",    "max_pm25_airnow",
          "n_sensor_days_purpleair", "n_sensors_purpleair",
          "mean_sd_pm25_purpleair",  "median_iqr_pm25_purpleair", "max_pm25_purpleair")
tbl5 <- tbl5[, ..keep]

## ---------------------------------------------------------------------------
## 7. Print and write
## ---------------------------------------------------------------------------
print(tbl5)

out_file <- file.path(DIR_MS1_OUT, "ms1_table4_dataset_summary.csv")
fwrite(tbl5, out_file)
message("Written: ", out_file)
