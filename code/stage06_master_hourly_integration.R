## =============================================================================
## stage06_master_hourly_integration.R
## MS1 pipeline — Stage 6 (AirNow): master hourly integration table
##
## Time base
##   - AirNow hourly files report Date and Time in GMT (UTC), marking the start
##     of the hour. GMT_Offset is the site's local STANDARD time offset
##     (no daylight saving). Local standard time = UTC + GMT_Offset.
##   - `datetime` (UTC) is the hourly key. Hourly sources (Storm Events, GVP)
##     are joined on it.
##   - `date` is the LOCAL STANDARD date of each hour. Daily sources (HMS/FIRED,
##     MAIAC AOD, GridMET, SPEI, Daymet) and the p95 month-day are joined on it,
##     and stage07_08 aggregates by it (EPA 24-h convention: midnight to
##     midnight local standard time).
##   - GMT_Offset (one value per site) is the standard offset of the time zone
##     at the site coordinates (lutz lookup, mid-January), as in stage06_pa.
##     The offset reported in the AirNow files is used only where the lookup
##     returns no time zone; sites where the two differ are listed in the log.
##
## Output columns added by this script: date, GMT_Offset, srad_gridmet,
##   srad_daymet (GridMET and Daymet solar radiation, named explicitly).
##
## Input:   FILE_AIRNOW_PM25, FILE_AIRNOW_LOCS, FILE_USCAN_SHP, FILE_HMS_FIRED_OUT,
##          FILE_AOD_047, FILE_AOD_055, FILE_STORM_OUT, FILE_GVP_OUT,
##          FILE_GRIDMET_MAIN, FILE_GRIDMET_P95, FILE_GRIDMET_SPEI,
##          FILE_DAYMET_MAIN, FILE_DAYMET_P95
## Output:  FILE_PM25_AOD_MET
## Follows: stage02, stage03, stage04, stage05
## Precedes: stage07_08_daily_classification.R
## =============================================================================

rm(list=ls())

# libraries
library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(stringr)
library(data.table)

source("config.R")

# -------------------------
# 0) Helper: normalize AQSID
# -------------------------
normalize_aqsid <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)

  x <- dplyr::na_if(x, "'NA")
  x <- dplyr::na_if(x, "NA")
  x <- dplyr::na_if(x, "")

  x <- stringr::str_replace(x, "^'+", "")   # remove leading apostrophes
  x <- stringr::str_trim(x)

  is_num <- !is.na(x) & stringr::str_detect(x, "^[0-9]+$")

  # only pad numeric strings shorter than 9
  x[is_num] <- ifelse(
    nchar(x[is_num]) < 9,
    stringr::str_pad(x[is_num], width = 9, side = "left", pad = "0"),
    x[is_num]
  )

  x
}

# -------------------------
# 1) Station locations + spatial clip
# -------------------------
locs_df <- read_csv(
  FILE_AIRNOW_LOCS,
  col_types = cols(
    AQSID        = col_character(),
    AQSID_rev    = col_character(),
    lat_numeric  = col_double(),
    long_numeric = col_double()
  )
) %>%
  mutate(
    AQSID     = normalize_aqsid(AQSID),
    AQSID_rev = normalize_aqsid(AQSID_rev)
  ) %>%
  distinct(AQSID, .keep_all = TRUE)

uscan <- st_read(FILE_USCAN_SHP)

# -------------------------
# 2) Read PM2.5 (hourly) and define "target station universe"
# -------------------------
## Raw AirNow combine output columns: Date (MM/DD/YY), Time (HH:MM),
## AQSID, SiteName, GMT_Offset, Parameter, Units, Value, Source.
## The file is already filtered to PM2.5 (combine script splits by Parameter).
## Date and Time are GMT (UTC) and are used as they are.
## GMT_Offset = hours to add to UTC to obtain local standard time
## (e.g. EST = -5), no daylight saving.
pm25 <- fread(
  FILE_AIRNOW_PM25,
  select     = c("AQSID", "Date", "Time", "GMT_Offset", "Value"),
  colClasses = c(
    AQSID  = "character",
    Date   = "character",
    Time   = "character",
    Value  = "numeric"
    ## GMT_Offset: let fread auto-detect (float64; e.g. -5, -3.5)
  )
) %>%
  mutate(
    AQSID       = normalize_aqsid(AQSID),
    datetime    = mdy_hm(paste(Date, Time), tz = "UTC"),   # UTC as reported
    PM25_airnow = Value
  )

## One standard-time offset per site: the most frequent value in its records
site_offset <- pm25 %>%
  filter(!is.na(GMT_Offset)) %>%
  count(AQSID, GMT_Offset) %>%
  arrange(AQSID, desc(n)) %>%
  distinct(AQSID, .keep_all = TRUE) %>%
  select(AQSID, GMT_Offset)

pm25 <- pm25 %>% select(AQSID, datetime, PM25_airnow)

# drop rows where AirNow PM2.5 is missing
pm25.na <- pm25 %>% filter(!is.na(PM25_airnow))

## Deduplicate: raw AirNow .dat files can have multiple Source submissions for
## the same AQSID × hour (e.g. AirNow + AIRSIS).  Average them so the join
## onto the hourly grid remains 1-to-1 and produces no extra rows.
pm25.na <- pm25.na %>%
  group_by(AQSID, datetime) %>%
  summarise(PM25_airnow = mean(PM25_airnow, na.rm = TRUE), .groups = "drop")

# Use PM2.5 AQSIDs to filter locs, then spatial clip to US/Canada
pm25_sites_all <- locs_df %>%
  filter(AQSID %in% unique(pm25.na$AQSID) | AQSID_rev %in% unique(pm25.na$AQSID)) %>%
  st_as_sf(coords = c("long_numeric", "lat_numeric"), crs = 4326)

pm25_sites <- st_filter(pm25_sites_all, uscan)

## -- Site offsets from the time zone of the site coordinates ---------------
## The AirNow GMT_Offset field is 0 for some temporary monitors and differs by
## 1-2 h from the site's time zone for others (e.g. Colorado sites reported at
## -6, western Kentucky at -5). The standard offset of the time zone at the
## site coordinates (lutz lookup; offset in mid-January) is therefore used for
## every site, as in stage06_pa. The reported offset is kept only where the
## lookup returns no time zone (or, if it is 0, replaced by round(lon / 15)).
if (!requireNamespace("lutz", quietly = TRUE))
  stop("Package 'lutz' is required (time-zone lookup from site coordinates).")
std_offset_h <- function(tz_name) {
  t0 <- as.POSIXct("2021-01-15 12:00:00", tz = "UTC")
  local_clock <- format(t0, tz = tz_name, usetz = FALSE)
  as.numeric(difftime(as.POSIXct(local_clock, tz = "UTC"), t0, units = "hours"))
}
xy <- st_coordinates(pm25_sites)
site_tz <- data.table(
  AQSID     = pm25_sites$AQSID,
  AQSID_rev = pm25_sites$AQSID_rev,
  lon       = xy[, 1],
  tz        = lutz::tz_lookup_coords(xy[, 2], xy[, 1], method = "accurate", warn = FALSE)
)
tz_names <- unique(na.omit(site_tz$tz))
tz_off   <- setNames(vapply(tz_names, std_offset_h, numeric(1)), tz_names)
site_tz[, tz_offset := unname(tz_off[tz])]
tz_long <- unique(rbind(site_tz[, .(AQSID, tz, tz_offset, lon)],
                        site_tz[!is.na(AQSID_rev), .(AQSID = AQSID_rev, tz, tz_offset, lon)]),
                  by = "AQSID")

site_offset <- merge(as.data.table(site_offset), tz_long, by = "AQSID", all.x = TRUE)
site_offset[, GMT_Offset := as.numeric(GMT_Offset)]          # half-hour offsets
site_offset[, reported := GMT_Offset]
site_offset[!is.na(tz_offset), GMT_Offset := tz_offset]
site_offset[is.na(tz_offset) & reported == 0 & !is.na(lon), GMT_Offset := round(lon / 15)]
chg <- site_offset[GMT_Offset != reported]
message(sprintf("Site offsets: %d sites | from time-zone lookup: %d | reported offset kept (no lookup): %d",
                nrow(site_offset), site_offset[!is.na(tz_offset), .N],
                site_offset[is.na(tz_offset), .N]))
message(sprintf("Sites whose offset differs from the reported GMT_Offset: %d", nrow(chg)))
if (nrow(chg)) print(chg[, .(AQSID, tz, reported, used = GMT_Offset)], nrows = 500)
site_offset <- site_offset[, .(AQSID, GMT_Offset)]

# Build ID list that includes BOTH AQSID and AQSID_rev (your intended pattern)
ids <- pm25_sites %>%
  st_drop_geometry() %>%
  select(AQSID, AQSID_rev) %>%
  pivot_longer(cols = c(AQSID, AQSID_rev), values_to = "id", values_drop_na = TRUE) %>%
  distinct(id) %>%
  pull(id)

# -------------------------
# 3) Create the full hourly target grid (2014-2025, UTC hours)
# -------------------------
hourly_seq <- seq(
  from = as.POSIXct(paste0(STUDY_START, " 00:00:00"), tz = "UTC"),
  to   = as.POSIXct(paste0(STUDY_END,   " 23:00:00"), tz = "UTC"),
  by   = "hour"
)

input_raw <- expand_grid(
  AQSID    = ids,
  datetime = hourly_seq
)

print(paste0("input raw done: ", nrow(input_raw)))

# -------------------------
# 4) Join PM2.5 onto the hourly grid; add offset and local standard date
# -------------------------
## date = local standard date of the hour = date of (UTC + GMT_Offset).
## IDs without any PM2.5 record have no offset and an NA date; they carry
## no PM2.5 and are dropped in stage07_08.
## The first UTC hours of STUDY_START fall on the previous local date and
## form a partial day that fails the MIN_HOURS_PM rule in stage07_08.
input_pm25 <- input_raw %>%
  left_join(pm25.na, by = c("AQSID", "datetime")) %>%
  rename(PM25 = PM25_airnow) %>%
  left_join(site_offset, by = "AQSID") %>%
  mutate(date = as.Date(datetime + GMT_Offset * 3600, tz = "UTC"))

print(paste0("PM2.5 done: ", nrow(input_pm25)))
rm(input_raw, pm25, pm25.na)

# -------------------------
# 5) HMS + FIRED (daily, keyed by sensor_id + local date)
# -------------------------
hms_fired <- read_csv(FILE_HMS_FIRED_OUT,
                      col_types = cols(
                        date      = col_date(),
                        sensor_id = col_character(),
                        density   = col_character(),
                        frd_d_s   = col_character()
                      )) %>%
  mutate(sensor_id = normalize_aqsid(sensor_id))

hms_fired.na <- hms_fired %>%
  filter(!is.na(density) | !is.na(frd_d_s)) %>%
  mutate(
    frd_d_s = str_replace_all(frd_d_s, "\\s*;\\s*", ";"),
    frd_d_s = str_trim(frd_d_s)
  )

input_hms_fired <- input_pm25 %>%
  left_join(hms_fired.na, by = c("AQSID" = "sensor_id", "date" = "date"))

print(paste0("HMS & FIRED done: ", nrow(input_hms_fired)))
rm(input_pm25, hms_fired, hms_fired.na)

# -------------------------
# 6) AOD (daily) 0.47 + 0.55, keyed by local date
# -------------------------
aod_47 <- read_csv(
  FILE_AOD_047,
  col_types = cols(
    AQSID   = col_character(),
    lon     = col_skip(),
    lat     = col_skip(),
    time    = col_date(),
    AOD_047 = col_double()
  )
) %>%
  mutate(AQSID = normalize_aqsid(AQSID),
         date  = as.Date(time)) %>%
  select(AQSID, date, AOD_047) %>%
  distinct(AQSID, date, .keep_all = TRUE)

aod_55 <- read_csv(
  FILE_AOD_055,
  col_types = cols(
    AQSID   = col_character(),
    lon     = col_skip(),
    lat     = col_skip(),
    time    = col_date(),
    AOD_055 = col_double()
  )
) %>%
  mutate(AQSID = normalize_aqsid(AQSID),
         date  = as.Date(time)) %>%
  select(AQSID, date, AOD_055) %>%
  distinct(AQSID, date, .keep_all = TRUE)

aod_clean <- full_join(aod_47, aod_55, by = c("AQSID", "date")) %>%
  distinct(AQSID, date, .keep_all = TRUE)

input_aod <- input_hms_fired %>%
  left_join(aod_clean, by = c("AQSID", "date"))

print(paste0("AOD done: ", nrow(input_aod)))
rm(input_hms_fired, aod_47, aod_55, aod_clean)

# -------------------------
# 7) NOAA Storm (hourly, UTC; join on AQSID OR AQSID_rev)
# -------------------------
storm <- read_csv(FILE_STORM_OUT,
                  col_types = cols(
                    AQSID     = col_character(),
                    AQSID_rev = col_character(),
                    datetime  = col_datetime(format = ""),
                    EVENT_ID  = col_character(),  # may be semicolon-joined if multiple events co-occur
                    EVENT_TYPE = col_character(),
                    .default  = col_guess()
                  )) %>%
  mutate(
    AQSID     = normalize_aqsid(AQSID),
    AQSID_rev = normalize_aqsid(AQSID_rev),
    datetime  = force_tz(datetime, tzone = "UTC")
  )

storm.na <- storm %>%
  filter(!(is.na(EVENT_ID) & is.na(EVENT_TYPE))) %>%
  select(AQSID, AQSID_rev, datetime, EVENT_ID, EVENT_TYPE)

storm_long <- storm.na %>%
  pivot_longer(
    cols           = c(AQSID, AQSID_rev),
    names_to       = "which_id",
    values_to      = "id",
    values_drop_na = TRUE
  )

storm_ag <- storm_long %>%
  group_by(id, datetime) %>%
  summarize(
    EVENT_ID   = paste(unique(EVENT_ID),   collapse = ";"),
    EVENT_TYPE = paste(unique(EVENT_TYPE), collapse = ";"),
    .groups = "drop"
  )

input_storm <- input_aod %>%
  left_join(storm_ag, by = c("AQSID" = "id", "datetime" = "datetime"))

print(paste0("STORM done: ", nrow(input_storm)))
rm(input_aod, storm, storm.na, storm_long, storm_ag)

# -------------------------
# 8) GVP (hourly, UTC)
# -------------------------
gvp <- read_csv(FILE_GVP_OUT,
                col_types = cols(
                  datetime        = col_datetime(format = ""),
                  AQSID           = col_character(),
                  Volcano_Number  = col_double(),
                  Eruption_Number = col_double(),
                  .default        = col_guess()
                )) %>%
  mutate(
    AQSID    = normalize_aqsid(AQSID),
    datetime = force_tz(datetime, tzone = "UTC")
  )

gvp.na <- gvp %>%
  filter(!is.na(Volcano_Number), !is.na(Eruption_Number)) %>%
  select(AQSID, datetime, Volcano_Number, Eruption_Number)

gvp_ag <- gvp.na %>%
  group_by(AQSID, datetime) %>%
  summarize(
    Volcano_Number   = paste(unique(Volcano_Number), collapse = ";"),
    Eruption_Number = paste(unique(Eruption_Number), collapse = ";"),
    .groups = "drop"
  )

input_gvp <- input_storm %>%
  left_join(gvp_ag, by = c("AQSID", "datetime"))

print(paste0("GVP done: ", nrow(input_gvp)))
rm(input_storm, gvp, gvp.na, gvp_ag)

# -------------------------
# 9) GridMet daily -> join by local date
# -------------------------
gridmet <- fread(FILE_GRIDMET_MAIN) %>%
  mutate(
    AQSID = normalize_aqsid(AQSID),
    time  = as.Date(time)
  )

gridmet.na <- gridmet %>%
  filter(!is.na(pr)) %>%
  select(AQSID, time, pr, rmax, rmin, srad_gridmet = srad, th, tmmn, tmmx, vs, vpd)

input_gridmet <- input_gvp %>%
  left_join(gridmet.na, by = c("AQSID" = "AQSID", "date" = "time"))

print(paste0("GridMet done: ", nrow(input_gridmet)))
rm(input_gvp, gridmet, gridmet.na)

# -------------------------
# 10) GridMet p95 (month-day only) -> join by AQSID + md of local date
# -------------------------
gridmet.tmp <- fread(FILE_GRIDMET_P95) %>%
  mutate(
    AQSID = normalize_aqsid(AQSID),  # int -> padded char
    md    = as.character(md)
  )

gridmet.tmp.na <- gridmet.tmp %>%
  filter(!is.na(tmmx_p95_1981_2010) | !is.na(tmmn_p95_1981_2010)) %>%
  select(AQSID, md, tmmx_p95_1981_2010, tmmx_p95_1991_2020, tmmn_p95_1981_2010, tmmn_p95_1991_2020)

input_gridmet_p95 <- input_gridmet %>%
  mutate(md = format(date, "%m-%d")) %>%
  left_join(gridmet.tmp.na, by = c("AQSID", "md")) %>%
  select(-md)

print(paste0("GridMet p95 done: ", nrow(input_gridmet_p95)))
rm(input_gridmet, gridmet.tmp, gridmet.tmp.na)

# -------------------------
# 11) SPEI sparse (5-day) -> densify daily -> join by local date
# -------------------------
gridmet.spei <- fread(FILE_GRIDMET_SPEI) %>%
  mutate(
    AQSID = normalize_aqsid(AQSID),
    time  = as.Date(time)
  )

spei_sparse <- gridmet.spei %>%
  transmute(
    AQSID,
    date = time,
    spei30d,
    spei90d
  ) %>%
  filter(!is.na(spei30d) | !is.na(spei90d))

full_dates <- seq(STUDY_START, STUDY_END, by = "day")

spei_daily_filled <- spei_sparse %>%
  group_by(AQSID) %>%
  tidyr::complete(date = full_dates) %>%
  arrange(date, .by_group = TRUE) %>%
  tidyr::fill(spei30d, spei90d, .direction = "downup") %>%  # forward fill, then back-fill the start of each series
  ungroup()

input_spei <- input_gridmet_p95 %>%
  left_join(spei_daily_filled, by = c("AQSID", "date"))

print(paste0("SPEI done: ", nrow(input_spei)))
rm(input_gridmet_p95, gridmet.spei, spei_sparse, spei_daily_filled)

# -------------------------
# 12) DayMet daily -> join by local date
# -------------------------
daymet <- fread(FILE_DAYMET_MAIN) %>%
  mutate(
    AQSID = normalize_aqsid(AQSID),
    time  = as.Date(time)
  )

daymet.na <- daymet %>%
  filter(!is.na(prcp)) %>%
  select(AQSID, time, dayl, prcp, srad_daymet = srad, tmax, tmin, vp)

input_daymet <- input_spei %>%
  left_join(daymet.na, by = c("AQSID" = "AQSID", "date" = "time"))

print(paste0("DAYMET done: ", nrow(input_daymet)))
rm(input_spei, daymet, daymet.na)

# -------------------------
# 13) DayMet p95 (month-day only) -> join by AQSID + md of local date
# -------------------------
daymet.tmp <- fread(FILE_DAYMET_P95) %>%
  mutate(
    AQSID = normalize_aqsid(AQSID),
    md    = as.character(md)
  )

daymet.tmp.na <- daymet.tmp %>%
  filter(!is.na(tmax_p95_1981_2010) | !is.na(tmin_p95_1981_2010)) %>%
  select(AQSID, md, tmax_p95_1981_2010, tmax_p95_1991_2020, tmin_p95_1981_2010, tmin_p95_1991_2020)

final_out <- input_daymet %>%
  mutate(md = format(date, "%m-%d")) %>%
  left_join(daymet.tmp.na, by = c("AQSID", "md")) %>%
  select(-md) %>%
  relocate(AQSID, datetime, date, GMT_Offset)

print(paste0("DAYMET p95 done: ", nrow(final_out)))
rm(input_daymet, daymet.tmp, daymet.tmp.na)

# -------------------------
# 14) Save
# -------------------------
fwrite(final_out,
       file     = FILE_PM25_AOD_MET,
       compress = "gzip")

print("output saved")
