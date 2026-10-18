rm(list = ls())
library(data.table)
library(lubridate)

source("config.R")

wd       <- DIR_EXTRACT_CSV
win_days <- P95_WINDOW_DAYS
periods  <- P95_PERIODS

## ------------------------- Helpers ------------------------------------------
clean_id <- function(x) gsub("^'+", "", trimws(as.character(x)))

parse_time_to_date <- function(x) {
  suppressWarnings({
    d1 <- mdy(x, quiet = TRUE)
    d2 <- ymd(x, quiet = TRUE)
  })
  out <- ifelse(!is.na(d1), as.character(d1), as.character(d2))
  as.Date(out)
}

prep_temp_only <- function(dt, id_col, time_col = "time") {
  dt <- as.data.table(dt)
  dt[, (id_col) := clean_id(get(id_col))]
  dt[, date := parse_time_to_date(get(time_col))]
  dt <- dt[!is.na(date)]
  
  dt[, year := year(date)]
  dt[, md := format(date, "%m-%d")]   # includes "02-29"
  dt[]
}

make_md_calendar <- function() {
  # Leap-year calendar ensures Feb 29 exists
  d <- seq.Date(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")
  data.table(md = format(d, "%m-%d"), doy = seq_along(d))  # doy = 1..366
}

build_md_window_map <- function(win = 7L) {
  cal <- make_md_calendar()
  n <- nrow(cal)  # 366
  
  map <- CJ(target_doy = 1:n, donor_doy = 1:n)
  map[, dist := pmin(abs(target_doy - donor_doy), n - abs(target_doy - donor_doy))]
  map <- map[dist <= win, .(target_doy, donor_doy)]
  
  map <- merge(map, cal[, .(target_doy = doy, target_md = md)], by = "target_doy")
  map <- merge(map, cal[, .(donor_doy  = doy, donor_md  = md)], by = "donor_doy")
  map <- map[, .(donor_md, target_md)]
  setkey(map, donor_md)
  map
}

compute_md_p95_table <- function(dt_temp, id_col, lon_col, lat_col, periods_list, win = 7L) {
  md_map <- build_md_window_map(win)
  
  # station coords (unique; take first non-missing)
  coords <- dt_temp[
    , .(
      lon = suppressWarnings(first(na.omit(get(lon_col)))),
      lat = suppressWarnings(first(na.omit(get(lat_col))))
    ),
    by = .(id = get(id_col))
  ]
  
  # full md calendar (366 rows)
  cal <- make_md_calendar()
  
  out_list <- list()
  
  for (nm in names(periods_list)) {
    s <- periods_list[[nm]][1]
    e <- periods_list[[nm]][2]
    
    base <- dt_temp[
      year >= s & year <= e,
      .(id = as.character(get(id_col)), donor_md = md, tmmx = tmmx, tmmn = tmmn)
    ]
    
    # join md window map to replicate each donor_md into all target_md within window
    x <- base[md_map, on = .(donor_md), allow.cartesian = TRUE]
    
    # compute p95 by station + target_md
    p95_tmmx <- x[!is.na(tmmx),
                  .(p95 = as.numeric(quantile(tmmx, 0.95, na.rm = TRUE, type = 7))),
                  by = .(id, md = target_md)
    ]
    setnames(p95_tmmx, "p95", paste0("tmmx_p95_", nm))
    
    p95_tmmn <- x[!is.na(tmmn),
                  .(p95 = as.numeric(quantile(tmmn, 0.95, na.rm = TRUE, type = 7))),
                  by = .(id, md = target_md)
    ]
    setnames(p95_tmmn, "p95", paste0("tmmn_p95_", nm))
    
    # merge the two for this baseline
    p95_b <- merge(p95_tmmx, p95_tmmn, by = c("id", "md"), all = TRUE)
    out_list[[nm]] <- p95_b
  }
  
  # merge both baseline tables together
  p95_all <- Reduce(function(a, b) merge(a, b, by = c("id", "md"), all = TRUE), out_list)
  
  # expand to full 366-day md calendar per station (so every station has all md rows)
  stations <- unique(dt_temp[[id_col]])
  stations <- clean_id(stations)
  grid <- CJ(id = stations, md = cal$md)
  
  p95_all <- merge(grid, p95_all, by = c("id", "md"), all.x = TRUE)
  
  # attach doy + coords
  p95_all <- merge(p95_all, cal, by = "md", all.x = TRUE)          # adds doy
  p95_all <- merge(p95_all, coords, by = "id", all.x = TRUE)       # adds lon/lat
  
  # final order + columns
  setorder(p95_all, id, doy)
  
  p95_all[, .(
    id,
    lon, lat,
    md, doy,
    tmmx_p95_1981_2010, tmmx_p95_1991_2020,
    tmmn_p95_1981_2010, tmmn_p95_1991_2020
  )]
}

## ------------------------- AirNow (GridMet)----------------------------------------
air_hist <- fread(FILE_GRIDMET_HIST)
air_main <- fread(FILE_GRIDMET_MAIN)

air_hist <- prep_temp_only(air_hist, id_col = "AQSID", time_col = "time")
air_main <- prep_temp_only(air_main, id_col = "AQSID", time_col = "time")

air_temp <- rbindlist(list(air_hist, air_main), use.names = TRUE, fill = TRUE)

air_lookup <- compute_md_p95_table(
  dt_temp = air_temp,
  id_col  = "AQSID",
  lon_col = "lon",
  lat_col = "lat",
  periods_list = periods,
  win = win_days
)

setnames(air_lookup, "id", SENSOR_ID)
fwrite(air_lookup, file = FILE_GRIDMET_P95)

message("DONE: wrote AirNow p95 table to: ", FILE_GRIDMET_P95)

## ------------------------- AirNow (DayMet) -----------------------------------
day_hist <- fread(FILE_DAYMET_HIST)
day_main <- fread(FILE_DAYMET_MAIN)

## DayMet uses tmax/tmin (°C); rename to tmmx/tmmn so compute_md_p95_table()
## works unchanged (output columns will read tmmx_p95_* / tmmn_p95_*;
## stage06 renames them to tmax_p95_* / tmin_p95_* on join).
setnames(day_hist, c("tmax", "tmin"), c("tmmx", "tmmn"), skip_absent = TRUE)
setnames(day_main, c("tmax", "tmin"), c("tmmx", "tmmn"), skip_absent = TRUE)

day_hist <- prep_temp_only(day_hist, id_col = "AQSID", time_col = "time")
day_main <- prep_temp_only(day_main, id_col = "AQSID", time_col = "time")

day_temp <- rbindlist(list(day_hist, day_main), use.names = TRUE, fill = TRUE)

day_lookup <- compute_md_p95_table(
  dt_temp = day_temp,
  id_col  = "AQSID",
  lon_col = "lon",
  lat_col = "lat",
  periods_list = periods,
  win = win_days
)

## Rename output columns back to DayMet convention (tmax/tmin)
setnames(day_lookup,
         c("tmmx_p95_1981_2010", "tmmx_p95_1991_2020",
           "tmmn_p95_1981_2010", "tmmn_p95_1991_2020"),
         c("tmax_p95_1981_2010", "tmax_p95_1991_2020",
           "tmin_p95_1981_2010", "tmin_p95_1991_2020"))

setnames(day_lookup, "id", SENSOR_ID)
fwrite(day_lookup, file = FILE_DAYMET_P95)

message("DONE: wrote AirNow DayMet p95 table to: ", FILE_DAYMET_P95)