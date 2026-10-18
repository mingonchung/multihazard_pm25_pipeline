rm(list = ls())

library(sf)
library(dplyr)
library(readr)
library(lubridate)
library(stringr)

# GEOS rather than S2 — consistent with stage01
sf_use_s2(FALSE)

source("config.R")

## ---------------------------------------------------------------------------
## 0. Helper functions
## ---------------------------------------------------------------------------

normalize_aqsid <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)

  x <- dplyr::na_if(x, "'NA")
  x <- dplyr::na_if(x, "NA")
  x <- dplyr::na_if(x, "")

  x <- stringr::str_replace(x, "^'+", "")   # remove leading apostrophes
  x <- stringr::str_trim(x)

  is_num <- !is.na(x) & stringr::str_detect(x, "^[0-9]+$")

  # pad numeric AQSIDs shorter than 9 chars
  x[is_num] <- ifelse(
    nchar(x[is_num]) < 9,
    stringr::str_pad(x[is_num], width = 9, side = "left", pad = "0"),
    x[is_num]
  )
  x
}

## ---------------------------------------------------------------------------
## 1. Load sensor locations
## ---------------------------------------------------------------------------
## FILE_AIRNOW_LOCS contains AQSID, AQSID_rev, lat_numeric, long_numeric.
## AQSID_rev is a co-located instrument at the same physical point.
## Stage 6 builds its hour spine from *both* AQSID and AQSID_rev and then
## joins smoke flags on the column named AQSID.  We therefore emit a row for
## each non-NA identifier so that both get smoke/fire assignments.

locs_raw <- read_csv(FILE_AIRNOW_LOCS,
                     col_types = cols(
                       AQSID        = col_character(),
                       AQSID_rev    = col_character(),
                       lat_numeric  = col_double(),
                       long_numeric = col_double()
                     )) %>%
  mutate(AQSID     = normalize_aqsid(AQSID),
         AQSID_rev = normalize_aqsid(AQSID_rev)) %>%
  filter(!is.na(lat_numeric) & !is.na(long_numeric)) %>%
  distinct(AQSID, .keep_all = TRUE)

## Build an sf object keyed by row index — one row per unique physical location.
## We carry AQSID_rev so we can duplicate rows for co-located instruments below.
sensors_sf <- st_as_sf(locs_raw,
                       coords = c("long_numeric", "lat_numeric"),
                       crs    = 4326,
                       remove = FALSE)

message(sprintf("Loaded %d unique sensor locations.", nrow(sensors_sf)))

## ---------------------------------------------------------------------------
## 2. Enumerate Stage 1 shapefiles
## ---------------------------------------------------------------------------
## Stage 1 writes: DIR_HMS_SMOKE/fired/hms_smoke_fired_YYYYMMDD.shp
shp_dir   <- file.path(DIR_HMS_SMOKE, "fired")
shp_files <- list.files(shp_dir,
                        pattern   = "^hms_smoke_fired_\\d{8}\\.shp$",
                        full.names = TRUE)

if (length(shp_files) == 0L)
  stop("No Stage 1 shapefiles found in: ", shp_dir,
       "\nRun stage01_hms_fired_join.R first.")

shp_dates <- as.Date(str_extract(basename(shp_files), "\\d{8}"), "%Y%m%d")
in_range  <- !is.na(shp_dates) &
             shp_dates >= STUDY_START &
             shp_dates <= STUDY_END
shp_files <- shp_files[in_range]
shp_dates <- shp_dates[in_range]

message(sprintf("Processing %d daily shapefiles (%s to %s).",
                length(shp_files), STUDY_START, STUDY_END))

## ---------------------------------------------------------------------------
## 3. Day-by-day spatial intersection
## ---------------------------------------------------------------------------
all_rows <- vector("list", length(shp_files))

for (i in seq_along(shp_files)) {

  this_date <- shp_dates[i]

  ## --- read smoke polygons ---
  smoke_sf <- tryCatch(
    st_read(shp_files[i], quiet = TRUE),
    error = function(e) {
      warning(sprintf("%s: could not read shapefile — %s", this_date,
                      conditionMessage(e)))
      NULL
    }
  )
  if (is.null(smoke_sf) || nrow(smoke_sf) == 0L) next

  ## --- align CRS (HMS output is WGS84 but confirm) ---
  if (!identical(st_crs(smoke_sf), st_crs(sensors_sf)))
    smoke_sf <- st_transform(smoke_sf, crs = st_crs(sensors_sf))

  smoke_sf <- st_make_valid(smoke_sf)

  ## --- sparse point-in-polygon: sensors (rows) × polygons (cols) ---
  intr    <- st_intersects(sensors_sf, smoke_sf, sparse = TRUE)
  hit_idx <- which(lengths(intr) > 0L)
  if (length(hit_idx) == 0L) next

  density <- rep(NA_character_, nrow(sensors_sf))
  frd     <- rep(NA_character_, nrow(sensors_sf))

  for (j in hit_idx) {

    hits <- intr[[j]]
    sub  <- smoke_sf[hits, ]

    # density priority — matches original case_when logic
    dens <- dplyr::case_when(
      "Heavy"  %in% sub$Density ~ "Heavy",
      "Medium" %in% sub$Density ~ "Medium",
      "Light"  %in% sub$Density ~ "Light",
      TRUE                       ~ NA_character_
    )

    # frd_d_s from polygons matching the winning density only
    raw_vals   <- sub$frd_d_s[sub$Density == dens]
    clean_vals <- raw_vals[!is.na(raw_vals)]

    if (length(clean_vals) == 0L) {
      fval <- NA_character_
    } else {
      txts <- format(clean_vals, scientific = FALSE, trim = TRUE)
      fval <- paste(sort(unique(txts)), collapse = "; ")
    }

    density[j] <- dens
    frd[j]     <- fval
  }

  ## build daily table (no geometry) — one row per sensor location
  tbl_primary <- sensors_sf %>%
    st_set_geometry(NULL) %>%
    transmute(
      date      = this_date,
      sensor_id = AQSID,
      density   = density,
      frd_d_s   = frd
    )

  ## duplicate rows for AQSID_rev (co-located instruments)
  tbl_rev <- sensors_sf %>%
    st_set_geometry(NULL) %>%
    filter(!is.na(AQSID_rev) & AQSID_rev != AQSID) %>%
    transmute(
      date      = this_date,
      sensor_id = AQSID_rev,
      density   = density[match(AQSID, locs_raw$AQSID)],
      frd_d_s   = frd[match(AQSID, locs_raw$AQSID)]
    )

  all_rows[[i]] <- bind_rows(tbl_primary, tbl_rev)

  if (i %% 100 == 0 || i == length(shp_files))
    message(sprintf("  [%d / %d]  %s  — %d sensor locations hit",
                    i, length(shp_files), this_date, length(hit_idx)))
}

## ---------------------------------------------------------------------------
## 4. Combine, deduplicate, and write
## ---------------------------------------------------------------------------
out <- bind_rows(Filter(Negate(is.null), all_rows))

if (nrow(out) == 0L)
  stop("No sensor-smoke intersections produced. ",
       "Check CRS consistency between shapefiles and sensor coordinates.")

## Keep highest-density row when a sensor appears twice for the same date
## (can happen via AQSID_rev duplication)
density_rank <- c("Heavy" = 3L, "Medium" = 2L, "Light" = 1L)
out <- out %>%
  mutate(.rank = dplyr::coalesce(density_rank[density], 0L)) %>%
  arrange(sensor_id, date, desc(.rank)) %>%
  distinct(sensor_id, date, .keep_all = TRUE) %>%
  select(-.rank)

message(sprintf(
  "Output: %d sensor \u00d7 date records | %d unique sensor IDs | %d unique dates.",
  nrow(out), dplyr::n_distinct(out$sensor_id), dplyr::n_distinct(out$date)
))

dir.create(dirname(FILE_HMS_FIRED_OUT), showWarnings = FALSE, recursive = TRUE)
write.csv(out, gzfile(FILE_HMS_FIRED_OUT), row.names = FALSE)
message("\u2713 Wrote: ", FILE_HMS_FIRED_OUT)
