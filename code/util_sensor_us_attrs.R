rm(list=ls())
   
library(sf)
library(dplyr)
library(stringr)
library(tigris)

source("config.R")

options(tigris_use_cache = TRUE, tigris_class = "sf")

# ----------------------------
# INPUTS
# ----------------------------
inputs <- c(
  FILE_AIRNOW_LOC_SHP
)

# Urban Areas 2020 shapefile (from config.R)
ua_path <- FILE_URBAN_SHP

# ----------------------------
# LOAD BOUNDARIES (2020)
# ----------------------------
message("Loading US states and counties (TIGER/Line 2020)…")
states  <- states(year = 2020, cb = TRUE) |> st_transform(4326)
counties <- counties(year = 2020, cb = TRUE) |> st_transform(4326)

# Union of all states for US-only clipping
us_union <- st_union(states) |> st_make_valid()

# Load Urban Areas 2020
message("Loading Urban Areas 2020…")
ua <- st_read(ua_path, quiet = TRUE) |> st_transform(4326) |> st_make_valid()

# Try to find a readable Urban Area name/ID column
cn <- names(ua)

# Prefer NAME20 or NAME (common on cb_2020_us_ua20_500k)
cand <- grep("^(NAME20|NAME)$", cn, ignore.case = TRUE, value = TRUE)
if (length(cand) == 0) {
  # Fallback to a code/ID if no name field exists
  cand <- grep("(GEOID20|GEOID|UACE20|UACE)", cn, ignore.case = TRUE, value = TRUE)
}
if (length(cand) == 0) {
  stop("Could not find a suitable UA name/id column in the UA shapefile. ",
       "Columns are: ", paste(cn, collapse = ", "))
}
ua_name_col <- cand[1]

# ----------------------------
# HELPERS
# ----------------------------
ensure_sf_points <- function(obj) {
  if (inherits(obj, "sf") && !is.null(st_geometry(obj))) return(obj)
  cn <- tolower(names(obj))
  lon_col <- c("lon","long","longitude","x") |> (\(c) c[c %in% cn])()
  lat_col <- c("lat","latitude","y")        |> (\(c) c[c %in% cn])()
  if (length(lon_col) && length(lat_col)) {
    st_as_sf(obj, coords = c(lon_col[1], lat_col[1]), crs = 4326, remove = FALSE)
  } else {
    stop("Layer has no geometry and no recognizable lon/lat columns.")
  }
}

add_us_only <- function(pts) {
  # Keep points that intersect US (includes AK, HI, PR, territories present in TIGER states)
  keep <- st_intersects(pts, us_union, sparse = FALSE)[,1]
  pts[keep, ]
}

add_urban_flag <- function(pts) {
  # Intersect with UA polygons; TRUE => Urban, FALSE => Rural
  idx <- st_intersects(pts, ua)
  is_urban <- lengths(idx) > 0
  ua_name <- rep(NA_character_, nrow(pts))
  if (any(is_urban)) {
    # For points that hit multiple UAs (rare), take the first
    ua_idx_first <- sapply(idx[is_urban], function(x) x[1])
    ua_name[is_urban] <- ua[[ua_name_col]][ua_idx_first]
  }
  pts$urban   <- ifelse(is_urban, 1L, 0L)          # 1 = Urban, 0 = Rural
  pts$ua_name <- ua_name                           # Urban Area name/id
  pts
}

add_state_county <- function(pts) {
  # Spatial join: counties then states
  pts_c <- st_join(pts, counties |> select(STATEFP, COUNTYFP, NAME, GEOID), left = TRUE, largest = TRUE)
  names(pts_c)[names(pts_c) == "NAME"] <- "cnty_name"
  names(pts_c)[names(pts_c) == "GEOID"] <- "cnty_geoid"
  
  pts_cs <- st_join(pts_c, states |> select(STATEFP, STUSPS, NAME), left = TRUE, largest = TRUE)
  names(pts_cs)[names(pts_cs) == "NAME"] <- "state_name"
  names(pts_cs)[names(pts_cs) == "STATEFP.x"] <- "county_stfp"
  names(pts_cs)[names(pts_cs) == "STATEFP.y"] <- "statefp"
  
  # Keep concise, Shapefile-safe columns (≤10 chars)
  # - urban (0/1), ua_name (UA name/id), statefp, stusps, state_name, countyfp, cnty_name, cnty_geoid
  keep_cols <- c("urban","ua_name","statefp","STUSPS","state_name","COUNTYFP","cnty_name","cnty_geoid")
  keep_cols <- keep_cols[keep_cols %in% names(pts_cs)]
  pts_cs <- pts_cs |> select(any_of(keep_cols), everything())
  
  # Rename to ≤10 chars
  safe_names <- names(pts_cs)
  safe_names[safe_names == "STUSPS"]     <- "stusps"
  safe_names[safe_names == "state_name"] <- "st_name"
  safe_names[safe_names == "COUNTYFP"]   <- "cntyfp"
  safe_names[safe_names == "cnty_name"]  <- "cnty_name"
  safe_names[safe_names == "cnty_geoid"] <- "cnty_geoid"
  names(pts_cs) <- substr(safe_names, 1, 10)
  
  pts_cs
}

process_one <- function(in_path) {
  message("Processing: ", in_path)
  pts <- suppressWarnings(st_read(in_path, quiet = TRUE))
  pts <- ensure_sf_points(pts)
  if (is.na(st_crs(pts))) st_crs(pts) <- 4326
  pts <- st_transform(pts, 4326)
  pts <- st_make_valid(pts)
  pts <- st_wrap_dateline(pts, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"))
  
  n0 <- nrow(pts)
  pts <- add_us_only(pts)
  n1 <- nrow(pts)
  message("US-only filter: kept ", n1, " of ", n0, " points.")
  
  if (n1 == 0) {
    warning("No US points remain after filter: ", in_path)
    return(NA_character_)
  }
  
  pts <- add_urban_flag(pts)
  pts <- add_state_county(pts)
  
  # Ensure SHP-safe names and CRS
  pts <- st_transform(pts, 4326)
  names(pts) <- substr(names(pts), 1, 10)
  
  out_path <- sub("\\.shp$", "_us_attrs.shp", in_path)
  st_write(pts, out_path, delete_layer = TRUE, quiet = TRUE)
  message("Wrote: ", out_path)
  out_path
}

# ----------------------------
# RUN
# ----------------------------
out_paths <- vapply(inputs, process_one, character(1))
cat("\nDone. Output files (NA = no US points):\n", paste(out_paths, collapse = "\n"), "\n")
