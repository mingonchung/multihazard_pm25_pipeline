## =============================================================================
## util_pa_terrain_extract.R
## Utility — Extract elevation, slope, and aspect for PurpleAir sensor locations
##
## Derives terrain attributes from AWS Terrain Tiles (Mapzen) via the
## `elevatr` R package at zoom level 12 (~20–30 m resolution). Slope and
## aspect are computed after projecting to EPSG:5070 (Albers Equal Area,
## metres) and the output is written in WGS84 (EPSG:4326).
##
## Aligned with util_terrain_extract.R (AirNow); key differences:
##   - STATION replaces AQSID as the sensor identifier
##   - Handles outdoor/indoor via command-line argument
##   - Clips to US+Canada using FILE_USCAN_SHP (consistent with PA pipeline)
##
## Usage:
##   Rscript util_pa_terrain_extract.R outdoor
##   Rscript util_pa_terrain_extract.R indoor
##
## Input:   FILE_PA_{TYPE}_LOC_SHP  (from util_pa_sensor_location_shp.R)
## Output:  FILE_PA_{TYPE}_TERRAIN_SHP — same points with elev_m, slope_deg,
##            aspect_deg appended
##
## Run once before stage09_pa_attribute_join.R. Requires internet access for
## the AWS Terrain Tiles download (via elevatr::get_elev_raster).
##
## Dependencies: elevatr, terra, sf, config_pa.R
## =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(elevatr)
  library(dplyr)
  library(stringr)
  library(tigris)
})

options(tigris_use_cache = TRUE, tigris_class = "sf")

source("config_pa.R")

## ---------------------------------------------------------------------------
## 0. Parse argument
## ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
TYPE <- if (length(args) >= 1) tolower(trimws(args[1])) else stop(
  "Usage: Rscript util_pa_terrain_extract.R <outdoor|indoor>"
)
if (!TYPE %in% c("outdoor", "indoor"))
  stop("type must be 'outdoor' or 'indoor'")

in_shp  <- pa_loc_shp_file(TYPE, attrs = FALSE)
out_shp <- pa_terrain_shp_file(TYPE)

message(sprintf("=== Terrain extraction: %s ===", TYPE))
message("  Input:  ", in_shp)
message("  Output: ", out_shp)

## ---------------------------------------------------------------------------
## 1. Configuration
## ---------------------------------------------------------------------------
dem_zoom       <- 12L             # ~20–30 m resolution
tile_deg       <- 1               # geographic tile size (degrees)
field_elev     <- "elev_m"        # ≤10 chars for SHP compatibility
field_slope    <- "slope_deg"
field_aspect   <- "aspect_deg"
processing_crs <- "EPSG:5070"     # Albers Equal Area (metres) for terrain calcs

## ---------------------------------------------------------------------------
## 2. Helper functions
## ---------------------------------------------------------------------------

## Create a regular grid of tiles over a bounding box
make_tiles <- function(bbox, step = 1) {
  lons <- seq(floor(bbox["xmin"]), ceiling(bbox["xmax"]) - step, by = step)
  lats <- seq(floor(bbox["ymin"]), ceiling(bbox["ymax"]) - step, by = step)
  polys <- list()
  for (x in lons) for (y in lats) {
    polys[[length(polys) + 1]] <- st_polygon(list(matrix(
      c(x, y,  x + step, y,  x + step, y + step,  x, y + step,  x, y),
      ncol = 2, byrow = TRUE
    )))
  }
  st_sf(geometry = st_sfc(polys, crs = 4326))
}

## Ensure an object is an sf with a valid geometry column
as_sf_geom <- function(x, crs = 4326) {
  if (inherits(x, "sf")) return(x)
  if (inherits(x, "sfg")) x <- st_sfc(x, crs = crs)
  if (inherits(x, "sfc")) return(st_sf(geometry = x))
  stop("Unsupported geometry type passed to as_sf_geom()")
}

## Process a single tile: download DEM, compute terrain, extract at points
process_tile <- function(tile_sf, pts_wgs84, z = dem_zoom, crs_m = processing_crs) {
  tile_sf  <- as_sf_geom(tile_sf, crs = 4326)
  tile_buf <- st_transform(tile_sf, 3857) |> st_buffer(2000) |> st_transform(4326)

  ii <- st_intersects(pts_wgs84, tile_sf, sparse = FALSE)[, 1]
  if (!any(ii)) return(NULL)
  pts_tile <- pts_wgs84[ii, ]

  dem_r <- tryCatch(
    get_elev_raster(tile_buf, z = z, clip = "locations"),
    error = function(e) NULL
  )
  if (is.null(dem_r)) return(NULL)

  dem   <- rast(dem_r)
  dem_m <- project(dem, crs_m, method = "bilinear")

  slope  <- terrain(dem_m, v = "slope",  unit = "degrees")
  aspect <- terrain(dem_m, v = "aspect", unit = "degrees")

  pts_m <- st_transform(pts_tile, crs(dem_m))
  elev_vals   <- terra::extract(dem_m, vect(pts_m), method = "bilinear")[, 2]
  slope_vals  <- terra::extract(slope,  vect(pts_m), method = "bilinear")[, 2]
  aspect_vals <- terra::extract(aspect, vect(pts_m), method = "bilinear")[, 2]

  out <- pts_tile
  for (nm in c(field_elev, field_slope, field_aspect)) {
    if (nm %in% names(out)) out[[nm]] <- NULL
  }
  out[[field_elev]]   <- as.numeric(elev_vals)
  out[[field_slope]]  <- as.numeric(slope_vals)
  out[[field_aspect]] <- as.numeric(aspect_vals)
  out
}

## ---------------------------------------------------------------------------
## 3. Load sensor locations and clip to US + Canada
## ---------------------------------------------------------------------------
if (!file.exists(in_shp))
  stop("Input shp not found: ", in_shp,
       "\nRun util_pa_sensor_location_shp.R first.")

message("Reading sensor locations: ", in_shp)
pts <- st_read(in_shp, quiet = TRUE)
if (is.na(st_crs(pts))) st_crs(pts) <- 4326
pts <- st_transform(pts, 4326) |> st_make_valid()
pts <- st_wrap_dateline(pts, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

message(sprintf("  Total sensors: %d", nrow(pts)))

## Clip to US-only using tigris states
message("Loading US states (TIGER/Line 2020) ...")
states_sf <- states(year = 2020, cb = TRUE, progress_bar = FALSE) |> 
  st_transform(4326)
us_union  <- st_union(states_sf) |> st_make_valid()

# ADD THIS LINE for Alaska/Aleutian island consistency:
us_union <- st_wrap_dateline(us_union, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

before_n <- nrow(pts)

keep <- st_intersects(pts, us_union, sparse = FALSE)[, 1]
pts  <- pts[keep, ]

message(sprintf("  After US-only clip: %d (dropped %d)", nrow(pts), before_n - nrow(pts)))

## ---------------------------------------------------------------------------
## 4. Tile and extract terrain
## ---------------------------------------------------------------------------
grid    <- make_tiles(st_bbox(pts), step = tile_deg)
has_pts <- st_intersects(grid, st_geometry(pts), sparse = TRUE)
grid    <- grid[lengths(has_pts) > 0, , drop = FALSE]

message(sprintf("Processing %d tiles at zoom = %d ...", nrow(grid), dem_zoom))

pieces <- vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
  if (i %% 25 == 0 || i == 1) {
    message(sprintf("  Tile %d / %d", i, nrow(grid)))
  }
  pieces[[i]] <- process_tile(grid[i, ], pts)
}

pieces <- pieces[!sapply(pieces, is.null)]
if (length(pieces) == 0L) stop("No DEM tiles fetched — check internet access.")

res <- do.call(rbind, pieces)
res <- st_transform(res, 4326)

## Truncate field names to 10 characters for shapefile compatibility
names(res) <- substr(names(res), 1, 10)

# FIX: Remove any lingering duplicates based on identical spatial geometries
res <- res[!duplicated(st_geometry(res)), ]

## ---------------------------------------------------------------------------
## 5. Write output
## ---------------------------------------------------------------------------
out_dir <- dirname(out_shp)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

st_write(res, out_shp, delete_layer = TRUE, quiet = TRUE)

message(sprintf("Wrote %d sensors with terrain attributes to: %s",
                nrow(res), out_shp))
message(sprintf("  elev_m  range: [%.1f, %.1f]",
                min(res[[field_elev]], na.rm = TRUE),
                max(res[[field_elev]], na.rm = TRUE)))
message(sprintf("  slope   range: [%.2f, %.2f]",
                min(res[[field_slope]], na.rm = TRUE),
                max(res[[field_slope]], na.rm = TRUE)))
message(sprintf("  aspect  range: [%.1f, %.1f]",
                min(res[[field_aspect]], na.rm = TRUE),
                max(res[[field_aspect]], na.rm = TRUE)))
