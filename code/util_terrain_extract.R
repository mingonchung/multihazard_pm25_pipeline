## =============================================================================
## util_terrain_extract.R
## Utility — Extract elevation, slope, and aspect for sensor locations
##
## Derives terrain attributes from AWS Terrain Tiles (Mapzen) via the
## `elevatr` R package at zoom level 12 (~20–30 m resolution). Slope and
## aspect are computed after projecting to EPSG:5070 (Albers Equal Area,
## metres) and the output is written in WGS84 (EPSG:4326).
##
## Input:   FILE_AIRNOW_LOC_SHP  — sensor location point shapefile
## Output:  FILE_AIRNOW_TERRAIN_SHP — same points with elev_m, slope_deg,
##            aspect_deg appended
##
## Run once before stage09_attribute_join.R. Requires internet access for
## the AWS Terrain Tiles download (via elevatr::get_elev_raster).
##
## Dependencies: elevatr, terra, sf, tigris
## =============================================================================

rm(list = ls())

library(sf)
library(terra)
library(elevatr)
library(dplyr)
library(stringr)
library(tigris)
options(tigris_use_cache = TRUE, tigris_class = "sf")

source("config.R")

## ---------------------------------------------------------------------------
## 0. Configuration
## ---------------------------------------------------------------------------
in_shp         <- FILE_AIRNOW_LOC_SHP
out_shp        <- FILE_AIRNOW_TERRAIN_SHP

dem_zoom       <- 12L             # ~20–30 m resolution
tile_deg       <- 1               # geographic tile size (degrees)
field_elev     <- "elev_m"        # ≤10 chars for SHP compatibility
field_slope    <- "slope_deg"
field_aspect   <- "aspect_deg"
processing_crs <- "EPSG:5070"     # Albers Equal Area (metres) for terrain calcs

## ---------------------------------------------------------------------------
## 1. Helper functions
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

## US-only polygon (AK, HI, PR included)
get_us_only <- function() {
  states_sf <- states(year = 2020, cb = TRUE, progress_bar = FALSE) |> 
    st_transform(4326)
  
  us_poly <- st_union(states_sf) |> st_make_valid()
  st_wrap_dateline(us_poly, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
}

## Clip points to US boundary
clip_to_us <- function(pts_wgs84, us_poly) {
  keep <- st_intersects(pts_wgs84, us_poly, sparse = FALSE)[, 1]
  pts_wgs84[keep, ]
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
## 2. Load sensor locations and clip to US + Canada
## ---------------------------------------------------------------------------
message("Reading sensor locations: ", in_shp)
pts <- st_read(in_shp, quiet = TRUE)
if (is.na(st_crs(pts))) st_crs(pts) <- 4326
pts <- st_transform(pts, 4326) |> st_make_valid()
pts <- st_wrap_dateline(pts, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

message(sprintf("  Total sensors: %d", nrow(pts)))

us_poly <- get_us_only()
before_n <- nrow(pts)
pts <- clip_to_us(pts, us_poly)
message(sprintf("  After US-only clip: %d (dropped %d)", nrow(pts), before_n - nrow(pts)))

if (nrow(pts) == 0L) stop("No sensor points remain after clipping to the US.")

## ---------------------------------------------------------------------------
## 3. Tile and extract terrain
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
## 4. Write output
## ---------------------------------------------------------------------------
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
