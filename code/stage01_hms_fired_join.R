rm(list=ls())

library(sf)
library(dplyr)

# use GEOS (more forgiving) rather than S2
sf::sf_use_s2(FALSE)

source("config.R")

## 1. load FIRED event delineations
fired_sf <- st_read(FILE_FIRED_EVENTS,
                    layer = "fired_usa_2000_to_2026_events") %>%
  st_set_crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs") %>%
  mutate(
    ig_date   = as.Date(ig_date),
    last_date = as.Date(last_date)
  )

## 2. locate HMS smoke zip files
hms_dir    <- DIR_HMS_SMOKE
output_dir <- file.path(hms_dir, "fired")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)   # create if missing

zip_files  <- list.files(hms_dir,
                         "^hms_smoke\\d{8}\\.zip$",
                         full.names = TRUE)

for(zip_path in zip_files) {
  day_str   <- sub(".*hms_smoke(\\d{8})\\.zip", "\\1", zip_path)
  this_date <- as.Date(day_str, "%Y%m%d")

  # --- skip if already processed (re-run safe) ---
  out_shp <- file.path(output_dir, sprintf("hms_smoke_fired_%s.shp", day_str))
  if (file.exists(out_shp)) {
    message("✔ skip (exists) ", this_date)
    next
  }

  # --- read raw smoke & tag ---
  shp_name <- paste0("hms_smoke", day_str, ".shp")
  vsizip   <- sprintf("/vsizip/%s/%s", zip_path, shp_name)
  smoke_sf <- tryCatch(
    st_read(vsizip, quiet = TRUE) %>%
      mutate(smoke_id = row_number(), date = this_date),
    error = function(e) {
      warning(sprintf("%s: could not read zip — %s", day_str, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(smoke_sf) || nrow(smoke_sf) == 0L) next

  # --- drop invalid polygons ---
  valid_idx <- which(st_is_valid(smoke_sf))
  if (length(valid_idx) < nrow(smoke_sf)) {
    warning(sprintf("%s: dropping %d invalid smoke polygons",
                    day_str, nrow(smoke_sf) - length(valid_idx)))
    smoke_sf <- smoke_sf[valid_idx, ]
  }

  # --- project to EPSG:5070 (Albers CONUS) for all spatial operations ---
  # st_buffer(0) and st_intersects require a planar CRS; calling them on
  # WGS84 (lon/lat) produces repeated "arc_degrees" and "assumes planar"
  # warnings and may give incorrect results near antimeridian or poles.
  smoke_prj <- tryCatch({
    st_transform(smoke_sf, 5070L) %>% st_make_valid() %>% st_buffer(0)
  }, error = function(e) {
    warning(sprintf("%s: smoke geometry cleanup failed — %s", day_str, conditionMessage(e)))
    st_transform(smoke_sf, 5070L) %>% st_make_valid()
  })

  # --- filter & reproject active fires to EPSG:5070 ---
  fires_active <- fired_sf %>%
    filter(ig_date <= this_date, last_date >= this_date) %>%
    select(id)

  fires_active_prj <- tryCatch({
    fires_active %>%
      st_transform(5070L) %>%
      st_make_valid() %>%
      st_buffer(0)
  }, error = function(e) {
    warning(sprintf("%s: fire geometry cleanup failed — %s", day_str, conditionMessage(e)))
    fires_active %>%
      st_transform(5070L) %>%
      st_make_valid()
  })

  # --- spatial join (both layers now in EPSG:5070) ---
  joined <- st_join(smoke_prj, fires_active_prj, join = st_intersects, left = TRUE)

  # --- propagate FIRED IDs to smoke polygons that missed the join ---
  light_hits <- joined %>% filter(!is.na(id))
  to_fill    <- which(is.na(joined$id))

  if (length(to_fill) > 0 && nrow(light_hits) > 0) {
    intrs   <- st_intersects(joined[to_fill, ], light_hits)
    new_ids <- sapply(intrs, function(ix) if (length(ix) == 0) NA_integer_ else light_hits$id[ix[1]])
    joined$id[to_fill] <- new_ids
  }

  # --- collapse to one row per smoke polygon ---
  # Column named frd_d_s matches stage02 (sub$frd_d_s) and stage06 col_types
  collapsed <- joined %>%
    group_by(smoke_id, date, Satellite, Start, End, Density, geometry) %>%
    summarize(
      frd_d_s = {
        ids <- sort(unique(na.omit(id)))
        if (length(ids) == 0) NA_character_ else paste(ids, collapse = ";")
      },
      .groups = "drop"
    )

  # --- reproject output back to WGS84 for stage02 compatibility ---
  collapsed <- st_transform(collapsed, 4326L)

  # --- write ---
  st_write(collapsed, out_shp, delete_layer = TRUE, quiet = TRUE)
  message("✔ ", this_date, " → ", nrow(collapsed), " features")
}
