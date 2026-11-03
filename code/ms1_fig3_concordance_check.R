## =============================================================================
## ms1_fig3_concordance_check.R
## MS1 Figure 3 — representative event capture maps (eight events) and
## event concordance table.
##
## For each event: sensors with a daily record on the event date inside the
## zoom window. Point classes:
##   - filled, hazard colour : the sensor-day's own hazard_cat, coloured as in
##                             the overview legend (compounds with Wildfire ->
##                             Wildfire (any); Heat+Drought -> Heat (any))
##   - filled, dark grey     : Baseline
##   - hollow, grey          : record present, hazard_cat NA (Unclassified)
## Points carrying the event's own category are drawn on top.
## Sensors without a record on the date are not drawn.
## Concordance (CSV) counts sensor-days in the event counties only.
##
## Input:   FILE_DAILY_INTEGRATED, FILE_PA_DAILY_INTEGRATED_OUTDOOR,
##          FILE_AIRNOW_USATTRS_SHP, FILE_PA_OUTDOOR_USATTRS_SHP
##          county and state boundaries: tigris (Census cartographic files, 2020)
## Output:  DIR_MS1_OUT/
##   ms1_fig3_ev_A_heat.svg         Multnomah + Lane OR, 28 Jun 2021
##   ms1_fig3_ev_B_wildfire.svg     Plumas + Butte CA, 7 Aug 2021
##   ms1_fig3_ev_C_volcano.svg      Hawai'i Co. HI, 9 Dec 2022
##   ms1_fig3_ev_D_flood.svg        Clark NV, 2 Sep 2023
##   ms1_fig3_ev_E_dust.svg         Pinal + Maricopa + Pima AZ, 25 Aug 2025
##   ms1_fig3_ev_F_tornado.svg      Cook + Will IL, Lake IN, 19 Mar 2025
##   ms1_fig3_ev_G_hurricane.svg    Jefferson + Lafourche + Orleans + St. Tammany LA,
##                                  28 Oct 2020 (Zeta)
##   ms1_fig3_ev_H_tropstorm.svg    Harris TX, 13 Sep 2021 (Nicholas)
##   ms1_fig3_us_overview.svg       US map, event counties, legends
##   ms1_fig3_composite.{svg,png}   overview + eight panels + leader lines
##   ms1_fig3_concordance_summary.csv
##   ms1_fig3_concordance_raw_sensors.csv
##
## Author notes on event selection (not verified in this script; cite sources
## before use in the text):
##   - Dust / Maricopa: few sensors classified Dust on 25 Aug 2025; possible
##     causes are MAIAC retrieval gaps under dense dust and monitor outages.
##   - D moved to 2 Sep 2023 (Las Vegas flash flooding, 1-2 Sep 2023); the
##     widely reported 28 Jul 2022 flood day is labelled Wildfire+Drought (smoke
##     overhead outranks Flood) and no flooding was reported for 25 Jul 2022.
##   - F, G, H replaced 2026-10-09 (were Chicago 28 Feb 2024, Claudette /
##     Jefferson AL, Ian / Duval FL), which carried no Tornado, Tropical Storm
##     or Hurricane label after the local-day and zone-level revisions
##     (ms1_fig3_event_candidates.R). New events checked against NWS and news
##     reports: NWS Chicago survey of 19 Mar 2025 (Lynwood, Cook Co.; Will Co.;
##     Lake Co. IN); Nicholas: Category 1 hurricane at landfall on the
##     Matagorda Peninsula, 05:30 UTC 14 Sep 2021 (23:30 CST 13 Sep, local
##     standard day 13 Sep); tropical-storm-force winds in Harris Co. (NHC
##     report: Morgan's Point 43 kt sustained; Hobby gust ~50 kt), hence the
##     panel title "Nicholas (Trop. Storm)". Hilary (San Diego, 20 Aug 2023)
##     was used before; replaced to balance western and eastern events. Zeta:
##     landfall on
##     28 Oct 2020 with Jefferson, Orleans, St. Tammany, Lafourche affected.
##   - Right column ordered so leader lines do not cross: G = Zeta (SE
##     Louisiana, mid-right), H = Nicholas (Houston, bottom-right); Houston lies
##     west of New Orleans, so the lower panel takes the western event.
## =============================================================================

rm(list = ls())

## ── 1. Packages ───────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(cowplot)
  library(svglite)
  library(tigris)
})
options(tigris_use_cache = TRUE, tigris_class = "sf")

source("config_pa.R")   # also sources config.R

## ---------------------------------------------------------------------------
## Font: Arial for all text (PNG and SVG)
## The cluster may have no Arial and may map the generic "sans" to a serif
## face. The family name "Arial" is therefore registered with systemfonts:
## the real Arial if installed or found in MS1_FONT_DIR (e.g. arial.ttf and
## arialbd.ttf copied from C:/Windows/Fonts to ~/.fonts), otherwise a
## metric-compatible substitute (Liberation Sans / Arimo / Helvetica / Nimbus
## Sans / DejaVu Sans). SVG files name "Arial", so they display in Arial on
## Windows and macOS. PNG files are drawn with ragg, which uses this font.
## ---------------------------------------------------------------------------
FONT <- "Arial"
## Font file path; match_fonts() from systemfonts 1.1.0, match_font() before
font_path <- function(family, bold = FALSE) {
  ns <- asNamespace("systemfonts")
  if ("match_fonts" %in% getNamespaceExports(ns)) {
    getExportedValue("systemfonts", "match_fonts")(family, weight = if (bold) "bold" else "normal")$path
  } else {
    systemfonts::match_font(family, bold = bold)$path
  }
}
setup_font <- function(family = FONT,
                       dir = Sys.getenv("MS1_FONT_DIR", "~/.fonts")) {
  fams <- unique(systemfonts::system_fonts()$family)
  if (family %in% fams) {
    message("Font: ", family, " (installed)")
    return(c(font_path(family), font_path(family, bold = TRUE)))
  }
  user <- path.expand(file.path(dir, c("arial.ttf", "arialbd.ttf")))
  if (file.exists(user[1])) {
    bold <- if (file.exists(user[2])) user[2] else user[1]
    systemfonts::register_font(family, plain = user[1], bold = bold,
                               italic = user[1], bolditalic = bold)
    message("Font: ", family, " registered from ", dirname(user[1]))
    return(c(user[1], bold))
  }
  for (alt in c("Liberation Sans", "Arimo", "Helvetica", "Nimbus Sans", "DejaVu Sans")) {
    if (alt %in% fams) {
      p <- font_path(alt)
      b <- font_path(alt, bold = TRUE)
      systemfonts::register_font(family, plain = p, bold = b, italic = p, bolditalic = b)
      message("Font: ", family, " not installed; drawn with ", alt, " (metrics) and named Arial in SVG")
      return(c(p, b))
    }
  }
  warning("No Arial or sans-serif font found; text falls back to the device default. ",
          "Copy arial.ttf and arialbd.ttf to ~/.fonts and re-run.")
  NULL
}
FONT_FILES <- setup_font()
## svglite: write font-family "Arial" in the SVG, with metrics from FONT_FILES
svg_fonts <- if (is.null(FONT_FILES)) list() else {
  ff <- list(plain      = list(alias = FONT, file = FONT_FILES[1]),
             bold       = list(alias = FONT, file = FONT_FILES[2]),
             italic     = list(alias = FONT, file = FONT_FILES[1]),
             bolditalic = list(alias = FONT, file = FONT_FILES[2]))
  setNames(list(ff, ff), c(FONT, "sans"))
}
if (!requireNamespace("ragg", quietly = TRUE))
  stop("Package 'ragg' is required for PNG output with the registered font.")
cowplot::set_null_device("agg")   # text measured with ragg (registered font)

## ── 2. Paths ──────────────────────────────────────────────────────────────────
out_dir <- file.path(ROOT_DIR, "ms1")
## Optional local (laptop) paths: if ms1_local_paths.R sits next to this
## script, it overrides the input paths and the output folder set above.
if (file.exists("ms1_local_paths.R")) source("ms1_local_paths.R")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## ── 3. CRS + themes ───────────────────────────────────────────────────────────
albers_crs <- st_crs("ESRI:102003")
excl_terr  <- c("60", "66", "69", "72", "78")

## US overview map theme (text is drawn with draw_label at explicit sizes)
base_map_theme <- list(
  theme_void(base_size = 11, base_family = FONT),
  theme(
    text             = element_text(size = 11),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.border     = element_blank(),
    plot.background  = element_rect(fill = "transparent", colour = NA),
    plot.margin      = margin(0, 0, 0, 0)
  )
)

## Individual event panel theme (white panel background)
event_panel_theme <- function() {
  theme_void(base_size = 18, base_family = FONT) +
    theme(
      plot.title       = element_text(size = 18, face = "bold",
                                      hjust = 0.5, margin = margin(t = 6, b = 4, l = 6, r = 6)),
      plot.subtitle    = element_text(size = 14,
                                      hjust = 0.5, margin = margin(b = 8, l = 6, r = 6)),
      legend.position  = "none",
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.margin      = margin(4, 4, 4, 4)
    )
}

## ── 4. Spatial data ───────────────────────────────────────────────────────────
message("Loading spatial data (tigris cache used after first run)...")

county_raw <- tigris::counties(cb = TRUE, resolution = "5m",
                               year = 2020, progress_bar = FALSE)

## Albers — US overview
county_alb <- county_raw |>
  filter(!STATEFP %in% excl_terr) |>
  st_transform(albers_crs) |>
  st_simplify(dTolerance = 2000, preserveTopology = TRUE) |>
  mutate(STATEFP = as.character(STATEFP))

## WGS84 — individual event maps
county_wgs <- county_raw |>
  filter(!STATEFP %in% excl_terr) |>
  mutate(STATEFP = as.character(STATEFP)) |>
  st_transform(4326) |>
  st_simplify(dTolerance = 0.005, preserveTopology = TRUE)

state_raw <- tigris::states(cb = TRUE, resolution = "20m",
                            year = 2020, progress_bar = FALSE) |>
  filter(!STATEFP %in% excl_terr)

state_alb <- state_raw |>
  st_transform(albers_crs) |>
  st_simplify(dTolerance = 3000, preserveTopology = TRUE)

state_wgs <- state_raw |>
  st_transform(4326) |>
  st_simplify(dTolerance = 0.005, preserveTopology = TRUE)

## Split by region (Fig. 2 pattern)
county_conus_alb <- county_alb |> filter(!STATEFP %in% c("02", "15"))
county_ak_alb    <- county_alb |> filter(STATEFP == "02")
county_hi_alb    <- county_alb |> filter(STATEFP == "15")
state_conus_alb  <- state_alb  |> filter(!STATEFP %in% c("02", "15"))
state_ak_alb     <- state_alb  |> filter(STATEFP == "02")

conus_bbox <- st_bbox(county_conus_alb)
ak_bbox    <- st_bbox(county_ak_alb)
hi_bbox    <- st_bbox(county_hi_alb)

## ── 5. Sensor SHP files ───────────────────────────────────────────────────────
message("Loading sensor shapefiles...")

air_pts_alb <- st_read(FILE_AIRNOW_USATTRS_SHP, quiet = TRUE) |>
  st_transform(albers_crs) |>
  mutate(AQSID_join = sub("^'+", "", trimws(as.character(AQSID))),
         statefp    = as.character(statefp)) |>
  filter(!statefp %in% excl_terr) |>
  select(AQSID_join, statefp)

pa_pts_alb <- st_read(FILE_PA_OUTDOOR_USATTRS_SHP, quiet = TRUE) |>
  st_transform(albers_crs) |>
  mutate(STATION = sub("^'+", "", trimws(as.character(STATION))),
         statefp = as.character(statefp)) |>
  filter(!statefp %in% excl_terr) |>
  select(STATION, statefp)

## WGS84 versions for event-level maps
air_pts_wgs <- st_transform(air_pts_alb, 4326)
pa_pts_wgs  <- st_transform(pa_pts_alb,  4326)

## ── 6. Classified daily data ──────────────────────────────────────────────────
message("Reading classified AirNow...")
airnow_dt <- fread(
  FILE_DAILY_INTEGRATED,
  na.strings = c("", "NA"),
  colClasses = list(character = c("AQSID", "AQSID_join"))
)[, .(AQSID_join = sub("^'+", "", trimws(AQSID_join)),
      date        = as.Date(date),
      cnty_geoid  = as.integer(cnty_geoid),
      hazard_cat, PM25)]
airnow_dt[hazard_cat == "",    hazard_cat := NA_character_]

message("Reading classified PurpleAir...")
purpleair_dt <- fread(
  FILE_PA_DAILY_INTEGRATED_OUTDOOR,
  na.strings = c("", "NA"),
  colClasses = list(character = "STATION")
)[, .(STATION    = sub("^'+", "", trimws(STATION)),
      date        = as.Date(date),
      cnty_geoid  = as.integer(cnty_geoid),
      hazard_cat, PM25)]
purpleair_dt[hazard_cat == "", hazard_cat := NA_character_]

## ── 7. Colours ────────────────────────────────────────────────────────────────
## Hazard colours, Table 2 order (same as Fig. 2 color_map)
ov_color_map <- c(
  "Volcano"         = "#6a3d9a",
  "Wildfire (any)"  = "#b2182b",
  "Dust"            = "#d8b365",
  "Heat (any)"      = "#f97b2b",
  "Tornado"         = "#8c510a",
  "Hurricane"       = "#74c476",
  "Tropical Storm"  = "#41ab5d",
  "Flood"           = "#1a7837"
)

## Non-hazard point classes on event panels
COL_BASELINE <- "#525252"   # filled
COL_NA       <- "#969696"   # hollow

## hazard_cat -> legend group (names of ov_color_map)
haz_group <- function(hazard_cat) {
  fcase(hazard_cat == "Volcano",                     "Volcano",
        grepl("^Wildfire", hazard_cat),              "Wildfire (any)",
        hazard_cat == "Dust",                        "Dust",
        hazard_cat %in% c("Heat", "Heat+Drought"),   "Heat (any)",
        hazard_cat == "Tornado",                     "Tornado",
        hazard_cat == "Hurricane",                   "Hurricane",
        hazard_cat == "TropicalStorm",               "Tropical Storm",
        hazard_cat == "Flood",                       "Flood",
        default = NA_character_)
}

PT_SIZE <- c("AirNow" = 3.8, "PurpleAir" = 2.0)

## ── 8. Event definitions ──────────────────────────────────────────────────────
events <- list(

  A_Heat = list(
    label       = "a PNW Heat Dome",
    date_str    = "Jun 28 2021",
    short       = "A_heat",
    date        = as.Date("2021-06-28"),
    cnty_geoids = c(41051L, 41039L),
    haz_cats    = c("Heat", "Heat+Drought"),
    statefp     = "41",
    zoom        = c(-124.5, -121.4, 43.3, 46.2),
    color       = ov_color_map[["Heat (any)"]],
    panel_lbl   = "a"
  ),

  B_Wildfire = list(
    label       = "b Dixie Fire",
    date_str    = "Aug 7 2021",
    short       = "B_wildfire",
    date        = as.Date("2021-08-07"),
    cnty_geoids = c(6063L, 6007L),
    haz_cats    = c("Wildfire", "Wildfire+Drought",
                    "Wildfire+Heat", "Wildfire+Heat+Drought"),
    statefp     = "06",
    zoom        = c(-122.2, -119.8, 38.5, 41.8),
    color       = ov_color_map[["Wildfire (any)"]],
    panel_lbl   = "b"
  ),

  C_Volcano = list(
    label       = "c Mauna Loa Eruption",
    date_str    = "Dec 9 2022",
    short       = "C_volcano",
    date        = as.Date("2022-12-09"),
    cnty_geoids = 15001L,
    haz_cats    = "Volcano",
    statefp     = "15",
    zoom        = c(-156.6, -154.3, 18.2, 21.0),
    color       = ov_color_map[["Volcano"]],
    panel_lbl   = "c"
  ),

  D_Flood = list(
    label       = "d Las Vegas Flood",
    date_str    = "Sep 2 2023",
    short       = "D_flood",
    date        = as.Date("2023-09-02"),
    cnty_geoids = 32003L,
    haz_cats    = "Flood",
    statefp     = "32",
    zoom        = c(-116.0, -113.9, 35.2, 37.4),
    color       = ov_color_map[["Flood"]],
    panel_lbl   = "d"
  ),

  E_Dust = list(
    label       = "e Arizona Haboob",
    date_str    = "Aug 25 2025",
    short       = "E_dust",
    date        = as.Date("2025-08-25"),
    cnty_geoids = c(4021L, 4013L, 4019L),
    haz_cats    = "Dust",
    statefp     = "04",
    zoom        = c(-113.5, -109.8, 30.6, 34.8),
    color       = ov_color_map[["Dust"]],
    panel_lbl   = "e"
  ),

  F_Tornado = list(
    label       = "f Chicago-area Tornadoes",
    date_str    = "Mar 19 2025",
    short       = "F_tornado",
    date        = as.Date("2025-03-19"),
    cnty_geoids = c(17031L, 17197L, 18089L),
    haz_cats    = "Tornado",
    statefp     = "17",
    zoom        = c(-88.4, -86.9, 41.0, 42.2),
    color       = ov_color_map[["Tornado"]],
    panel_lbl   = "f"
  ),

  G_Hurricane = list(
    label       = "g Hurricane Zeta",
    date_str    = "Oct 28 2020",
    short       = "G_hurricane",
    date        = as.Date("2020-10-28"),
    cnty_geoids = c(22051L, 22057L, 22071L, 22103L),
    haz_cats    = "Hurricane",
    statefp     = "22",
    zoom        = c(-91.4, -88.9, 28.8, 31.0),
    color       = ov_color_map[["Hurricane"]],
    panel_lbl   = "g"
  ),

  H_TropicalStorm = list(
    label       = "h Nicholas (Trop. Storm)",
    date_str    = "Sep 13 2021",
    short       = "H_tropstorm",
    date        = as.Date("2021-09-13"),
    cnty_geoids = 48201L,
    haz_cats    = "TropicalStorm",
    statefp     = "48",
    zoom        = c(-96.3, -94.6, 29.0, 30.6),
    color       = ov_color_map[["Tropical Storm"]],
    panel_lbl   = "h"
  )
)

## ── 9. Concordance summary and raw sensor data (event counties) ───────────────
message("Computing concordance summary and extracting raw sensor data...")

summary_and_raw <- lapply(names(events), function(nm) {
  ev  <- events[[nm]]

  an  <- airnow_dt[date == ev$date & cnty_geoid %in% ev$cnty_geoids]
  pa  <- purpleair_dt[date == ev$date & cnty_geoid %in% ev$cnty_geoids]

  sum_row <- data.table(
    panel         = ev$panel_lbl,
    event         = ev$label,
    date          = ev$date,
    n_counties    = length(ev$cnty_geoids),
    an_sensors    = uniqueN(an$AQSID_join),
    an_concordant = an[hazard_cat %in% ev$haz_cats, .N],
    an_hit_rate   = if (nrow(an) > 0)
      round(an[hazard_cat %in% ev$haz_cats, .N] / nrow(an), 3) else NA_real_,
    pa_sensors    = uniqueN(pa$STATION),
    pa_concordant = pa[hazard_cat %in% ev$haz_cats, .N],
    pa_hit_rate   = if (nrow(pa) > 0)
      round(pa[hazard_cat %in% ev$haz_cats, .N] / nrow(pa), 3) else NA_real_
  )

  an_raw <- copy(an)
  if (nrow(an_raw) > 0) {
    setnames(an_raw, old = "AQSID_join", new = "sensor_id", skip_absent = TRUE)
    an_raw[, `:=`(network = "AirNow", event = ev$label, panel = ev$panel_lbl)]
  }
  pa_raw <- copy(pa)
  if (nrow(pa_raw) > 0) {
    setnames(pa_raw, old = "STATION", new = "sensor_id", skip_absent = TRUE)
    pa_raw[, `:=`(network = "PurpleAir", event = ev$label, panel = ev$panel_lbl)]
  }

  list(summary = sum_row,
       raw     = rbindlist(list(an_raw, pa_raw), use.names = TRUE, fill = TRUE))
})

summary_tbl <- rbindlist(lapply(summary_and_raw, `[[`, "summary"))
fwrite(summary_tbl, file.path(out_dir, "ms1_fig3_concordance_summary.csv"))
message("  Concordance table written.")

raw_sensors_tbl <- rbindlist(lapply(summary_and_raw, `[[`, "raw"),
                             use.names = TRUE, fill = TRUE)
raw_out_path <- file.path(out_dir, "ms1_fig3_concordance_raw_sensors.csv")
fwrite(raw_sensors_tbl, raw_out_path)
message("  Raw sensor-level data written to: ", raw_out_path)

## ── 10. Helper: get_event_sensors() ──────────────────────────────────────────
## has_obs separates "no record" (inactive sensor, not drawn) from
## "record present, hazard_cat NA" (Unclassified, drawn hollow).
get_event_sensors <- function(ev) {
  zoom_bb <- st_bbox(c(
    xmin = ev$zoom[1] - 0.3, xmax = ev$zoom[2] + 0.3,
    ymin = ev$zoom[3] - 0.3, ymax = ev$zoom[4] + 0.3
  ), crs = st_crs(4326))

  an_day <- airnow_dt[date == ev$date,
                      .(AQSID_join, hazard_cat, PM25, cnty_geoid, has_obs = TRUE)]
  an_sf <- suppressWarnings(st_crop(air_pts_wgs, zoom_bb)) |>
    left_join(as.data.frame(an_day), by = "AQSID_join") |>
    filter(!is.na(has_obs)) |>
    mutate(in_focus = cnty_geoid %in% ev$cnty_geoids)

  pa_day <- purpleair_dt[date == ev$date,
                         .(STATION, hazard_cat, PM25, cnty_geoid, has_obs = TRUE)]
  pa_sf <- suppressWarnings(st_crop(pa_pts_wgs, zoom_bb)) |>
    left_join(as.data.frame(pa_day), by = "STATION") |>
    filter(!is.na(has_obs)) |>
    mutate(in_focus = cnty_geoid %in% ev$cnty_geoids)

  list(airnow = an_sf, purpleair = pa_sf)
}

## Point colour and shape: own hazard colour, Baseline dark grey, NA hollow
point_style <- function(hazard_cat) {
  grp <- haz_group(hazard_cat)
  list(col = fcase(!is.na(grp),              unname(ov_color_map[grp]),
                   hazard_cat == "Baseline", COL_BASELINE,
                   default = COL_NA),
       shp = ifelse(is.na(hazard_cat), 1L, 16L))
}

## ── 11. Helper: build individual event panel ──────────────────────────────────
build_event_map <- function(ev) {
  sensors <- get_event_sensors(ev)
  an_sf   <- sensors$airnow
  pa_sf   <- sensors$purpleair

  zoom_bb    <- st_bbox(c(xmin = ev$zoom[1], xmax = ev$zoom[2],
                          ymin = ev$zoom[3], ymax = ev$zoom[4]),
                        crs = st_crs(4326))
  cnty_crop  <- suppressWarnings(st_crop(county_wgs, zoom_bb))
  state_crop <- suppressWarnings(st_crop(state_wgs,  zoom_bb))
  focus_sf   <- cnty_crop |> filter(as.integer(GEOID) %in% ev$cnty_geoids)

  ## Concordance counts (event counties, for subtitle)
  n_an   <- sum(an_sf$in_focus)
  n_an_c <- sum(an_sf$in_focus & an_sf$hazard_cat %in% ev$haz_cats, na.rm = TRUE)
  n_pa   <- sum(pa_sf$in_focus)
  n_pa_c <- sum(pa_sf$in_focus & pa_sf$hazard_cat %in% ev$haz_cats, na.rm = TRUE)

  subtitle <- sprintf("%s, AN %d/%d PA %d/%d",
                      ev$date_str, n_an_c, n_an, n_pa_c, n_pa)

  prep_sf <- function(sf_obj, nw_label) {
    if (nrow(sf_obj) == 0) return(NULL)
    sf_obj |>
      mutate(network = nw_label) |>
      select(network, in_focus, hazard_cat, geometry)
  }

  all_sf <- bind_rows(prep_sf(pa_sf, "PurpleAir"), prep_sf(an_sf, "AirNow"))
  if (is.null(all_sf) || nrow(all_sf) == 0) {
    all_sf <- st_sf(network = character(), hazard_cat = character(),
                    geometry = st_sfc(crs = 4326))
  }
  all_sf <- all_sf |>
    mutate(
      network = factor(network, levels = c("AirNow", "PurpleAir")),
      pt_col  = point_style(hazard_cat)$col,
      pt_shp  = point_style(hazard_cat)$shp,
      ## draw order: PurpleAir under AirNow; event-category points on top
      ord     = (hazard_cat %in% ev$haz_cats) * 2L + (network == "AirNow")
    ) |>
    arrange(ord)

  ggplot() +
    geom_sf(data = cnty_crop,
            fill = "grey95", colour = "grey78", linewidth = 0.10) +
    geom_sf(data      = focus_sf,
            fill      = adjustcolor(ev$color, alpha.f = 0.14),
            colour    = ev$color, linewidth = 0.65) +
    geom_sf(data = state_crop,
            fill = NA, colour = "grey42", linewidth = 0.30) +
    geom_sf(data   = all_sf,
            aes(size = network, colour = I(pt_col), shape = I(pt_shp)),
            stroke = 0.8, alpha = 0.9) +
    scale_size_manual(values = PT_SIZE, guide = "none") +
    coord_sf(xlim = ev$zoom[1:2], ylim = ev$zoom[3:4], expand = FALSE) +
    labs(title = ev$label, subtitle = subtitle) +
    event_panel_theme()
}

## ── 12. Save individual event maps ────────────────────────────────────────────
message("\nSaving individual event maps...")
all_panels <- list()
for (nm in names(events)) {
  ev  <- events[[nm]]
  message("  Building: ", nm)
  p   <- build_event_map(ev)
  all_panels[[nm]] <- p
  out <- file.path(out_dir, paste0("ms1_fig3_ev_", ev$short, ".svg"))
  svglite(out, width = 6.5, height = 5.5, bg = "white", user_fonts = svg_fonts)
  print(p)
  dev.off()
  message("    Saved: ", out)
}

## ── 13. US overview map ───────────────────────────────────────────────────────
message("\nBuilding US overview map...")

## Event-county highlight layer (Albers)
ev_cnty_meta <- rbindlist(lapply(names(events), function(nm) {
  ev <- events[[nm]]
  data.table(cnty_geoid = ev$cnty_geoids,
             panel_lbl  = ev$panel_lbl,
             ev_color   = unname(ev$color))
}))

ev_cnty_alb <- county_alb |>
  mutate(cnty_geoid = as.integer(GEOID)) |>
  filter(cnty_geoid %in% ev_cnty_meta$cnty_geoid) |>
  left_join(as.data.frame(ev_cnty_meta), by = "cnty_geoid")

ev_cnty_conus <- ev_cnty_alb |> filter(!STATEFP %in% c("02", "15"))
ev_cnty_hi    <- ev_cnty_alb |> filter(STATEFP == "15")

p_conus <- ggplot() +
  geom_sf(data = county_conus_alb,
          fill = "grey95", colour = "grey70", linewidth = 0.08) +
  geom_sf(data = state_conus_alb,
          fill = NA,       colour = "grey55", linewidth = 0.28) +
  geom_sf(data  = ev_cnty_conus,
          aes(fill = I(ev_color)),
          colour = "grey20", linewidth = 0.22, alpha = 0.85) +
  coord_sf(crs    = albers_crs,
           xlim   = c(conus_bbox["xmin"], conus_bbox["xmax"]),
           ylim   = c(conus_bbox["ymin"], conus_bbox["ymax"]),
           expand = FALSE) +
  base_map_theme +
  theme(legend.position = "none")

p_ak <- ggplot() +
  geom_sf(data = county_ak_alb, fill = "grey95", colour = "grey70",
          linewidth = 0.08) +
  geom_sf(data = state_ak_alb,  fill = NA,       colour = "grey55",
          linewidth = 0.26) +
  coord_sf(crs    = albers_crs,
           xlim   = c(ak_bbox["xmin"], ak_bbox["xmax"]),
           ylim   = c(ak_bbox["ymin"], ak_bbox["ymax"]),
           expand = FALSE) +
  base_map_theme +
  theme(panel.border = element_rect(colour = "grey20", fill = NA,
                                    linewidth = 0.6))

p_hi <- ggplot() +
  geom_sf(data = county_hi_alb, fill = "grey95", colour = "grey70",
          linewidth = 0.08) +
  geom_sf(data  = ev_cnty_hi,
          aes(fill = I(ev_color)),
          colour = "grey20", linewidth = 0.22, alpha = 0.85) +
  coord_sf(crs    = albers_crs,
           xlim   = c(hi_bbox["xmin"], hi_bbox["xmax"]),
           ylim   = c(hi_bbox["ymin"], hi_bbox["ymax"]),
           expand = FALSE) +
  base_map_theme +
  theme(panel.border = element_rect(colour = "grey20", fill = NA,
                                    linewidth = 0.6))

ak_box <- ggdraw() +
  draw_plot(p_ak, 0, 0, 1, 1) +
  draw_label("Alaska", x = 0.25, y = 0.93, hjust = 0, vjust = 1, size = 16, fontfamily = FONT)
hi_box <- ggdraw() +
  draw_plot(p_hi, 0, 0, 1, 1) +
  draw_label("Hawaii", x = 0.28, y = 0.96, hjust = 0, vjust = 1, size = 16, fontfamily = FONT)

## Legend 1 — hazard colours (Table 2 order)
leg_df <- data.frame(
  haz = factor(names(ov_color_map), levels = names(ov_color_map)),
  x   = seq_along(ov_color_map),
  y   = 1
)
p_leg_ov <- ggplot(leg_df, aes(x, y, color = haz)) +
  geom_point(alpha = 0, size = 0, show.legend = TRUE) +
  scale_color_manual(
    values = ov_color_map,
    breaks = names(ov_color_map),
    name   = NULL,
    guide  = guide_legend(nrow = 2L, byrow = TRUE,
                          override.aes = list(size = 4.5, alpha = 1))
  ) +
  theme_void(base_size = 16, base_family = FONT) +
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.text      = element_text(size = 16, margin = margin(l = 6, r = 10)),
    legend.key.size  = unit(0.5, "cm"),
    legend.spacing.x = unit(0.15, "cm"),
    legend.margin    = margin(0, 0, 0, 0),
    plot.margin      = margin(0, 0, 0, 0)
  )

## Legend 2 — non-hazard classes and network size on event panels
## (hazard points use the colours of legend 1)
st_lab <- c("Baseline", "Unclassified (NA)", "AirNow", "PurpleAir")
st_df <- data.frame(
  lab = factor(st_lab, levels = st_lab),
  x = seq_along(st_lab), y = 1
)
p_leg_st <- ggplot(st_df, aes(x, y, shape = lab)) +
  geom_point(alpha = 0, size = 0, show.legend = TRUE) +
  scale_shape_manual(
    values = setNames(c(16, 1, 16, 16), st_lab),
    name   = NULL,
    guide  = guide_legend(
      nrow = 1L, byrow = TRUE,
      override.aes = list(
        colour = c(COL_BASELINE, COL_NA, "grey20", "grey20"),
        size   = c(4, 4, PT_SIZE[["AirNow"]], PT_SIZE[["PurpleAir"]]),
        stroke = 0.8, alpha = 1))
  ) +
  theme_void(base_size = 16, base_family = FONT) +
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.text      = element_text(size = 16, margin = margin(l = 6, r = 10)),
    legend.key.size  = unit(0.5, "cm"),
    legend.spacing.x = unit(0.15, "cm"),
    legend.margin    = margin(0, 0, 0, 0),
    plot.margin      = margin(0, 0, 0, 0)
  )

overview <- ggdraw() +
  draw_plot(p_conus,   x = 0.15, y = 0.18, width = 0.85, height = 0.82) +
  draw_plot(hi_box,    x = 0.04, y = 0.23, width = 0.20, height = 0.21) +
  draw_plot(ak_box,    x = 0.24, y = 0.21, width = 0.18, height = 0.23) +
  draw_plot(p_leg_ov,  x = 0.05, y = 0.09, width = 0.95, height = 0.10) +
  draw_plot(p_leg_st,  x = 0.05, y = 0.00, width = 0.95, height = 0.09)

svg_ov <- file.path(out_dir, "ms1_fig3_us_overview.svg")
svglite(svg_ov, width = 10, height = 7.5, bg = "white", user_fonts = svg_fonts)
print(overview)
dev.off()
message("  Saved: ", svg_ov)

## ── 14. Composite: overview + eight panels + leader lines ────────────────────
## The CONUS map, insets and legends are placed directly in the composite (the
## standalone overview SVG keeps its own layout). Leader lines run from each
## panel edge to the event-county centroid on the CONUS map; the map box has
## the map's aspect ratio, so the mapping is linear.

COMP_W <- 20   # composite size, inches
COMP_H <- 13

## Panel positions (x, y, w, h, NPC): 3 left | 2 bottom-centre | 3 right
LAYOUT <- list(
  A_Heat          = c(0.000, 0.680, 0.230, 0.300),
  B_Wildfire      = c(0.000, 0.350, 0.230, 0.300),
  C_Volcano       = c(0.000, 0.020, 0.230, 0.300),
  D_Flood         = c(0.260, 0.000, 0.230, 0.300),
  E_Dust          = c(0.510, 0.000, 0.230, 0.300),
  F_Tornado       = c(0.770, 0.680, 0.230, 0.300),
  G_Hurricane     = c(0.770, 0.350, 0.230, 0.300),
  H_TropicalStorm = c(0.770, 0.020, 0.230, 0.300)
)

## Central block, in composite inches (y from the bottom). Equal gaps GAP_IN
## between: D/E panel tops -> legend 2 -> legend 1 -> lowest inset -> map.
## Alaska and Hawaii insets are not to scale (caption).
GAP_IN    <- 0.40
LEG2_H_IN <- 0.30                      # one legend row
LEG1_H_IN <- 0.62                      # two legend rows
MAP_W_IN  <- 9.3                       # CONUS map width (upper limit)
dx_conus  <- conus_bbox[["xmax"]] - conus_bbox[["xmin"]]
dy_conus  <- conus_bbox[["ymax"]] - conus_bbox[["ymin"]]

de_top_in <- max(LAYOUT$D_Flood[2] + LAYOUT$D_Flood[4],
                 LAYOUT$E_Dust[2]  + LAYOUT$E_Dust[4]) * COMP_H
leg2_y_in <- de_top_in + GAP_IN
leg1_y_in <- leg2_y_in + LEG2_H_IN + GAP_IN
low_in    <- leg1_y_in + LEG1_H_IN + GAP_IN      # bottom of map and insets

## Insets, sized to their own map extents and bottom-aligned with the CONUS
## map: Hawaii just left of the map's west edge, Alaska in the Pacific corner
## (top kept below the CA/AZ border, about 0.25 of the map height).
## The map is narrowed if it would reach the right-hand panel column.
bb_asp     <- function(bb) (bb[["ymax"]] - bb[["ymin"]]) / (bb[["xmax"]] - bb[["xmin"]])
left_edge  <- (LAYOUT$A_Heat[1] + LAYOUT$A_Heat[3]) * COMP_W + 0.15
right_edge <- LAYOUT$F_Tornado[1] * COMP_W - 0.15
for (pass in 1:2) {
  MAP_H_IN <- MAP_W_IN * dy_conus / dx_conus
  HI_H <- 0.219 * MAP_W_IN;  HI_W <- HI_H / bb_asp(hi_bbox)
  if (HI_W > 0.14 * MAP_W_IN) { HI_W <- 0.14 * MAP_W_IN; HI_H <- HI_W * bb_asp(hi_bbox) }
  AK_W <- 0.212 * MAP_W_IN;  AK_H <- AK_W * bb_asp(ak_bbox)
  if (AK_H > 0.25 * MAP_H_IN) { AK_H <- 0.25 * MAP_H_IN; AK_W <- AK_H / bb_asp(ak_bbox) }
  map_x_in <- max(COMP_W / 2 - MAP_W_IN / 2, left_edge + HI_W)
  if (map_x_in + MAP_W_IN > right_edge) MAP_W_IN <- right_edge - map_x_in
}
MAP_H_IN <- MAP_W_IN * dy_conus / dx_conus
map_y_in <- low_in                                    # map and insets share the bottom
if (map_y_in + MAP_H_IN > COMP_H)
  warning("CONUS map extends above the composite; reduce MAP_W_IN or GAP_IN")

hi_x_in <- map_x_in - HI_W
ak_x_in <- map_x_in + 0.01 * MAP_W_IN
hi_y_in <- map_y_in
ak_y_in <- map_y_in

alb_to_npc <- function(cx, cy) {
  list(x = (map_x_in + (cx - conus_bbox[["xmin"]]) / dx_conus * MAP_W_IN) / COMP_W,
       y = (map_y_in + (cy - conus_bbox[["ymin"]]) / dy_conus * MAP_H_IN) / COMP_H)
}

npc <- function(x_in, y_in, w_in, h_in)
  list(x = x_in / COMP_W, y = y_in / COMP_H, width = w_in / COMP_W, height = h_in / COMP_H)

leg_cx_in <- map_x_in + MAP_W_IN / 2          # legends centred under the map
leg_w_in  <- 11
b_map <- npc(map_x_in, map_y_in, MAP_W_IN, MAP_H_IN)
b_hi  <- npc(hi_x_in, hi_y_in, HI_W, HI_H)
b_ak  <- npc(ak_x_in, ak_y_in, AK_W, AK_H)
b_l1  <- npc(leg_cx_in - leg_w_in / 2, leg1_y_in, leg_w_in, LEG1_H_IN)
b_l2  <- npc(leg_cx_in - leg_w_in / 2, leg2_y_in, leg_w_in, LEG2_H_IN)

comp <- ggdraw() +
  draw_plot(p_conus, b_map$x, b_map$y, b_map$width, b_map$height) +
  draw_plot(hi_box,  b_hi$x,  b_hi$y,  b_hi$width,  b_hi$height) +
  draw_plot(ak_box,  b_ak$x,  b_ak$y,  b_ak$width,  b_ak$height)
## legends are added after the leader lines (white background, lines pass behind)

## Event-county centroids (one per event: mean of its county centroids)
ev_cents    <- suppressWarnings(st_centroid(st_geometry(ev_cnty_conus)))
ev_cents_df <- data.frame(panel_lbl = ev_cnty_conus$panel_lbl,
                          CX = st_coordinates(ev_cents)[, 1],
                          CY = st_coordinates(ev_cents)[, 2])
ev_cents_df <- aggregate(cbind(CX, CY) ~ panel_lbl, data = ev_cents_df, FUN = mean)

## Leader lines (CONUS events only; Volcano sits in the Hawaii inset).
## Each line is cut where it crosses the Hawaii and Alaska inset boxes, so the
## insets keep their transparent background without lines drawn over them.
cut_interval <- function(iv, t0, t1) {
  if (iv[2] <= t0 || iv[1] >= t1) return(list(iv))
  out <- list()
  if (iv[1] < t0) out <- c(out, list(c(iv[1], t0)))
  if (iv[2] > t1) out <- c(out, list(c(t1, iv[2])))
  out
}
seg_outside <- function(x, y, boxes) {
  keep <- list(c(0, 1))
  dx <- x[2] - x[1]
  dy <- y[2] - y[1]
  for (b in boxes) {
    p  <- c(-dx, dx, -dy, dy)
    q  <- c(x[1] - b$x, b$x + b$width - x[1], y[1] - b$y, b$y + b$height - y[1])
    t0 <- 0
    t1 <- 1
    hit <- TRUE
    for (k in 1:4) {
      if (p[k] == 0) {
        if (q[k] < 0) { hit <- FALSE; break }
      } else if (p[k] < 0) {
        t0 <- max(t0, q[k] / p[k])
      } else {
        t1 <- min(t1, q[k] / p[k])
      }
    }
    if (!hit || t0 >= t1) next
    keep <- unlist(lapply(keep, cut_interval, t0 = t0, t1 = t1), recursive = FALSE)
  }
  lapply(keep, function(iv) list(x = x[1] + iv * dx, y = y[1] + iv * dy))
}

for (nm in names(events)) {
  ev     <- events[[nm]]
  pp     <- LAYOUT[[nm]]
  cent_r <- ev_cents_df[ev_cents_df$panel_lbl == ev$panel_lbl, ]
  if (nrow(cent_r) == 0) next
  us_pt   <- alb_to_npc(cent_r$CX[1], cent_r$CY[1])
  is_left <- pp[1] < 0.5 && pp[2] > 0.01
  pan_x   <- if (pp[2] <= 0.01) pp[1] + pp[3] / 2 else if (is_left) pp[1] + pp[3] else pp[1]
  pan_y   <- if (pp[2] <= 0.01) pp[2] + pp[4] else pp[2] + pp[4] / 2
  for (s in seg_outside(c(pan_x, us_pt$x), c(pan_y, us_pt$y), list(b_hi, b_ak))) {
    comp <- comp + draw_line(
      x         = s$x,
      y         = s$y,
      color     = ev$color,
      linewidth = 0.45,
      linetype  = "dashed"
    )
  }
}

leg_bg <- theme(legend.background = element_rect(fill = "white", colour = NA))
comp <- comp +
  draw_grob(get_legend(p_leg_ov + leg_bg), b_l1$x, b_l1$y, b_l1$width, b_l1$height) +
  draw_grob(get_legend(p_leg_st + leg_bg), b_l2$x, b_l2$y, b_l2$width, b_l2$height)

for (nm in names(events)) {
  pp   <- LAYOUT[[nm]]
  comp <- comp + draw_plot(all_panels[[nm]],
                           x = pp[1], y = pp[2],
                           width = pp[3], height = pp[4])
}

svglite(file.path(out_dir, "ms1_fig3_composite.svg"),
        width = COMP_W, height = COMP_H, bg = "white", user_fonts = svg_fonts)
print(comp)
dev.off()
ggsave(file.path(out_dir, "ms1_fig3_composite.png"), plot = comp,
       width = COMP_W, height = COMP_H, dpi = 300, bg = "white", device = ragg::agg_png)
message("Composite saved (SVG and PNG).")
