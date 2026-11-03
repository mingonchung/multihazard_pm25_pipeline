## =============================================================================
## ms1_fig2_sensor_hazard_ranking.R
## MS1 Figure 2 — Primary / secondary / tertiary hazard at each sensor
## Two separate figures: AirNow (2014–2025) | PurpleAir outdoor (2020–2025)
##
## Rank = hazard category with the most sensor-days at the sensor (Wildfire and
## Heat variants collapsed). Baseline and Unclassified (NA) are not ranked.
## Minimum sensor-days to show a rank: MIN_DAYS_RANK1/2/3.
## Legend follows the Table 2 category order.
## Panels A-C are drawn by one function in cells of equal size; the legend has
## its own strip under panel C.
##
## Input:   FILE_DAILY_INTEGRATED, FILE_PA_DAILY_INTEGRATED_OUTDOOR,
##          FILE_AIRNOW_USATTRS_SHP, FILE_PA_OUTDOOR_USATTRS_SHP
##          county and state boundaries: tigris (Census cartographic files, 2020)
## Output:  DIR_MS1_OUT/ms1_fig2a_hazard_ranking_airnow.{svg,png}
##          DIR_MS1_OUT/ms1_fig2b_hazard_ranking_purpleair_outdoor.{svg,png}
##          DIR_MS1_OUT/ms1_fig2_rank_{airnow,purpleair_outdoor}.csv
## =============================================================================

rm(list = ls())

library(data.table)
library(lubridate)
library(sf)
library(dplyr)
library(ggplot2)
library(cowplot)
library(svglite)
library(tigris)

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

## ---------------------------------------------------------------------------
## 1. Paths
## ---------------------------------------------------------------------------
out_dir <- file.path(ROOT_DIR, "ms1")
## Optional local (laptop) paths: if ms1_local_paths.R sits next to this
## script, it overrides the input paths and the output folder set above.
if (file.exists("ms1_local_paths.R")) source("ms1_local_paths.R")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## ---------------------------------------------------------------------------
## 2. Parameters
## ---------------------------------------------------------------------------
STUDY_YEARS    <- 2014:2025
MIN_DAYS_RANK1 <- 1
MIN_DAYS_RANK2 <- 5
MIN_DAYS_RANK3 <- 5

COLLAPSE_WILDFIRE <- TRUE
COLLAPSE_HEAT     <- TRUE

LEGEND_REL_H   <- 0.22   # height of the legend strip relative to one panel

PT_SIZE_AIRNOW <- 0.9    # point size, AirNow figure
PT_SIZE_PA     <- 0.45   # point size, PurpleAir figure (denser network)

## ---------------------------------------------------------------------------
## 3. CRS + base map theme
## ---------------------------------------------------------------------------
albers_crs <- st_crs("ESRI:102003")
excl_terr  <- c("60", "66", "69", "72", "78")

base_map_theme <- list(
  theme_void(base_size = 24, base_family = FONT),
  theme(
    text             = element_text(size = 24),
    legend.text      = element_text(size = 24),
    legend.title     = element_text(size = 24),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border     = element_blank(),
    plot.background  = element_rect(fill = "transparent", color = NA),
    plot.margin      = margin(0, 0, 0, 0, unit = "cm")
  )
)

## ---------------------------------------------------------------------------
## 4. County and state basemaps (state outlines drawn over county lines)
## ---------------------------------------------------------------------------
county_raw   <- tigris::counties(cb = TRUE, resolution = "5m",
                                 year = 2020, progress_bar = FALSE)
county       <- st_transform(county_raw, albers_crs)
county$STATEFP <- as.character(county$STATEFP)
county_slim  <- st_simplify(county, dTolerance = 2000, preserveTopology = TRUE)

county_conus <- county_slim |> filter(!STATEFP %in% c("02", "15", excl_terr))
county_ak    <- county_slim |> filter(STATEFP == "02")
county_hi    <- county_slim |> filter(STATEFP == "15")

state_raw    <- tigris::states(cb = TRUE, resolution = "20m",
                               year = 2020, progress_bar = FALSE)
state_slim   <- state_raw |>
  st_transform(albers_crs) |>
  mutate(STATEFP = as.character(STATEFP)) |>
  st_simplify(dTolerance = 3000, preserveTopology = TRUE)

state_conus  <- state_slim |> filter(!STATEFP %in% c("02", "15", excl_terr))
state_ak     <- state_slim |> filter(STATEFP == "02")
state_hi     <- state_slim |> filter(STATEFP == "15")

conus_bbox   <- st_bbox(county_conus)
ak_bbox      <- st_bbox(county_ak)

## Line styles: thin light county lines, darker state outlines on top
county_line <- list(fill = "grey95", color = "grey75", linewidth = 0.06)
state_line  <- list(fill = NA,       color = "grey30", linewidth = 0.30)

## ---------------------------------------------------------------------------
## 5. Sensor point geometries
## ---------------------------------------------------------------------------
air_pts <- st_read(FILE_AIRNOW_USATTRS_SHP, quiet = TRUE) |>
  st_transform(albers_crs) |>
  mutate(AQSID_join = sub("^'+", "", as.character(AQSID))) |>
  select(AQSID_join, statefp)

pa_pts <- st_read(FILE_PA_OUTDOOR_USATTRS_SHP, quiet = TRUE) |>
  st_transform(albers_crs) |>
  mutate(STATION = as.character(sub("^'+", "", as.character(STATION)))) |>
  select(STATION, statefp)

## ---------------------------------------------------------------------------
## 6. Read MS1 classified daily tables
## ---------------------------------------------------------------------------
read_classified <- function(filepath, id_cols) {
  DT <- fread(filepath, na.strings = c("", "NA"),
              colClasses = list(character = id_cols))
  for (col in id_cols) {
    if (col %in% names(DT))
      DT[, (col) := sub("^'+", "", trimws(get(col)))]
  }
  DT[, date := as.Date(date)]
  DT[year(date) %in% STUDY_YEARS]
}

airnow    <- read_classified(FILE_DAILY_INTEGRATED, c("AQSID", "AQSID_join"))
purpleair <- read_classified(FILE_PA_DAILY_INTEGRATED_OUTDOOR, "STATION")

## ---------------------------------------------------------------------------
## 7. Recode hazard categories
##    "Baseline" and NA are excluded before ranking.
## ---------------------------------------------------------------------------
wildfire_cats <- c("Wildfire", "Wildfire+Drought", "Wildfire+Heat",
                   "Wildfire+Heat+Drought")
heat_cats     <- c("Heat", "Heat+Drought")

## Categories to EXCLUDE from ranking entirely
exclude_from_rank <- c("Baseline", "Unclassified (NA)", NA_character_)

recode_hazard <- function(x) {
  x <- as.character(x)
  x[x == "" | x == "NA"] <- NA_character_
  if (COLLAPSE_WILDFIRE) x[x %in% wildfire_cats] <- "Wildfire (any)"
  if (COLLAPSE_HEAT)     x[x %in% heat_cats]     <- "Heat (any)"
  x[x == "TropicalStorm"]                        <- "Tropical Storm"
  x
}

airnow[,    hazard_recode := recode_hazard(hazard_cat)]
purpleair[, hazard_recode := recode_hazard(hazard_cat)]

## ---------------------------------------------------------------------------
## 8. Count sensor-days, assign ranks, pivot wide
## ---------------------------------------------------------------------------
count_station_days <- function(DT, id_col) {
  DT[
    !is.na(get(id_col)) &
      !is.na(hazard_recode) &
      !hazard_recode %in% exclude_from_rank,
    .(station_days = uniqueN(date)),
    by = c(id_col, "hazard_recode")
  ]
}

pivot_top3 <- function(DT, id_col) {
  DT2 <- copy(DT)
  # deterministic: highest station_days first, ties broken alphabetically
  setorderv(DT2, c(id_col, "station_days", "hazard_recode"),
            order = c(1L, -1L, 1L), na.last = TRUE)
  top3 <- DT2[, head(.SD, 3), by = id_col]
  top3[, rank := seq_len(.N), by = id_col]
  wide <- dcast(top3,
                as.formula(paste(id_col, "~ paste0('rank', rank)")),
                value.var = "hazard_recode")
  # also keep station_days per rank for min-day filtering
  wide_n <- dcast(top3,
                  as.formula(paste(id_col, "~ paste0('n_rank', rank)")),
                  value.var = "station_days")
  for (col in c("rank1", "rank2", "rank3")) {
    if (!col %in% names(wide))   wide[,   (col) := NA_character_]
  }
  for (col in c("n_rank1", "n_rank2", "n_rank3")) {
    if (!col %in% names(wide_n)) wide_n[, (col) := NA_integer_]
  }
  merge(wide, wide_n, by = id_col)
}

rank_air <- count_station_days(airnow,    "AQSID_join")
rank_pa  <- count_station_days(purpleair, "STATION")

wide_air <- pivot_top3(rank_air, "AQSID_join")
wide_pa  <- pivot_top3(rank_pa,  "STATION")

## ---------------------------------------------------------------------------
## 9. Join to geometries + apply minimum-day filter
## ---------------------------------------------------------------------------
apply_min_filter <- function(pts, wide_DT, id_col) {
  pts |>
    left_join(as.data.frame(wide_DT), by = id_col) |>
    filter(!is.na(rank1), !statefp %in% excl_terr) |>
    mutate(
      rank1 = if_else(!is.na(n_rank1) & n_rank1 >= MIN_DAYS_RANK1,
                      rank1, NA_character_),
      rank2 = if_else(!is.na(n_rank2) & n_rank2 >= MIN_DAYS_RANK2,
                      rank2, NA_character_),
      rank3 = if_else(!is.na(n_rank3) & n_rank3 >= MIN_DAYS_RANK3,
                      rank3, NA_character_)
    )
}

air_sf <- apply_min_filter(air_pts, wide_air, "AQSID_join")
pa_sf  <- apply_min_filter(pa_pts,  wide_pa,  "STATION")

## ---------------------------------------------------------------------------
## 10. Colour palette — in Table 2 order (legend order follows this vector)
##     Shared with Figure 3.
## ---------------------------------------------------------------------------
color_map <- c(
  "Volcano"         = "#6a3d9a",
  "Wildfire (any)"  = "#b2182b",
  "Dust"            = "#d8b365",
  "Heat (any)"      = "#f97b2b",
  "Tornado"         = "#8c510a",
  "Hurricane"       = "#74c476",
  "Tropical Storm"  = "#41ab5d",
  "Flood"           = "#1a7837"
)

## ---------------------------------------------------------------------------
## 11. Single-panel map (CONUS + Alaska and Hawaii insets)
## ---------------------------------------------------------------------------
plot_rank_map <- function(sensor_sf, rank_col, panel_label, pt_size = 0.4) {

  # Colours present in this panel
  all_haz   <- na.omit(unique(sensor_sf[[rank_col]]))
  used_cols <- color_map[names(color_map) %in% all_haz]

  # Region splits
  conus_sf <- sensor_sf |> filter(!statefp %in% c("02", "15")) |>
    mutate(.haz = .data[[rank_col]])
  ak_sf    <- sensor_sf |> filter(statefp == "02") |>
    mutate(.haz = .data[[rank_col]])
  hi_sf    <- sensor_sf |> filter(statefp == "15") |>
    mutate(.haz = .data[[rank_col]])

  region_map <- function(cnty, st, pts, size, xlim, ylim, boxed) {
    p <- ggplot() +
      geom_sf(data = cnty, fill = county_line$fill, color = county_line$color,
              linewidth = county_line$linewidth) +
      geom_sf(data = st, fill = state_line$fill, color = state_line$color,
              linewidth = state_line$linewidth) +
      geom_sf(data = pts |> filter(!is.na(.haz)),
              aes(color = .haz), size = size, alpha = 0.8) +
      scale_color_manual(values = used_cols, na.value = "transparent",
                         guide = "none") +
      coord_sf(crs = albers_crs, xlim = xlim, ylim = ylim, expand = FALSE) +
      base_map_theme
    if (boxed) p <- p + theme(panel.border = element_rect(color = "grey20", fill = NA,
                                                          linewidth = 0.6))
    p
  }

  p_conus <- region_map(county_conus, state_conus, conus_sf, pt_size,
                        c(conus_bbox["xmin"], conus_bbox["xmax"]),
                        c(conus_bbox["ymin"], conus_bbox["ymax"]), FALSE)
  p_ak    <- region_map(county_ak, state_ak, ak_sf, pt_size * 1.5,
                        c(ak_bbox["xmin"], ak_bbox["xmax"]),
                        c(ak_bbox["ymin"], ak_bbox["ymax"]), TRUE)
  p_hi    <- region_map(county_hi, state_hi, hi_sf, pt_size * 1.5,
                        c(-6300000, -5900000), c(-100000, 600000), TRUE)

  ak_box <- ggdraw() +
    draw_plot(p_ak, 0, 0, 1, 1) +
    draw_label("Alaska", x = 0.25, y = 0.93, hjust = 0, vjust = 1, size = 20, fontfamily = FONT)
  hi_box <- ggdraw() +
    draw_plot(p_hi, 0, 0, 1, 1) +
    draw_label("Hawaii", x = 0.28, y = 0.96, hjust = 0, vjust = 1, size = 20, fontfamily = FONT)

  ggdraw() +
    draw_plot(p_conus, x = 0.22, y = 0.12, width = 0.76, height = 0.86) +
    draw_plot(ak_box,  x = 0.02, y = 0.55, width = 0.18, height = 0.23) +
    draw_plot(hi_box,  x = 0.04, y = 0.25, width = 0.20, height = 0.21) +
    draw_label(panel_label, x = 0.02, y = 0.99,
               hjust = 0, vjust = 1, size = 26, fontface = "bold", fontfamily = FONT)
}

## ---------------------------------------------------------------------------
## 12. Assemble + save one figure (3 identical panels stacked; legend strip
##     under panel C)
## ---------------------------------------------------------------------------
save_figure <- function(sensor_sf, pt_size, file_stem,
                        fig_width = 10, fig_height = 18) {

  # Legend covers every hazard shown in panels A + B + C, in Table 2 order
  all_haz_fig <- na.omit(unique(c(sensor_sf$rank1,
                                  sensor_sf$rank2,
                                  sensor_sf$rank3)))
  used_all    <- color_map[names(color_map) %in% all_haz_fig]

  pa <- plot_rank_map(sensor_sf, "rank1", "a", pt_size = pt_size)
  pb <- plot_rank_map(sensor_sf, "rank2", "b", pt_size = pt_size)
  pc <- plot_rank_map(sensor_sf, "rank3", "c", pt_size = pt_size)

  leg_df <- data.frame(
    haz = factor(names(used_all), levels = names(used_all)),
    x   = seq_along(used_all),
    y   = 1
  )
  p_leg_full <- ggplot(leg_df, aes(x, y, color = haz)) +
    geom_point(alpha = 0, size = 0, show.legend = TRUE) +
    scale_color_manual(
      values = used_all,
      breaks = names(used_all),
      name   = NULL,
      guide  = guide_legend(
        nrow         = 3L,
        byrow        = TRUE,
        override.aes = list(size = 4, alpha = 1)
      )
    ) +
    theme_void(base_size = 24, base_family = FONT) +
    theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.text      = element_text(size = 20, margin = margin(l = 6, r = 12)),
      legend.key.size  = unit(0.5, "cm"),
      legend.spacing.x = unit(0.2, "cm"),
      legend.margin    = margin(4, 0, 4, 0),
      plot.margin      = margin(0, 0, 0, 0)
    )

  ## Legend strip, aligned with the CONUS map (x from 0.22). The three panels
  ## get cells of equal height, so the map, the insets, and the panel label
  ## have the same size and position in A, B, and C.
  leg_row <- ggdraw() +
    draw_plot(p_leg_full, x = 0.22, y = 0.00, width = 0.78, height = 1.00)

  fig <- plot_grid(pa, pb, pc, leg_row, ncol = 1, rel_heights = c(1, 1, 1, LEGEND_REL_H))

  svg_path <- file.path(out_dir, paste0(file_stem, ".svg"))
  png_path <- file.path(out_dir, paste0(file_stem, ".png"))

  svglite(svg_path, width = fig_width, height = fig_height, bg = "white", user_fonts = svg_fonts)
  print(fig)
  dev.off()

  ggsave(png_path, plot = fig, width = fig_width, height = fig_height,
         dpi = 300, bg = "white", device = ragg::agg_png)

  message("Saved: ", svg_path)
  message("Saved: ", png_path)
}

## ---------------------------------------------------------------------------
## 13. Produce two figures
## ---------------------------------------------------------------------------
save_figure(air_sf, PT_SIZE_AIRNOW, "ms1_fig2a_hazard_ranking_airnow")
save_figure(pa_sf,  PT_SIZE_PA,     "ms1_fig2b_hazard_ranking_purpleair_outdoor")

## ---------------------------------------------------------------------------
## 14. Summary CSVs
## ---------------------------------------------------------------------------
fwrite(wide_air, file.path(out_dir, "ms1_fig2_rank_airnow.csv"))
fwrite(wide_pa,  file.path(out_dir, "ms1_fig2_rank_purpleair_outdoor.csv"))

message("All done. Outputs: ", out_dir)
