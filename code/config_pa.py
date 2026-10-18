"""
config_pa.py — PurpleAir pipeline configuration

Inherits all shared settings from config.py (ROOT_DIR, GEE_PROJECT, collection IDs,
batch/scale settings, AirNow paths, etc.) and adds PurpleAir-specific assets,
date ranges, and output path templates.

HOW TO USE:
    from config_pa import *   at the top of every PurpleAir pipeline script.

SENSOR NETWORKS:
    outdoor  — PurpleAir outdoor sensors (purpleair GEE asset)
                 processed year-by-year; output filenames include year suffix
    indoor   — PurpleAir indoor sensors (purpleair_indoor GEE asset)
                 processed over full date ranges; output filenames are fixed

NOTE: HMS plume polygons, NOAA Storm Events, and GVP volcanic records are
shared with the AirNow pipeline and do not need to be re-downloaded for PurpleAir.
The R-side join scripts will reference the same source files.
"""

from config import *   # inherit ROOT_DIR, GEE_PROJECT, collections, batch settings, DIR_CSV, etc.

# =============================================================================
# 1. PurpleAir GEE sensor assets
# =============================================================================

GEE_PA_OUTDOOR_ASSET = f"projects/{GEE_PROJECT}/assets/purpleair"
GEE_PA_INDOOR_ASSET  = f"projects/{GEE_PROJECT}/assets/purpleair_indoor"

# PurpleAir sensor identifier field (differs from AirNow's AQSID)
PA_SENSOR_ID_FIELD = "STATION"

# =============================================================================
# 2. PurpleAir date ranges
# =============================================================================

# Study period: PurpleAir data available from 2020
PA_STUDY_START = "2020-01-01"
PA_STUDY_END   = "2025-12-31"

# Historical baseline for p95 tmmx climatology (1991-2020 normals).
# Indoor hist covers 1980-2019; the 2020 year comes from the study period.
PA_HIST_START  = "1980-01-01"
PA_HIST_END    = "2019-12-31"

# DayMet available through end of 2025 only (same constraint as AirNow)
PA_DAYMET_STUDY_END = "2025-12-31"

# =============================================================================
# 3. Output path templates and fixed paths
# =============================================================================
# Outdoor: year-by-year runs → filename includes {year}; format in script as
#     str(PA_OUT_AOD_047_OUTDOOR_TPL).format(year=YEAR)
# Indoor: single full-range run → fixed filename

# --- AOD ---
PA_OUT_AOD_047_OUTDOOR_TPL = str(DIR_CSV / "purpleair_aod_047_{year}.csv.gz")
PA_OUT_AOD_055_OUTDOOR_TPL = str(DIR_CSV / "purpleair_aod_055_{year}.csv.gz")
PA_OUT_AOD_047_INDOOR      = DIR_CSV / "purpleair_aod_047_indoor.csv.gz"
PA_OUT_AOD_055_INDOOR      = DIR_CSV / "purpleair_aod_055_indoor.csv.gz"

# --- GridMET ---
PA_OUT_GRIDMET_MAIN_OUTDOOR_TPL = str(DIR_CSV / "purpleair_gridmet_{year}.csv.gz")
PA_OUT_GRIDMET_HIST_OUTDOOR_TPL = str(DIR_CSV / "purpleair_gridmet_temp_{year}.csv.gz")
PA_OUT_GRIDMET_SPEI_OUTDOOR_TPL = str(DIR_CSV / "purpleair_gridmet_spei_{year}.csv.gz")
PA_OUT_GRIDMET_MAIN_INDOOR      = DIR_CSV / "purpleair_gridmet_2020_2025_indoor.csv.gz"
PA_OUT_GRIDMET_HIST_INDOOR      = DIR_CSV / "purpleair_gridmet_temp_1980_2019_indoor.csv.gz"
PA_OUT_GRIDMET_SPEI_INDOOR      = DIR_CSV / "purpleair_gridmet_spei_2020_2025_indoor.csv.gz"

# --- DayMet ---
PA_OUT_DAYMET_MAIN_OUTDOOR_TPL = str(DIR_CSV / "purpleair_daymet_{year}.csv.gz")
PA_OUT_DAYMET_HIST_OUTDOOR_TPL = str(DIR_CSV / "purpleair_daymet_temp_{year}.csv.gz")
PA_OUT_DAYMET_MAIN_INDOOR      = DIR_CSV / "purpleair_daymet_2020_2025_indoor.csv.gz"
PA_OUT_DAYMET_HIST_INDOOR      = DIR_CSV / "purpleair_daymet_temp_1980_2019_indoor.csv.gz"
