"""
config.py — shared configuration for all AirNow pipeline Python / GEE scripts

HOW TO USE:
    from config import *   at the top of every Python pipeline script.

SET ONCE:
    Change ROOT_DIR and GEE_PROJECT to match your environment.
    All output paths are derived from ROOT_DIR.
"""

from pathlib import Path

# =============================================================================
# 1. Environment (CHANGE THESE for your environment)
# =============================================================================

ROOT_DIR    = Path("/data001/projects/chungmin")
GEE_PROJECT = "YOUR_GEE_PROJECT"

# =============================================================================
# 2. GEE assets
# =============================================================================

GEE_SENSOR_ASSET   = f"projects/{GEE_PROJECT}/assets/airnow"
GEE_BOUNDARY_ASSET = f"projects/{GEE_PROJECT}/assets/uscan"

# =============================================================================
# 3. GEE collection IDs (Earth Engine catalog identifiers)
# =============================================================================

COLLECTION_MAIAC    = "MODIS/061/MCD19A2_GRANULES"   # MODIS MAIAC AOD
COLLECTION_GRIDMET  = "IDAHO_EPSCOR/GRIDMET"          # GridMET daily met
COLLECTION_DROUGHT  = "GRIDMET/DROUGHT"               # GridMET SPEI / drought
COLLECTION_DAYMET   = "NASA/ORNL/DAYMET_V4"           # DayMet V4 daily

# =============================================================================
# 4. GEE extraction settings
# =============================================================================

BATCH_SIZE      = 1000    # sensors per batch per day
SCALE_M         = 1000    # sampling scale in metres
TILE_SCALE      = 8       # tileScale for reduceRegions (4/8/16: higher = safer, slower)
BUFFER_M        = 5000    # sensor buffer radius for AOD extraction (metres)
BUFFER_MAXERR_M = 100     # max error for buffer simplification (metres)

SENSOR_ID_FIELD = "AQSID"

# =============================================================================
# 5. Study date ranges
# =============================================================================

STUDY_START = "2014-01-01"
STUDY_END   = "2025-12-31"

# Historical range for p95 climatology baseline extraction
HIST_START  = "1980-01-01"
HIST_END    = "2013-12-31"

# DayMet currently available through end of 2025 only
DAYMET_END  = "2025-12-31"

# =============================================================================
# 6. Derived output paths
# =============================================================================

DIR_CSV     = ROOT_DIR / "extraction" / "output" / "csv"
DIR_OUT     = ROOT_DIR / "extraction" / "output"
DIR_MERGE   = ROOT_DIR / "extraction" / "output" / "merge"
DIR_USCAN   = ROOT_DIR / "aqdata" / "uscan"

# GEE extraction outputs
OUT_AOD_047       = DIR_CSV / "airnow_aod_047.csv.gz"
OUT_AOD_055       = DIR_CSV / "airnow_aod_055.csv.gz"
OUT_GRIDMET_MAIN  = DIR_CSV / "airnow_gridmet_2014_2025.csv.gz"
OUT_GRIDMET_HIST  = DIR_CSV / "airnow_gridmet_temp_1980_2013.csv.gz"
OUT_GRIDMET_SPEI  = DIR_CSV / "airnow_gridmet_spei_2014_2025.csv.gz"
OUT_DAYMET_MAIN   = DIR_CSV / "airnow_daymet_2014_2025.csv.gz"
OUT_DAYMET_HIST   = DIR_CSV / "airnow_daymet_temp_1980_2013.csv.gz"
