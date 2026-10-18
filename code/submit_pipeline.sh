#!/bin/bash
# =============================================================================
# MS1 pipeline (AirNow) — SLURM submission script
#
# Run from the directory containing all .sub and .R files:
#   cd /home/ges/chungmin
#   bash submit_pipeline.sh 1
#   bash submit_pipeline.sh 2 <cmb_jid> <st01_jid> <aod_jid> <gmet_jid> <dmet_jid>
#
# Stage numbers follow the script file names (stage01 ... stage09).
# =============================================================================

# Always run from the directory this script lives in
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

PHASE="${1:-1}"

# ---------------------------------------------------------------------------
# Helper: submit a job and return only the numeric job ID.
# sbatch prints "Submitted batch job 482301" on success.
# On failure, print the error and exit so bad IDs never propagate.
# ---------------------------------------------------------------------------
submit() {
  local sub_file="$1"
  shift
  local result
  result=$(sbatch "$@" "${sub_file}" 2>&1)
  local jid
  jid=$(echo "$result" | grep -oE 'Submitted batch job [0-9]+' | awk '{print $NF}')
  if [[ -z "$jid" ]]; then
    echo "ERROR: sbatch failed for ${sub_file}" >&2
    echo "       sbatch output: $result" >&2
    exit 1
  fi
  echo "$jid"
}

# =============================================================================
# PHASE 1 — downloads + stage01
# =============================================================================
if [[ "$PHASE" == "1" ]]; then

  echo "=== Phase 1: submitting acquisition and stage01 jobs ==="

  JOB_AIRNOW_DL=$(submit sub_acq_airnow_download.sub)
  echo "  acq_airnow_download   submitted: $JOB_AIRNOW_DL"

  JOB_HMS=$(submit sub_acq_hms.sub)
  echo "  acq_hms               submitted: $JOB_HMS"

  JOB_STORM_DL=$(submit sub_acq_storm_download.sub)
  echo "  acq_storm_download    submitted: $JOB_STORM_DL"

  JOB_AOD=$(submit sub_acq_gee_aod.sub)
  echo "  acq_gee_aod [arr]     submitted: $JOB_AOD"

  JOB_GRIDMET=$(submit sub_acq_gee_gridmet.sub)
  echo "  acq_gee_gridmet [arr] submitted: $JOB_GRIDMET"

  JOB_DAYMET=$(submit sub_acq_gee_daymet.sub)
  echo "  acq_gee_daymet [arr]  submitted: $JOB_DAYMET"

  JOB_AIRNOW_CMB=$(submit sub_acq_airnow_combine.sub --dependency=afterok:${JOB_AIRNOW_DL})
  echo "  acq_airnow_combine    submitted: $JOB_AIRNOW_CMB  (after $JOB_AIRNOW_DL)"

  JOB_STAGE01=$(submit sub_stage01_hms_fired.sub --dependency=afterok:${JOB_HMS})
  echo "  stage01_hms_fired     submitted: $JOB_STAGE01  (after $JOB_HMS)"

  echo ""
  echo "=== Phase 1 submitted ==="
  echo ""
  echo "  Monitor:  squeue -u \$USER"
  echo "  Logs:     ls logs/"
  echo ""
  echo "  After ALL jobs show COMPLETED, do the manual metadata step:"
  echo "    Rscript acq_airnow_station_metadata.R"
  echo "    QA the output CSV (lat/lon sanity check, fix duplicates)"
  echo "    Save corrected file as FILE_AIRNOW_LOCS in config.R"
  echo "  Also place the NWS zone-county file (FILE_ZONE_COUNTY) and the"
  echo "  zone lookup file (FILE_ZONE_LEGACY) in DIR_STORM (see README)."
  echo ""
  echo "  Then copy-paste this exact Phase 2 command:"
  echo ""
  echo "    bash submit_pipeline.sh 2 ${JOB_AIRNOW_CMB} ${JOB_STAGE01} ${JOB_AOD} ${JOB_GRIDMET} ${JOB_DAYMET}"
  echo ""

# =============================================================================
# PHASE 2 — processing stages (requires FILE_AIRNOW_LOCS from manual step)
# =============================================================================
elif [[ "$PHASE" == "2" ]]; then

  if [[ $# -lt 6 ]]; then
    echo "Usage: bash submit_pipeline.sh 2 <airnow_cmb_jid> <stage01_jid> <aod_jid> <gridmet_jid> <daymet_jid>"
    echo "These 5 job IDs were printed at the end of Phase 1."
    exit 1
  fi

  JOB_AIRNOW_CMB="$2"
  JOB_STAGE01="$3"
  JOB_AOD="$4"
  JOB_GRIDMET="$5"
  JOB_DAYMET="$6"

  echo "=== Phase 2: submitting processing stages ==="
  echo "  Phase 1 job IDs: cmb=$JOB_AIRNOW_CMB  st01=$JOB_STAGE01  aod=$JOB_AOD  gmet=$JOB_GRIDMET  dmet=$JOB_DAYMET"
  echo ""

  # -- Sensor location shapefile (FILE_AIRNOW_LOC_SHP) from the PM2.5 file and
  #    FILE_AIRNOW_LOCS. An existing shapefile is kept and compared, not
  #    overwritten (see sub_util_sensor_location_shp.sub).
  JOB_LOCSHP=$(submit sub_util_sensor_location_shp.sub \
    --dependency=afterok:${JOB_AIRNOW_CMB})
  echo "  util_sensor_loc_shp   submitted: $JOB_LOCSHP  (after cmb=$JOB_AIRNOW_CMB)"

  #    util_sensor_us_attrs: required by stage04 (county FIPS) and stage09.
  #    util_terrain_extract: required by stage09 (elevation, slope, aspect).
  JOB_UTIL=$(submit sub_util_sensor_us_attrs.sub \
    --dependency=afterok:${JOB_LOCSHP})
  echo "  util_sensor_us_attrs  submitted: $JOB_UTIL  (after loc_shp=$JOB_LOCSHP)"

  JOB_TERRAIN=$(submit sub_util_terrain_extract.sub \
    --dependency=afterok:${JOB_LOCSHP})
  echo "  util_terrain_extract  submitted: $JOB_TERRAIN  (after loc_shp=$JOB_LOCSHP; requires internet for AWS tiles)"

  # stage02: smoke/fire assignment to sensors
  JOB_STAGE02=$(submit sub_stage02_smoke_assign.sub \
    --dependency=afterok:${JOB_STAGE01})
  echo "  stage02_smoke_assign  submitted: $JOB_STAGE02  (after stage01=$JOB_STAGE01)"

  # stage03: p95 temperature climatology
  JOB_STAGE03=$(submit sub_stage03_p95.sub \
    --dependency=afterok:${JOB_GRIDMET}:${JOB_DAYMET})
  echo "  stage03_p95           submitted: $JOB_STAGE03  (after gridmet=$JOB_GRIDMET, daymet=$JOB_DAYMET)"

  # stage04: Storm Events join — needs county FIPS from util_sensor_us_attrs
  JOB_STAGE04=$(submit sub_stage04_storm_join.sub \
    --dependency=afterok:${JOB_UTIL})
  echo "  stage04_storm_join    submitted: $JOB_STAGE04  (after util=$JOB_UTIL)"

  # stage05: volcanic eruption buffer join — uses FILE_AIRNOW_LOCS (CSV only)
  JOB_STAGE05=$(submit sub_stage05_gvp.sub)
  echo "  stage05_gvp           submitted: $JOB_STAGE05"

  # stage06: master hourly integration — merges all upstream outputs
  JOB_STAGE06=$(submit sub_stage06_integration.sub \
    --dependency=afterok:${JOB_AIRNOW_CMB}:${JOB_STAGE02}:${JOB_STAGE03}:${JOB_STAGE04}:${JOB_STAGE05}:${JOB_AOD}:${JOB_GRIDMET}:${JOB_DAYMET})
  echo "  stage06_integration   submitted: $JOB_STAGE06  (after acquisition + stages 02-05)"

  # stage07_08: daily aggregation + hazard classification
  JOB_STAGE0708=$(submit sub_stage07_08_classify.sub \
    --dependency=afterok:${JOB_STAGE06})
  echo "  stage07_08_classify   submitted: $JOB_STAGE0708  (after stage06=$JOB_STAGE06)"

  # stage09: attribute join — needs US attrs + terrain shapefiles
  JOB_STAGE09=$(submit sub_stage09_attr_join.sub \
    --dependency=afterok:${JOB_STAGE0708}:${JOB_UTIL}:${JOB_TERRAIN})
  echo "  stage09_attr_join     submitted: $JOB_STAGE09  (after stage07_08=$JOB_STAGE0708, util=$JOB_UTIL, terrain=$JOB_TERRAIN)"

  echo ""
  echo "=== Phase 2 submitted. Full dependency chain is active. ==="
  echo ""
  echo "  Monitor all jobs:      squeue -u \$USER"
  echo "  Watch stage06 log:     tail -f logs/stage06_int_${JOB_STAGE06}.out"
  echo "  Watch stage07_08 log:  tail -f logs/stage07_08_${JOB_STAGE0708}.out"
  echo ""
  echo "  After the PurpleAir pipeline also finishes (stage09_pa), MS1 outputs:"
  echo "    sbatch sub_ms1_outputs.sub                          # Table 5, Figs. 2-3"
  echo "    sbatch ms1_table7_sensitivity_analyses_rev.sub      # Table 6"

else
  echo "Unknown phase: '$PHASE'. Use '1' or '2'."
  exit 1
fi
