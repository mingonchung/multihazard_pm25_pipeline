#!/bin/bash
# =============================================================================
# MS1 PurpleAir pipeline — SLURM submission script
#
# Run from the directory containing all .sub and .R/.py files:
#   cd /home/ges/chungmin
#   bash submit_pa_pipeline.sh 1
#   bash submit_pa_pipeline.sh 2 <cmb_out_jid> <cmb_in_jid> \
#        <aod_out_jid> <aod_in_jid> \
#        <gm_main_jid> <gm_spei_jid> <gm_hist_jid> <gm_in_jid> \
#        <dm_main_jid> <dm_hist_jid> <dm_in_jid>
#
# DATA SOURCE ASSUMPTION:
#   EPA RSIG PurpleAir data is already on disk:
#     /purpleair/raw/outdoor/US/pm25_corrected/purpleair_*_YYYY-MM-DD.gz
#     /purpleair/raw/indoor/US/pm25_corrected/purpleair_*_YYYY-MM-DD.gz
#   To download via native PA API instead, see sub_pa_dl_outdoor.sub / indoor.
#
# PHASE 1 submits:
#   - Combine (outdoor + indoor separately; creates sensor list after combining)
#   - All GEE extractions (use pre-existing GEE sensor shapefiles; independent)
#
# Manual step between phases:
#   After all Phase 1 jobs complete, run:
#     sbatch sub_pa_loc_shp.sub
#   (builds sensor location shapefiles from the sensor list created by combine)
#   Wait for that to finish, then submit Phase 2.
#
# PHASE 2 submits stages 02-09.
#
# Prerequisites:
#   - GEE authenticated: earthengine authenticate
#   - GEE sensor shapefiles already uploaded:
#       projects/YOUR_GEE_PROJECT/assets/purpleair
#       projects/YOUR_GEE_PROJECT/assets/purpleair_indoor
#   - AirNow stage01_hms_fired_join.R completed (shared HMS files on disk)
#   - Storm Events raw files, FILE_ZONE_COUNTY and FILE_ZONE_LEGACY in
#     DIR_STORM (shared with the AirNow pipeline)
#   - R package lutz installed (stage06_pa time-zone lookup)
#
# RE-RUN: stage02_pa, stage04_pa, and stage06_pa skip outputs that already
# exist. Move old outputs of a stage before re-submitting it.
# =============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

PHASE="${1:-1}"

# ---------------------------------------------------------------------------
# Helper: submit a job and return only the numeric job ID.
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
# PHASE 1 — combine (reads EPA RSIG data on disk) + GEE extractions
# =============================================================================
if [[ "$PHASE" == "1" ]]; then

  echo "=== Phase 1: submitting combine jobs and GEE extractions ==="
  echo ""

  # Hourly combine — outdoor (128G per year; creates sensor_list_outdoor.csv)
  JOB_CMB_OUT=$(submit sub_pa_combine_outdoor.sub)
  echo "  pa_cmb_outdoor  [arr 1-6]      submitted: $JOB_CMB_OUT"

  # Hourly combine — indoor (64G per year; creates sensor_list_indoor.csv)
  JOB_CMB_IN=$(submit sub_pa_combine_indoor.sub)
  echo "  pa_cmb_indoor   [arr 1-6]      submitted: $JOB_CMB_IN"

  # GEE extractions — independent of combine (use GEE shapefiles already uploaded)
  # AOD
  JOB_AOD_OUT=$(submit sub_pa_gee_aod_out.sub)
  echo "  pa_gee_aod_out  [arr 1-6]      submitted: $JOB_AOD_OUT"

  JOB_AOD_IN=$(submit sub_pa_gee_aod_in.sub)
  echo "  pa_gee_aod_in   [single]       submitted: $JOB_AOD_IN"

  # GridMET outdoor
  JOB_GM_MAIN=$(submit sub_pa_gee_gm_out_main.sub)
  echo "  pa_gee_gm_main  [arr 1-6]      submitted: $JOB_GM_MAIN"

  JOB_GM_SPEI=$(submit sub_pa_gee_gm_out_spei.sub)
  echo "  pa_gee_gm_spei  [arr 1-6]      submitted: $JOB_GM_SPEI"

  JOB_GM_HIST=$(submit sub_pa_gee_gm_out_hist.sub)
  echo "  pa_gee_gm_hist  [arr 1-40]     submitted: $JOB_GM_HIST"

  # GridMET indoor
  JOB_GM_IN=$(submit sub_pa_gee_gm_in.sub)
  echo "  pa_gee_gm_in    [single]       submitted: $JOB_GM_IN"

  # DayMet outdoor (arr 1-6: years 2020-2025; DayMet available through 2025)
  JOB_DM_MAIN=$(submit sub_pa_gee_dm_out_main.sub)
  echo "  pa_gee_dm_main  [arr 1-6]      submitted: $JOB_DM_MAIN"

  JOB_DM_HIST=$(submit sub_pa_gee_dm_out_hist.sub)
  echo "  pa_gee_dm_hist  [arr 1-40]     submitted: $JOB_DM_HIST"

  # DayMet indoor
  JOB_DM_IN=$(submit sub_pa_gee_dm_in.sub)
  echo "  pa_gee_dm_in    [single]       submitted: $JOB_DM_IN"

  echo ""
  echo "=== Phase 1 submitted ==="
  echo ""
  echo "  Monitor:  squeue -u \$USER"
  echo "  Logs:     ls /home/ges/chungmin/logs/"
  echo ""
  echo "  After ALL jobs show COMPLETED:"
  echo "  1. Verify sensor lists exist:"
  echo "       ls /data001/projects/chungmin/purpleair/raw/outdoor/sensor_list_outdoor.csv"
  echo "       ls /data001/projects/chungmin/purpleair/raw/indoor/sensor_list_indoor.csv"
  echo "  2. Submit the sensor location shp job and wait for completion:"
  echo "       sbatch sub_pa_loc_shp.sub"
  echo "       squeue -u \$USER"
  echo ""
  echo "  Then copy-paste this exact Phase 2 command:"
  echo ""
  echo "    bash submit_pa_pipeline.sh 2 \\"
  echo "      ${JOB_CMB_OUT} ${JOB_CMB_IN} \\"
  echo "      ${JOB_AOD_OUT} ${JOB_AOD_IN} \\"
  echo "      ${JOB_GM_MAIN} ${JOB_GM_SPEI} ${JOB_GM_HIST} ${JOB_GM_IN} \\"
  echo "      ${JOB_DM_MAIN} ${JOB_DM_HIST} ${JOB_DM_IN}"
  echo ""

# =============================================================================
# PHASE 2 — processing stages 02-09
# =============================================================================
elif [[ "$PHASE" == "2" ]]; then

  if [[ $# -lt 12 ]]; then
    echo "Usage: bash submit_pa_pipeline.sh 2 \\"
    echo "  <cmb_out_jid> <cmb_in_jid> \\"
    echo "  <aod_out_jid> <aod_in_jid> \\"
    echo "  <gm_main_jid> <gm_spei_jid> <gm_hist_jid> <gm_in_jid> \\"
    echo "  <dm_main_jid> <dm_hist_jid> <dm_in_jid>"
    echo "These 11 job IDs were printed at the end of Phase 1."
    exit 1
  fi

  JOB_CMB_OUT="$2";   JOB_CMB_IN="$3"
  JOB_AOD_OUT="$4";   JOB_AOD_IN="$5"
  JOB_GM_MAIN="$6";   JOB_GM_SPEI="$7"; JOB_GM_HIST="$8"; JOB_GM_IN="$9"
  JOB_DM_MAIN="${10}"; JOB_DM_HIST="${11}"; JOB_DM_IN="${12}"

  echo "=== Phase 2: submitting PA processing stages 02-09 ==="
  echo "  Phase 1 job IDs:"
  echo "    cmb_out=$JOB_CMB_OUT  cmb_in=$JOB_CMB_IN"
  echo "    aod_out=$JOB_AOD_OUT  aod_in=$JOB_AOD_IN"
  echo "    gm_main=$JOB_GM_MAIN  gm_spei=$JOB_GM_SPEI  gm_hist=$JOB_GM_HIST  gm_in=$JOB_GM_IN"
  echo "    dm_main=$JOB_DM_MAIN  dm_hist=$JOB_DM_HIST  dm_in=$JOB_DM_IN"
  echo ""

  # Stage 02: HMS shapefiles from AirNow stage01 on disk; sensor shp built
  JOB_STAGE02=$(submit sub_pa_stage02.sub)
  echo "  pa_stage02      submitted: $JOB_STAGE02"

  # Terrain extraction: base loc shp on disk (from sub_pa_loc_shp manual step).
  # Requires internet access for AWS Terrain Tile downloads.
  JOB_TERRAIN=$(submit sub_pa_terrain.sub)
  echo "  pa_terrain [arr 1-2]  submitted: $JOB_TERRAIN  (requires internet for AWS tiles)"

  # Stage 03: all GridMET + DayMet hist and main must complete first
  JOB_STAGE03=$(submit sub_pa_stage03.sub \
    --dependency=afterok:${JOB_GM_MAIN}:${JOB_GM_HIST}:${JOB_GM_IN}:${JOB_DM_MAIN}:${JOB_DM_HIST}:${JOB_DM_IN})
  echo "  pa_stage03 [arr 1-4]  submitted: $JOB_STAGE03  (after all GridMET+DayMet)"

  # Stage 04: Storm Events (county- and zone-coded paths) -> compact interval
  # table; us_attrs shp on disk (from sub_pa_loc_shp manual step)
  JOB_STAGE04=$(submit sub_pa_stage04.sub)
  echo "  pa_stage04      submitted: $JOB_STAGE04"

  # Stage 05: us_attrs shp on disk
  JOB_STAGE05=$(submit sub_pa_stage05.sub)
  echo "  pa_stage05      submitted: $JOB_STAGE05"

  # Stage 06: combined files + GEE study-period outputs + stages 02-05.
  # Each task reads the neighbouring years' combined files (local-year days),
  # so it waits for the whole combine arrays.
  JOB_STAGE06=$(submit sub_pa_stage06.sub \
    --dependency=afterok:${JOB_CMB_OUT}:${JOB_CMB_IN}:${JOB_STAGE02}:${JOB_STAGE03}:${JOB_STAGE04}:${JOB_STAGE05}:${JOB_AOD_OUT}:${JOB_AOD_IN}:${JOB_GM_MAIN}:${JOB_GM_SPEI}:${JOB_GM_IN}:${JOB_DM_MAIN}:${JOB_DM_IN})
  echo "  pa_stage06 [arr 1-12] submitted: $JOB_STAGE06  (after combine + stages 02-05 + GEE)"

  JOB_STAGE0708=$(submit sub_pa_stage07_08.sub \
    --dependency=afterok:${JOB_STAGE06})
  echo "  pa_stage07_08 [arr 1-2]  submitted: $JOB_STAGE0708  (after stage06=$JOB_STAGE06)"

  # Stage 09: attribute join — needs terrain + us_attrs shps
  JOB_STAGE09=$(submit sub_pa_stage09.sub \
    --dependency=afterok:${JOB_STAGE0708}:${JOB_TERRAIN})
  echo "  pa_stage09 [arr 1-2]  submitted: $JOB_STAGE09  (after stage07_08=$JOB_STAGE0708, terrain=$JOB_TERRAIN)"

  echo ""
  echo "=== Phase 2 submitted. Full dependency chain is active. ==="
  echo ""
  echo "  Monitor all jobs:          squeue -u \$USER"
  echo "  Logs:                      ls /home/ges/chungmin/logs/"
  echo "  Watch stage06 outdoor 2020: tail -f /home/ges/chungmin/logs/pa_stg06_${JOB_STAGE06}_1.out"
  echo "  Watch stage06 indoor  2020: tail -f /home/ges/chungmin/logs/pa_stg06_${JOB_STAGE06}_7.out"
  echo "  Watch stage07_08 outdoor:  tail -f /home/ges/chungmin/logs/pa_stg07_08_${JOB_STAGE0708}_1.out"
  echo ""
  echo "  Final outputs:"
  echo "    purpleair_outdoor_daily_classified_US.csv.gz"
  echo "    purpleair_indoor_daily_classified_US.csv.gz"

else
  echo "Unknown phase: '$PHASE'. Use '1' or '2'."
  exit 1
fi
