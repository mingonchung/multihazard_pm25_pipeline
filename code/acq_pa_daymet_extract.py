#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
acq_pa_daymet_extract.py
Extract daily DayMet V4 variables for PurpleAir sensors via Google Earth Engine.

Consolidates four previous scripts into one:
  4_purpleair_daymet_extract.py              → --network outdoor --run main --year YYYY
  4_purpleair_daymet_extract_tmax.py         → --network outdoor --run hist --year YYYY
  5_purpleair_daymet_extract_indoor.py       → --network indoor  --run main
  5_purpleair_daymet_extract_indoor_tmax.py  → --network indoor  --run hist

Usage:
    # Outdoor (year-by-year; run once per year for each --run config):
    python acq_pa_daymet_extract.py --network outdoor --run main --year 2020
    python acq_pa_daymet_extract.py --network outdoor --run hist --year 1990

    # Indoor (full range per --run config; no --year needed):
    python acq_pa_daymet_extract.py --network indoor --run main
    python acq_pa_daymet_extract.py --network indoor --run hist

Run configurations:
  main  — full variable suite (dayl, prcp, srad, tmax, tmin, vp)
           outdoor: year-by-year 2020-2025 (DayMet ends 2025)
           indoor:  PA_STUDY_START–PA_DAYMET_STUDY_END (2020-2025, single run)
  hist  — temperature only (tmax, tmin) for p95 climatology baseline
           outdoor: year-by-year 1980-2019
           indoor:  PA_HIST_START–PA_HIST_END (1980-2019, single run)

Note: DayMet currently available through end of 2025 only (PA_DAYMET_STUDY_END).
GridMET is used for hazard classification thresholds where both sources overlap.
DayMet provides AK/HI/PR coverage beyond CONUS (where GridMET is unavailable).

Re-run safe: skips silently if the output file already exists.
Memory: outdoor accumulates one year in-memory then saves; indoor hist (40 years)
uses incremental gzip writing; indoor main (5 years) accumulates in-memory.

Method: point sampling via sampleRegions (no buffer). DayMet native res = 1 km.
"""

import argparse
import os
import gzip
import ee
import pandas as pd
from datetime import datetime, timedelta
from config_pa import (
    GEE_PROJECT, GEE_BOUNDARY_ASSET,
    GEE_PA_OUTDOOR_ASSET, GEE_PA_INDOOR_ASSET,
    PA_SENSOR_ID_FIELD,
    COLLECTION_DAYMET,
    PA_STUDY_START, PA_DAYMET_STUDY_END,
    PA_HIST_START, PA_HIST_END,
    BATCH_SIZE, SCALE_M, TILE_SCALE,
    PA_OUT_DAYMET_MAIN_OUTDOOR_TPL, PA_OUT_DAYMET_HIST_OUTDOOR_TPL,
    PA_OUT_DAYMET_MAIN_INDOOR, PA_OUT_DAYMET_HIST_INDOOR,
)

# ---------------------------------------------------------------------------
# Run configurations
# ---------------------------------------------------------------------------
RUN_CONFIGS = {
    "main": {
        "bands":         ["dayl", "prcp", "srad", "tmax", "tmin", "vp"],
        "outdoor_tpl":   PA_OUT_DAYMET_MAIN_OUTDOOR_TPL,
        "indoor_path":   str(PA_OUT_DAYMET_MAIN_INDOOR),
        "outdoor_dates": (PA_STUDY_START,  PA_DAYMET_STUDY_END),
        "indoor_dates":  (PA_STUDY_START,  PA_DAYMET_STUDY_END),
        "desc":          "full variable suite (dayl, prcp, srad, tmax, tmin, vp)",
    },
    "hist": {
        "bands":         ["tmax", "tmin"],
        "outdoor_tpl":   PA_OUT_DAYMET_HIST_OUTDOOR_TPL,
        "indoor_path":   str(PA_OUT_DAYMET_HIST_INDOOR),
        "outdoor_dates": (PA_HIST_START, PA_HIST_END),
        "indoor_dates":  (PA_HIST_START, PA_HIST_END),
        "desc":          "tmax/tmin only (p95 climatology baseline, 1980-2019)",
    },
}

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------
parser = argparse.ArgumentParser(
    description="Extract DayMet V4 variables for PurpleAir outdoor or indoor sensors."
)
parser.add_argument(
    "--network", choices=["outdoor", "indoor"], required=True,
    help="Sensor network: outdoor | indoor",
)
parser.add_argument(
    "--run", choices=["main", "hist"], required=True,
    help="Run configuration: main (full suite) | hist (tmax/tmin for p95 baseline)",
)
parser.add_argument(
    "--year", type=int, default=None,
    help="Calendar year to process (required for --network outdoor).",
)
args = parser.parse_args()

if args.network == "outdoor" and args.year is None:
    parser.error("--year is required when --network outdoor")
if args.network == "indoor" and args.year is not None:
    print(f"Note: --year {args.year} is ignored for indoor (full range processed).", flush=True)

# ---------------------------------------------------------------------------
# Resolve settings from run config
# ---------------------------------------------------------------------------
cfg   = RUN_CONFIGS[args.run]
BANDS = cfg["bands"]

if args.network == "outdoor":
    SENSOR_ASSET = GEE_PA_OUTDOOR_ASSET
    # Respect DayMet end-date cap for outdoor main runs
    default_start, default_end = cfg["outdoor_dates"]
    START_DATE   = f"{args.year}-01-01"
    END_DATE     = f"{args.year}-12-31"
    # Cap outdoor main at DayMet availability
    if args.run == "main" and END_DATE > PA_DAYMET_STUDY_END:
        END_DATE = PA_DAYMET_STUDY_END
        print(f"Note: END_DATE capped at DayMet availability ({PA_DAYMET_STUDY_END}).", flush=True)
    OUTPUT_CSV      = cfg["outdoor_tpl"].format(year=args.year)
    USE_INCREMENTAL = False   # one year in-memory is fine
else:
    SENSOR_ASSET = GEE_PA_INDOOR_ASSET
    START_DATE, END_DATE = cfg["indoor_dates"]
    OUTPUT_CSV   = cfg["indoor_path"]
    # Indoor hist spans 40 years → incremental write to avoid RAM pressure
    USE_INCREMENTAL = (args.run == "hist")

print(
    f"Network: {args.network} | Run: {args.run} ({cfg['desc']}) | "
    f"Period: {START_DATE} → {END_DATE}",
    flush=True,
)

# ---------------------------------------------------------------------------
# Skip if output already exists
# ---------------------------------------------------------------------------
if os.path.exists(OUTPUT_CSV):
    print(f"Output already exists, skipping: {OUTPUT_CSV}", flush=True)
    raise SystemExit(0)

# ---------------------------------------------------------------------------
# Initialise GEE
# ---------------------------------------------------------------------------
try:
    ee.Initialize(project=GEE_PROJECT)
    print(f"✓ GEE initialised (project: {GEE_PROJECT})", flush=True)
except Exception as e:
    print(f"✗ GEE init failed: {e}", flush=True)
    raise SystemExit(1)

# ---------------------------------------------------------------------------
# Load assets — precompute lon/lat properties (point geometry, no buffer)
# ---------------------------------------------------------------------------
boundary       = ee.FeatureCollection(GEE_BOUNDARY_ASSET).geometry()
sensors_fc     = ee.FeatureCollection(SENSOR_ASSET).filterBounds(boundary)

def prep_sensor(f):
    """Store lon/lat as properties for inclusion in the sampled output."""
    coords = f.geometry().coordinates()
    return f.set({"lon": coords.get(0), "lat": coords.get(1)})

sensors_fc_pts = sensors_fc.map(prep_sensor)
sensor_list    = sensors_fc_pts.toList(sensors_fc_pts.size())
num_sensors    = sensor_list.size().getInfo()
total_batches  = (num_sensors + BATCH_SIZE - 1) // BATCH_SIZE
print(f"Found {num_sensors} sensors ({total_batches} batches of up to {BATCH_SIZE}).", flush=True)

# ---------------------------------------------------------------------------
# Main extraction loop
# ---------------------------------------------------------------------------
collection   = ee.ImageCollection(COLLECTION_DAYMET)
all_dfs      = []
wrote_header = False

start_dt = datetime.strptime(START_DATE, "%Y-%m-%d")
end_dt   = datetime.strptime(END_DATE,   "%Y-%m-%d")
dates    = [start_dt + timedelta(days=i)
            for i in range((end_dt - start_dt).days + 1)]

OUT_COLS = [PA_SENSOR_ID_FIELD, "lon", "lat", "time"] + BANDS

for current in dates:
    date_str = current.strftime("%Y-%m-%d")
    next_str = (current + timedelta(days=1)).strftime("%Y-%m-%d")
    print(f"\nDate {date_str}", flush=True)

    daily_ic = collection.filterDate(date_str, next_str)
    try:
        if daily_ic.size().getInfo() == 0:
            print("  – no DayMet image, skip", flush=True)
            continue
    except Exception as e:
        print(f"  – error counting DayMet images: {e}", flush=True)
        continue

    daily_img = daily_ic.first().select(BANDS)

    for i in range(0, num_sensors, BATCH_SIZE):
        batch_num = (i // BATCH_SIZE) + 1
        batch_fc  = ee.FeatureCollection(sensor_list.slice(i, i + BATCH_SIZE))
        print(f"  – batch {batch_num}/{total_batches}", flush=True)

        # sampleRegions: efficient point sampling; geometries=False reduces payload
        sampled = daily_img.sampleRegions(
            collection=batch_fc,
            properties=[PA_SENSOR_ID_FIELD, "lon", "lat"],
            scale=SCALE_M,
            tileScale=TILE_SCALE,
            geometries=False,
        )
        sampled = sampled.select([PA_SENSOR_ID_FIELD, "lon", "lat"] + BANDS, None, False)

        try:
            feats = sampled.getInfo()["features"]
        except Exception as e:
            print(f"    – batch error: {e}", flush=True)
            continue

        rows = []
        for f in feats:
            props = f.get("properties", {})
            # Skip rows where all band values are missing
            if not any(props.get(b) is not None for b in BANDS):
                continue
            row = {
                "time":             date_str,
                PA_SENSOR_ID_FIELD: props.get(PA_SENSOR_ID_FIELD),
                "lon":              props.get("lon"),
                "lat":              props.get("lat"),
            }
            for b in BANDS:
                row[b] = props.get(b)
            rows.append(row)

        print(f"    – found {len(rows)} obs", flush=True)

        if not rows:
            continue

        if USE_INCREMENTAL:
            df_batch = pd.DataFrame(rows)[OUT_COLS]
            with gzip.open(OUTPUT_CSV, "at") as fout:
                df_batch.to_csv(fout, index=False, header=(not wrote_header))
            wrote_header = True
        else:
            all_dfs.append(pd.DataFrame(rows))

# ---------------------------------------------------------------------------
# Save (in-memory path only; incremental path already written above)
# ---------------------------------------------------------------------------
if not USE_INCREMENTAL:
    if all_dfs:
        df = pd.concat(all_dfs, ignore_index=True)
        df = df[OUT_COLS]
        df.to_csv(OUTPUT_CSV, index=False, compression="gzip")
        print(f"\n✓ Saved {len(df)} records to {OUTPUT_CSV}", flush=True)
    else:
        print("No data extracted; no file written.", flush=True)
else:
    if wrote_header:
        print(f"\n✓ Saved output (incremental) to {OUTPUT_CSV}", flush=True)
    else:
        print("No data extracted; no file written.", flush=True)
