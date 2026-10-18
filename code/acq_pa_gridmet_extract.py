#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
acq_pa_gridmet_extract.py
Extract daily GridMET variables for PurpleAir sensors via Google Earth Engine.

Consolidates six previous scripts into one:
  4_purpleair_gridmet_extract.py              → --network outdoor --run main --year YYYY
  4_purpleair_gridmet_extract_tmmx.py         → --network outdoor --run hist --year YYYY
  4_purpleair_gridmet_drought_extract.py      → --network outdoor --run spei --year YYYY
  5_purpleair_gridmet_extract_indoor.py       → --network indoor  --run main
  5_purpleair_gridmet_extract_indoor_tmmx.py  → --network indoor  --run hist
  5_purpleair_gridmet_drought_extract_indoor.py → --network indoor --run spei

Usage:
    # Outdoor (year-by-year; run once per year for each --run config):
    python acq_pa_gridmet_extract.py --network outdoor --run main --year 2020
    python acq_pa_gridmet_extract.py --network outdoor --run hist --year 1990
    python acq_pa_gridmet_extract.py --network outdoor --run spei --year 2020

    # Indoor (full range per --run config; no --year needed):
    python acq_pa_gridmet_extract.py --network indoor --run main
    python acq_pa_gridmet_extract.py --network indoor --run hist
    python acq_pa_gridmet_extract.py --network indoor --run spei

Run configurations:
  main  — full met suite (pr, rmax, rmin, srad, th, tmmn, tmmx, vs, vpd)
           outdoor: PA_STUDY_START–PA_STUDY_END (year-by-year)
           indoor:  PA_STUDY_START–PA_STUDY_END (2020-2025, single run)
  hist  — temperature only (tmmn, tmmx) for p95 climatology baseline
           outdoor: PA_HIST_START–PA_HIST_END (year-by-year)
           indoor:  PA_HIST_START–PA_HIST_END (1980-2019, single run)
  spei  — drought indices (spei30d, spei90d)
           outdoor: PA_STUDY_START–PA_STUDY_END (year-by-year, ~5-day cadence)
           indoor:  PA_STUDY_START–PA_STUDY_END (2020-2025, single run)

Re-run safe: skips silently if the output file already exists.
Memory: outdoor accumulates one year in-memory then saves; indoor accumulates
full range in-memory then saves (hist indoor uses incremental gz for 40-yr range).
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
    COLLECTION_GRIDMET, COLLECTION_DROUGHT,
    PA_STUDY_START, PA_STUDY_END,
    PA_HIST_START, PA_HIST_END,
    BATCH_SIZE,
    PA_OUT_GRIDMET_MAIN_OUTDOOR_TPL, PA_OUT_GRIDMET_HIST_OUTDOOR_TPL,
    PA_OUT_GRIDMET_SPEI_OUTDOOR_TPL,
    PA_OUT_GRIDMET_MAIN_INDOOR, PA_OUT_GRIDMET_HIST_INDOOR,
    PA_OUT_GRIDMET_SPEI_INDOOR,
)

GRIDMET_SCALE_M = 4638.3   # native GridMET pixel size in metres
MAX_PIXELS      = 1e8

# ---------------------------------------------------------------------------
# Run configurations
# ---------------------------------------------------------------------------
RUN_CONFIGS = {
    "main": {
        "collection":    COLLECTION_GRIDMET,
        "bands":         ["pr", "rmax", "rmin", "srad", "th", "tmmn", "tmmx", "vs", "vpd"],
        "outdoor_tpl":   PA_OUT_GRIDMET_MAIN_OUTDOOR_TPL,
        "indoor_path":   str(PA_OUT_GRIDMET_MAIN_INDOOR),
        "outdoor_dates": (PA_STUDY_START, PA_STUDY_END),
        "indoor_dates":  (PA_STUDY_START, PA_STUDY_END),
        "desc":          "full met suite",
    },
    "hist": {
        "collection":    COLLECTION_GRIDMET,
        "bands":         ["tmmn", "tmmx"],
        "outdoor_tpl":   PA_OUT_GRIDMET_HIST_OUTDOOR_TPL,
        "indoor_path":   str(PA_OUT_GRIDMET_HIST_INDOOR),
        "outdoor_dates": (PA_HIST_START, PA_HIST_END),
        "indoor_dates":  (PA_HIST_START, PA_HIST_END),
        "desc":          "tmmn/tmmx only (p95 climatology baseline)",
    },
    "spei": {
        "collection":    COLLECTION_DROUGHT,
        "bands":         ["spei30d", "spei90d"],
        "outdoor_tpl":   PA_OUT_GRIDMET_SPEI_OUTDOOR_TPL,
        "indoor_path":   str(PA_OUT_GRIDMET_SPEI_INDOOR),
        "outdoor_dates": (PA_STUDY_START, PA_STUDY_END),
        "indoor_dates":  (PA_STUDY_START, PA_STUDY_END),
        "desc":          "SPEI drought indices (GRIDMET/DROUGHT, ~5-day cadence)",
    },
}

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------
parser = argparse.ArgumentParser(
    description="Extract GridMET variables for PurpleAir outdoor or indoor sensors."
)
parser.add_argument(
    "--network", choices=["outdoor", "indoor"], required=True,
    help="Sensor network: outdoor | indoor",
)
parser.add_argument(
    "--run", choices=["main", "hist", "spei"], required=True,
    help="Run configuration: main | hist | spei",
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
cfg        = RUN_CONFIGS[args.run]
COLLECTION = cfg["collection"]
BANDS      = cfg["bands"]

if args.network == "outdoor":
    SENSOR_ASSET = GEE_PA_OUTDOOR_ASSET
    hist_start, hist_end = cfg["outdoor_dates"]
    START_DATE   = f"{args.year}-01-01"
    END_DATE     = f"{args.year}-12-31"
    OUTPUT_CSV   = cfg["outdoor_tpl"].format(year=args.year)
    USE_INCREMENTAL = False   # one year in-memory is manageable
else:
    SENSOR_ASSET = GEE_PA_INDOOR_ASSET
    START_DATE, END_DATE = cfg["indoor_dates"]
    OUTPUT_CSV   = cfg["indoor_path"]
    # Indoor hist spans 40 years → write incrementally to avoid RAM pressure
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
except ee.EEException as e:
    print(f"✗ GEE init failed: {e}", flush=True)
    raise SystemExit(1)

# ---------------------------------------------------------------------------
# Load assets (point geometries; no buffer for GridMET point sampling)
# ---------------------------------------------------------------------------
boundary    = ee.FeatureCollection(GEE_BOUNDARY_ASSET).geometry()
sensors_fc  = ee.FeatureCollection(SENSOR_ASSET).filterBounds(boundary)
sensor_list = sensors_fc.toList(sensors_fc.size())
num_sensors = sensor_list.size().getInfo()
total_batches = (num_sensors + BATCH_SIZE - 1) // BATCH_SIZE
print(f"Found {num_sensors} sensors ({total_batches} batches of up to {BATCH_SIZE}).", flush=True)

# ---------------------------------------------------------------------------
# Main extraction loop
# ---------------------------------------------------------------------------
image_collection = ee.ImageCollection(COLLECTION)
all_dfs          = []
wrote_header     = False

start_dt = datetime.strptime(START_DATE, "%Y-%m-%d")
end_dt   = datetime.strptime(END_DATE,   "%Y-%m-%d")
dates    = [start_dt + timedelta(days=i)
            for i in range((end_dt - start_dt).days + 1)]

OUT_COLS = [PA_SENSOR_ID_FIELD, "lon", "lat", "time"] + BANDS

for current in dates:
    date_str = current.strftime("%Y-%m-%d")
    next_str = (current + timedelta(days=1)).strftime("%Y-%m-%d")
    print(f"\nDate {date_str}", flush=True)

    daily_coll = image_collection.filterDate(date_str, next_str)
    try:
        if daily_coll.size().getInfo() == 0:
            # Normal for SPEI (~5-day cadence); expected gaps for others
            print("  – no image available, skip", flush=True)
            continue
    except Exception as e:
        print(f"  – error querying collection: {e}", flush=True)
        continue

    # Cast to ee.Image (required for SPEI/DROUGHT collection; harmless for others)
    daily_img = ee.Image(daily_coll.first()).select(BANDS)

    for i in range(0, num_sensors, BATCH_SIZE):
        batch_num = (i // BATCH_SIZE) + 1
        batch_fc  = ee.FeatureCollection(sensor_list.slice(i, i + BATCH_SIZE))
        print(f"  – batch {batch_num}/{total_batches}", flush=True)

        def sample_fn(feat):
            vals = daily_img.reduceRegion(
                reducer=ee.Reducer.mean(),
                geometry=feat.geometry(),
                scale=GRIDMET_SCALE_M,
                maxPixels=MAX_PIXELS,
            )
            return feat.set(vals)

        sampled = batch_fc.map(sample_fn)
        try:
            feats = sampled.getInfo()["features"]
        except Exception as e:
            print(f"    – batch error: {e}", flush=True)
            continue

        rows = []
        for f in feats:
            prop = f["properties"]
            row  = {
                "time":             date_str,
                PA_SENSOR_ID_FIELD: prop[PA_SENSOR_ID_FIELD],
                "lon":              f["geometry"]["coordinates"][0],
                "lat":              f["geometry"]["coordinates"][1],
            }
            for b in BANDS:
                row[b] = prop.get(b)
            rows.append(row)

        print(f"    – found {len(rows)} obs", flush=True)

        if not rows:
            continue

        if USE_INCREMENTAL:
            # Write each batch immediately to avoid accumulating 40 years in RAM
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
        print(f"\n✓ Saved {len(df)} rows to {OUTPUT_CSV}", flush=True)
    else:
        print("No data extracted; no file written.", flush=True)
else:
    if wrote_header:
        print(f"\n✓ Saved output (incremental) to {OUTPUT_CSV}", flush=True)
    else:
        print("No data extracted; no file written.", flush=True)
