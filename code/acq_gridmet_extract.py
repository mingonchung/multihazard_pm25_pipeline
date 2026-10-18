#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
acq_gridmet_extract.py
Extract daily GridMET variables for AirNow sensors via Google Earth Engine.

Three named run configurations:
  main   — full met suite (pr, rmax, rmin, srad, th, tmmn, tmmx, vs, vpd)
             study period 2014-2025, IDAHO_EPSCOR/GRIDMET
  hist   — temperature only (tmmn, tmmx) for p95 climatology baseline
             historical period 1980-2013, IDAHO_EPSCOR/GRIDMET
  spei   — drought indices (spei30d, spei90d)
             study period 2014-2025, GRIDMET/DROUGHT (5-day cadence; most days skipped)

Usage:
    python acq_gridmet_extract.py --run main
    python acq_gridmet_extract.py --run hist
    python acq_gridmet_extract.py --run spei

Method: point sampling via reduceRegion at each sensor location (no buffer).
GridMET native resolution is 4 km; scale set to 4638.3 m to match native pixels.
"""

import argparse
import ee
import pandas as pd
from datetime import datetime, timedelta
from config import (
    GEE_PROJECT, GEE_SENSOR_ASSET, GEE_BOUNDARY_ASSET,
    COLLECTION_GRIDMET, COLLECTION_DROUGHT, SENSOR_ID_FIELD,
    STUDY_START, STUDY_END, HIST_START, HIST_END,
    BATCH_SIZE,
    OUT_GRIDMET_MAIN, OUT_GRIDMET_HIST, OUT_GRIDMET_SPEI,
)

# ---------------------------------------------------------------------------
# Run configurations
# ---------------------------------------------------------------------------
RUN_CONFIGS = {
    "main": {
        "collection": COLLECTION_GRIDMET,
        "bands":      ["pr", "rmax", "rmin", "srad", "th", "tmmn", "tmmx", "vs", "vpd"],
        "start":      STUDY_START,
        "end":        STUDY_END,
        "output_csv": str(OUT_GRIDMET_MAIN),
        "desc":       "full met suite 2014-2025",
    },
    "hist": {
        "collection": COLLECTION_GRIDMET,
        "bands":      ["tmmn", "tmmx"],
        "start":      HIST_START,
        "end":        HIST_END,
        "output_csv": str(OUT_GRIDMET_HIST),
        "desc":       "temperature only 1980-2013 (for p95 climatology)",
    },
    "spei": {
        "collection": COLLECTION_DROUGHT,
        "bands":      ["spei30d", "spei90d"],
        "start":      STUDY_START,
        "end":        STUDY_END,
        "output_csv": str(OUT_GRIDMET_SPEI),
        "desc":       "SPEI drought indices 2014-2025 (GRIDMET/DROUGHT, 5-day cadence)",
    },
}

GRIDMET_SCALE_M = 4638.3   # native GridMET pixel size in metres

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------
parser = argparse.ArgumentParser(description="Extract GridMET variables for AirNow sensors.")
parser.add_argument(
    "--run",
    choices=list(RUN_CONFIGS.keys()),
    required=True,
    help="Run configuration: main | hist | spei",
)
args = parser.parse_args()
cfg = RUN_CONFIGS[args.run]

COLLECTION = cfg["collection"]
BANDS      = cfg["bands"]
START_DATE = cfg["start"]
END_DATE   = cfg["end"]
OUTPUT_CSV = cfg["output_csv"]

print(f"Run: {args.run} — {cfg['desc']}", flush=True)

# ---------------------------------------------------------------------------
# Skip if output already exists (re-run safe)
# ---------------------------------------------------------------------------
import os
if os.path.exists(OUTPUT_CSV):
    print(f"Output already exists, skipping extraction: {OUTPUT_CSV}", flush=True)
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
sensors_fc = ee.FeatureCollection(GEE_SENSOR_ASSET)
boundary   = ee.FeatureCollection(GEE_BOUNDARY_ASSET)
sensors_fc = sensors_fc.filterBounds(boundary.geometry())
sensor_list  = sensors_fc.toList(sensors_fc.size())
num_sensors  = sensor_list.size().getInfo()
total_batches = (num_sensors + BATCH_SIZE - 1) // BATCH_SIZE
print(f"Found {num_sensors} sensors ({total_batches} batches of up to {BATCH_SIZE}).", flush=True)

# ---------------------------------------------------------------------------
# Main extraction loop
# ---------------------------------------------------------------------------
image_collection = ee.ImageCollection(COLLECTION)
all_dfs = []

start_dt = datetime.strptime(START_DATE, "%Y-%m-%d")
end_dt   = datetime.strptime(END_DATE,   "%Y-%m-%d")
dates    = [start_dt + timedelta(days=i)
            for i in range((end_dt - start_dt).days + 1)]

for current in dates:
    date_str = current.strftime("%Y-%m-%d")
    next_str = (current + timedelta(days=1)).strftime("%Y-%m-%d")
    print(f"\nDate {date_str}", flush=True)

    daily_coll = image_collection.filterDate(date_str, next_str)
    try:
        if daily_coll.size().getInfo() == 0:
            # Normal for SPEI (5-day cadence); expected gap for other collections
            print("  – no image available, skip", flush=True)
            continue
    except Exception as e:
        print(f"  – error querying collection: {e}", flush=True)
        continue

    # SPEI collection requires explicit ee.Image() cast; harmless for others
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
                maxPixels=1e8,
            )
            return feat.set(vals)

        sampled = batch_fc.map(sample_fn)
        try:
            feats = sampled.getInfo()["features"]
        except Exception as e:
            print(f"    – batch sampling error: {e}", flush=True)
            continue

        rows = []
        for f in feats:
            prop = f["properties"]
            row  = {
                "time":          date_str,
                SENSOR_ID_FIELD: prop[SENSOR_ID_FIELD],
                "lon":           f["geometry"]["coordinates"][0],
                "lat":           f["geometry"]["coordinates"][1],
            }
            for b in BANDS:
                row[b] = prop.get(b)
            rows.append(row)

        print(f"    – found {len(rows)} obs", flush=True)
        if rows:
            all_dfs.append(pd.DataFrame(rows))

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------
if all_dfs:
    df = pd.concat(all_dfs, ignore_index=True)
    col_order = [SENSOR_ID_FIELD, "lon", "lat", "time"] + BANDS
    df = df[col_order]
    df.to_csv(OUTPUT_CSV, index=False, compression="gzip")
    print(f"\n✓ Saved {len(df)} rows to {OUTPUT_CSV}", flush=True)
else:
    print("No data extracted; no file written.", flush=True)
