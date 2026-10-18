#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
acq_daymet_extract.py
Extract daily DayMet V4 variables for AirNow sensors via Google Earth Engine.

Two named run configurations:
  main   — full variable suite (dayl, prcp, srad, tmax, tmin, vp)
             study period 2014-2025, NASA/ORNL/DAYMET_V4
             Note: DayMet currently available through end of 2025 only.
  hist   — temperature only (tmax, tmin) for p95 climatology baseline
             historical period 1980-2013, NASA/ORNL/DAYMET_V4

Usage:
    python acq_daymet_extract.py --run main
    python acq_daymet_extract.py --run hist

Method: point sampling via sampleRegions (no buffer; geometries not returned
to reduce payload size). DayMet native resolution is 1 km; SCALE_M set to 1000 m.

DayMet extends coverage to Alaska, Hawaii, and Puerto Rico where GridMET
(CONUS-only) is unavailable.
"""

import argparse
import ee
import pandas as pd
from datetime import datetime, timedelta
from config import (
    GEE_PROJECT, GEE_SENSOR_ASSET, GEE_BOUNDARY_ASSET,
    COLLECTION_DAYMET, SENSOR_ID_FIELD,
    STUDY_START, DAYMET_END, HIST_START, HIST_END,
    BATCH_SIZE, SCALE_M, TILE_SCALE,
    OUT_DAYMET_MAIN, OUT_DAYMET_HIST,
)

# ---------------------------------------------------------------------------
# Run configurations
# ---------------------------------------------------------------------------
RUN_CONFIGS = {
    "main": {
        "bands":      ["dayl", "prcp", "srad", "tmax", "tmin", "vp"],
        "start":      STUDY_START,
        "end":        DAYMET_END,
        "output_csv": str(OUT_DAYMET_MAIN),
        "desc":       "full variable suite 2014-2025",
    },
    "hist": {
        "bands":      ["tmax", "tmin"],
        "start":      HIST_START,
        "end":        HIST_END,
        "output_csv": str(OUT_DAYMET_HIST),
        "desc":       "temperature only 1980-2013 (for p95 climatology)",
    },
}

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------
parser = argparse.ArgumentParser(description="Extract DayMet V4 variables for AirNow sensors.")
parser.add_argument(
    "--run",
    choices=list(RUN_CONFIGS.keys()),
    required=True,
    help="Run configuration: main | hist",
)
args = parser.parse_args()
cfg = RUN_CONFIGS[args.run]

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
except Exception as e:
    print(f"✗ GEE init failed: {e}", flush=True)
    raise SystemExit(1)

# ---------------------------------------------------------------------------
# Load assets — precompute lon/lat properties once (point geometry, no buffer)
# ---------------------------------------------------------------------------
boundary   = ee.FeatureCollection(GEE_BOUNDARY_ASSET).geometry()
sensors_fc = ee.FeatureCollection(GEE_SENSOR_ASSET).filterBounds(boundary)

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
collection = ee.ImageCollection(COLLECTION_DAYMET)
all_dfs    = []

start_dt = datetime.strptime(START_DATE, "%Y-%m-%d")
end_dt   = datetime.strptime(END_DATE,   "%Y-%m-%d")
dates    = [start_dt + timedelta(days=i)
            for i in range((end_dt - start_dt).days + 1)]

for current in dates:
    date_str = current.strftime("%Y-%m-%d")
    next_str = (current + timedelta(days=1)).strftime("%Y-%m-%d")
    print(f"\nDate {date_str}", flush=True)

    daily_ic = collection.filterDate(date_str, next_str)
    try:
        if daily_ic.size().getInfo() == 0:
            print(f"  – no DayMet image, skip", flush=True)
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
            properties=[SENSOR_ID_FIELD, "lon", "lat"],
            scale=SCALE_M,
            tileScale=TILE_SCALE,
            geometries=False,
        )
        sampled = sampled.select([SENSOR_ID_FIELD, "lon", "lat"] + BANDS, None, False)

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
                "time":          date_str,
                SENSOR_ID_FIELD: props.get(SENSOR_ID_FIELD),
                "lon":           props.get("lon"),
                "lat":           props.get("lat"),
            }
            for b in BANDS:
                row[b] = props.get(b)
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
    print(f"\n✓ Saved {len(df)} records to {OUTPUT_CSV}", flush=True)
else:
    print("No data extracted; no file written.", flush=True)
