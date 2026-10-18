#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
acq_maiac_aod_extract.py
Extract daily MODIS MAIAC AOD for AirNow sensors via Google Earth Engine.

Supports both 0.47 µm (AOD_047, primary classification band) and
0.55 µm (AOD_055, supplementary) in a single script.

Usage:
    python acq_maiac_aod_extract.py --band 047
    python acq_maiac_aod_extract.py --band 055

Method: buffered reduceRegions (BUFFER_M radius) averaged across SCALE_M pixels.
QA filtering retains clear/possibly-cloudy land pixels with quality flag 0, 3, or 11.
"""

import argparse
import ee
import pandas as pd
from datetime import datetime, timedelta
from config import (
    GEE_PROJECT, GEE_SENSOR_ASSET, GEE_BOUNDARY_ASSET,
    COLLECTION_MAIAC, SENSOR_ID_FIELD,
    STUDY_START, STUDY_END,
    BATCH_SIZE, SCALE_M, TILE_SCALE, BUFFER_M, BUFFER_MAXERR_M,
    OUT_AOD_047, OUT_AOD_055,
)

# ---------------------------------------------------------------------------
# Run configurations (one per band)
# ---------------------------------------------------------------------------
BAND_CONFIGS = {
    "047": {
        "ee_band":    "Optical_Depth_047",
        "out_col":    "AOD_047",
        "output_csv": OUT_AOD_047,
    },
    "055": {
        "ee_band":    "Optical_Depth_055",
        "out_col":    "AOD_055",
        "output_csv": OUT_AOD_055,
    },
}

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------
parser = argparse.ArgumentParser(description="Extract MAIAC AOD for AirNow sensors.")
parser.add_argument(
    "--band",
    choices=list(BAND_CONFIGS.keys()),
    required=True,
    help="AOD band to extract: 047 (0.47 µm) or 055 (0.55 µm)",
)
args = parser.parse_args()
cfg = BAND_CONFIGS[args.band]

EE_BAND    = cfg["ee_band"]
OUT_COL    = cfg["out_col"]
OUTPUT_CSV = str(cfg["output_csv"])

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
# Load assets and prepare buffered sensor geometries
# ---------------------------------------------------------------------------
sensors  = ee.FeatureCollection(GEE_SENSOR_ASSET)
boundary = ee.FeatureCollection(GEE_BOUNDARY_ASSET)
sensors  = sensors.filterBounds(boundary.geometry())

def prep_sensor(f):
    """Store lon/lat as properties and buffer the geometry for area sampling."""
    coords = f.geometry().coordinates()
    f = f.set({"lon": coords.get(0), "lat": coords.get(1)})
    return f.setGeometry(f.geometry().buffer(BUFFER_M, BUFFER_MAXERR_M))

sensors_buf  = sensors.map(prep_sensor)
sensor_list  = sensors_buf.toList(sensors_buf.size())
num_sensors  = sensor_list.size().getInfo()
total_batches = (num_sensors + BATCH_SIZE - 1) // BATCH_SIZE
print(f"Found {num_sensors} sensors ({total_batches} batches of up to {BATCH_SIZE}).", flush=True)

# ---------------------------------------------------------------------------
# MAIAC QA filter
# ---------------------------------------------------------------------------
def apply_maiac_qa_filter(image):
    qa = image.select("AOD_QA")
    ok_cloud = qa.bitwiseAnd(7).lte(2)                       # bits 0-2: clear or possibly cloudy
    ok_land  = qa.rightShift(3).bitwiseAnd(3).eq(0)          # bits 3-4: land only
    ok_adj   = qa.rightShift(5).bitwiseAnd(7).lte(1).Or(     # bits 5-7: normal or one-edge-of-cloud
               qa.rightShift(5).bitwiseAnd(7).eq(3))
    ok_qa    = qa.rightShift(8).bitwiseAnd(15)               # bits 8-11: retrieval quality
    ok_qa    = ok_qa.eq(0).Or(ok_qa.eq(3)).Or(ok_qa.eq(11))
    return image.updateMask(ok_cloud.And(ok_land).And(ok_adj).And(ok_qa))

# ---------------------------------------------------------------------------
# Main extraction loop
# ---------------------------------------------------------------------------
collection = ee.ImageCollection(COLLECTION_MAIAC)
all_dfs    = []

start_dt = datetime.strptime(STUDY_START, "%Y-%m-%d")
end_dt   = datetime.strptime(STUDY_END,   "%Y-%m-%d")
dates    = [start_dt + timedelta(days=i)
            for i in range((end_dt - start_dt).days + 1)]

for current in dates:
    date_str = current.strftime("%Y-%m-%d")
    next_str = (current + timedelta(days=1)).strftime("%Y-%m-%d")
    print(f"\nDate {date_str}", flush=True)

    daily = (collection
             .filterDate(date_str, next_str)
             .map(apply_maiac_qa_filter)
             .map(lambda img: img.select(EE_BAND)
                                .multiply(0.001)
                                .rename(OUT_COL)
                                .copyProperties(img, ["system:time_start"])))

    try:
        if daily.size().getInfo() == 0:
            print("  – no valid MAIAC images, skip", flush=True)
            continue
    except Exception as e:
        print(f"  – error checking images: {e}", flush=True)
        continue

    mean_img = daily.mean().rename(OUT_COL)
    day_rows = []

    for i in range(0, num_sensors, BATCH_SIZE):
        batch_num  = (i // BATCH_SIZE) + 1
        batch_fc   = ee.FeatureCollection(sensor_list.slice(i, i + BATCH_SIZE))
        print(f"  – batch {batch_num}/{total_batches}", flush=True)

        sampled = mean_img.reduceRegions(
            collection=batch_fc,
            reducer=ee.Reducer.mean(),
            scale=SCALE_M,
            tileScale=TILE_SCALE,
        )
        sampled = (sampled
                   .select([SENSOR_ID_FIELD, "lon", "lat", "mean"], None, False)
                   .map(lambda f: f.setGeometry(None)))

        try:
            feats = sampled.getInfo()["features"]
        except Exception as e:
            print(f"    – batch sampling error: {e}", flush=True)
            continue

        for f in feats:
            props = f.get("properties", {})
            aod   = props.get("mean")
            if aod is None:
                continue
            day_rows.append({
                "time":          date_str,
                SENSOR_ID_FIELD: props.get(SENSOR_ID_FIELD),
                "lon":           props.get("lon"),
                "lat":           props.get("lat"),
                OUT_COL:         aod,
            })

    print(f"  – found {len(day_rows)} obs", flush=True)
    if day_rows:
        all_dfs.append(pd.DataFrame(day_rows))

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------
if all_dfs:
    df = pd.concat(all_dfs, ignore_index=True)
    df = df[[SENSOR_ID_FIELD, "lon", "lat", "time", OUT_COL]]
    df.to_csv(OUTPUT_CSV, index=False, compression="gzip")
    print(f"\n✓ Saved {len(df)} observations to {OUTPUT_CSV}", flush=True)
else:
    print("No data extracted; no file written.", flush=True)
