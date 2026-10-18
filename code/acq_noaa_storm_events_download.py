#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
acq_noaa_storm_events_download.py
Download NOAA Storm Events details CSV files for the study period.

Source: NOAA NCEI Storm Events Database
URL:    https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/

Filename pattern (NCEI convention):
    StormEvents_details-ftp_v1.0_dYYYY_cYYYYMMDD.csv.gz
    where d = data year, c = file creation/last-update date.
    The 'c' date changes when NCEI reprocesses a year, so filenames
    cannot be constructed without first scraping the directory index.

Usage:
    python acq_noaa_storm_events_download.py

Outputs one .csv.gz file per year into OUTPUT_DIR (below), which must equal
DIR_STORM_RAW in config.R. Files already present are skipped (re-run safe).

Access note: NCEI does not require authentication. If a year's file has
been reprocessed since last download, delete the local file and re-run
to fetch the updated version.
"""

import re
import sys
import urllib.request
from pathlib import Path

# ---------------------------------------------------------------------------
# Inline configuration (mirrors config.py values; no import to keep standalone)
# Change ROOT_DIR to match config.py before running.
# ---------------------------------------------------------------------------
BASE_URL    = "https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/"
STUDY_START = 2014
STUDY_END   = 2025

ROOT_DIR   = "/data001/projects/chungmin"   # must match ROOT_DIR in config.py
OUTPUT_DIR = Path(ROOT_DIR) / "aqdata" / "storm_events" / "raw"
# ^ must equal DIR_STORM_RAW in config.R (read by stage04_noaa_storm_events_join.R
#   and stage04_pa_noaa_storm_events_join.R)

# All event types are downloaded; no filtering is done here. Downstream use:
#   stage04 (both networks): county-coded records (CZ_TYPE = "C") of all types;
#     zone-coded records (CZ_TYPE = "Z") of Hurricane, Tropical (Storm,
#     Depression) and Dust Storm types only, resolved to counties;
#     marine records dropped.
#   stage07_08 (both networks): hazard categories by EVENT_TYPE pattern:
#     "Tornado" -> Tornado; "Hurricane" -> Hurricane; "Tropical" -> Tropical
#     Storm; "Flood" -> Flood; "Dust Storm" with AOD > CLN_THR -> Dust. Any Storm Events
#     record in an hour removes that hour from the Baseline pool.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
def fetch_directory_index(url: str) -> str:
    """Return the HTML text of the NCEI directory listing."""
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req, timeout=30) as resp:
        return resp.read().decode("utf-8")


def parse_details_filenames(html: str) -> dict:
    """
    Extract details filenames from the directory HTML.
    Returns {data_year (int): filename (str)}.
    Keeps only the most-recently-published version when multiple exist
    for the same data year (highest 'c' date).
    """
    pattern = re.compile(
        r"(StormEvents_details-ftp_v\d+\.\d+_d(\d{4})_c(\d{8})\.csv\.gz)"
    )
    matches = pattern.findall(html)     # (filename, data_year, creation_date)

    best: dict = {}
    for filename, data_year_str, creation_date in matches:
        year = int(data_year_str)
        # Keep the entry with the latest creation date (string comparison works for YYYYMMDD)
        if year not in best or creation_date > best[year][1]:
            best[year] = (filename, creation_date)

    return {year: info[0] for year, info in best.items()}


def download_file(url: str, dest: Path) -> None:
    """Download url to dest, streaming to avoid large in-memory buffers."""
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req, timeout=120) as resp, open(dest, "wb") as out:
        while chunk := resp.read(1 << 20):   # 1 MB chunks
            out.write(chunk)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

print(f"Fetching directory index from: {BASE_URL}", flush=True)
try:
    html = fetch_directory_index(BASE_URL)
except Exception as e:
    print(f"✗ Failed to fetch directory index: {e}", file=sys.stderr)
    raise SystemExit(1)

all_files = parse_details_filenames(html)
print(f"Found {len(all_files)} data years in directory.", flush=True)

years_needed = range(STUDY_START, STUDY_END + 1)
missing = [y for y in years_needed if y not in all_files]
if missing:
    print(f"⚠  No file found for year(s): {missing}. These may not yet be published.", flush=True)

errors = []
for year in years_needed:
    if year not in all_files:
        continue

    filename = all_files[year]
    dest     = OUTPUT_DIR / filename
    url      = BASE_URL + filename

    if dest.exists():
        print(f"  {year}: already present ({filename}), skipping.", flush=True)
        continue

    print(f"  {year}: downloading {filename} ...", end=" ", flush=True)
    try:
        download_file(url, dest)
        print(f"✓ ({dest.stat().st_size // 1024:,} KB)", flush=True)
    except Exception as e:
        print(f"✗ FAILED: {e}", flush=True)
        errors.append((year, str(e)))
        if dest.exists():
            dest.unlink()     # remove partial file

if errors:
    print(f"\n✗ {len(errors)} download(s) failed:", file=sys.stderr)
    for year, msg in errors:
        print(f"  {year}: {msg}", file=sys.stderr)
    raise SystemExit(1)

print(f"\n✓ All files downloaded to: {OUTPUT_DIR}", flush=True)
