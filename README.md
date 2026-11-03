# Multi-hazard PM₂.₅ attribution dataset for the United States, 2014–2025: pipeline code

Code for the data harmonization pipeline described in:

> Chung, M. G. et al. A multi-hazard PM2.5 attribution dataset for the United States, 2014–2025. *Scientific Data* (submitted).

The pipeline attaches smoke, fire, storm, volcanic, meteorological, drought, and aerosol data to hourly PM₂.₅ from two networks and assigns each monitor-day or sensor-day to one of 12 hazard categories or Baseline, or leaves it unclassified (`hazard_cat = NA`).

- **AirNow** regulatory monitors, 2014–2025 (monitor, monitor-day)
- **PurpleAir** low-cost sensors, outdoor and indoor, 2020–2025 (sensor, sensor-day). The paper reports outdoor sensors.

Stage numbers in this README follow Figure 1 of the paper (Stages 1–8). Script file names keep their own numbers (`stage01` … `stage09`). The mapping is in [Processing stages](#processing-stages).

---

## Released data

| File | Content | Where |
|---|---|---|
| `airnow_daily_classified_US.csv.gz` (470 MB) | AirNow daily classified records, CONUS, Alaska, and Hawaii, 2014–2025. Columns in Table 4 of the paper | Zenodo, https://doi.org/10.5281/zenodo.23930322 |
| `airnow_pm25_location_us_attrs.zip` (shapefile and its companion files) | AirNow monitor locations with state, county, and urban attributes. Monitor coordinates are not repeated in the daily file. Join to it by `AQSID` | Zenodo, https://doi.org/10.5281/zenodo.23930322 |
| Code and the hand-prepared inputs (`inputs/`) | This repository | https://github.com/mingonchung/multihazard_pm25_pipeline, archived at Zenodo, https://doi.org/10.5281/zenodo.23930322 |

Not released: PurpleAir sensor records and sensor-level pipeline outputs (daily files, hourly tables, sensor lists and shapefiles, sensor-level result tables), and the Stage 5 hourly integration tables. Users with PurpleAir API access can produce the PurpleAir outputs with the released code ([PurpleAir input](#purpleair-input)).

---

## Repository layout

```
README.md
LICENSE
code/      All scripts, configuration files, and SLURM job files, in one folder.
           Scripts source config.R (R) or import config.py (Python) and helper
           scripts from the working directory, so run everything from code/.
inputs/    Hand-prepared inputs released with the code (see Inputs prepared by hand)
```

File names in `code/` group the scripts:

| Prefix | Role |
|---|---|
| `config*` | Paths, date ranges, and parameters (`config.R`, `config.py`; PurpleAir: `config_pa.R`, `config_pa.py`) |
| `acq_` | Data acquisition and GEE extraction |
| `util_` | Location shapefiles, U.S. attributes, terrain, Storm Events zone resolution, GVP reading |
| `stage` | Processing stages (Figure 1) |
| `ms1_` | Tables and figures of the paper (MS1) and the environment report |
| `sub_`, `submit_` | SLURM job files and submission scripts |

PurpleAir versions of the scripts carry `_pa` in the file name (e.g., `stage07_08_pa_daily_classification.R`). `stage01_hms_fired_join.R` is shared by both networks.

---

## Scripts

### Acquisition

| Script | Function |
|---|---|
| `acq_airnow_download.R` | AirNow hourly observation files from the AirNow AWS S3 bucket |
| `acq_airnow_hourly_combine.R` | Combine hourly files into one file per parameter (PM₂.₅: `FILE_AIRNOW_PM25`) |
| `acq_airnow_station_metadata.R` | Monitor coordinates from the active-sites and monitoring-site-locations files (completed by hand, see below) |
| `acq_hms_download.R` | NOAA HMS smoke polygons (fire points are also downloaded, not used) |
| `acq_maiac_aod_extract.py` | GEE: MODIS MAIAC AOD, 0.47 and 0.55 µm, 5-km buffer mean |
| `acq_gridmet_extract.py` | GEE: GridMET meteorology (2014–2025), historical temperature (1980–2013), SPEI |
| `acq_daymet_extract.py` | GEE: Daymet V4 (2014–2025) and historical temperature (1980–2013) |
| `acq_noaa_storm_events_download.py` | NOAA Storm Events annual details files from NCEI |
| `acq_pa_download.R` | PurpleAir hourly records through the PurpleAir API, with channel QA and correction (API key required) |
| `acq_pa_hourly_combine.R` | Combine PurpleAir hourly files by year and build the sensor list |
| `acq_pa_maiac_aod_extract.py`, `acq_pa_gridmet_extract.py`, `acq_pa_daymet_extract.py` | GEE extractions at PurpleAir sensor locations |

### Processing stages

| Stage (Fig. 1) | AirNow script | PurpleAir script | Function |
|---|---|---|---|
| 1 | `stage01_hms_fired_join.R`, `stage02_sensor_smoke_fire_assign.R` | `stage01_hms_fired_join.R`, `stage02_pa_sensor_smoke_fire_assign.R` | HMS × FIRED join and smoke density and FIRED IDs per monitor-day |
| 2 | `stage03_p95_climatology.R` | `stage03_pa_p95_climatology.R` | Calendar-day p95 of daily maximum and minimum temperature (±7-day window, 1991–2020 and 1981–2010), GridMET and Daymet |
| 3 | `stage04_noaa_storm_events_join.R` | `stage04_pa_noaa_storm_events_join.R` | Storm Events join by county (zone-coded Hurricane, Tropical Storm/Depression, and Dust Storm records resolved to counties) |
| 4 | `stage05_gvp_join.R` | `stage05_pa_gvp_join.R` | GVP eruption flags within `GVP_BUFFER_M` (100 km) of each volcano |
| 5 | `stage06_master_hourly_integration.R` | `stage06_pa_master_hourly_integration.R` | Hourly integration table (one row per monitor-hour or sensor-hour) |
| 6/7 | `stage07_08_daily_classification.R` | `stage07_08_pa_daily_classification.R` | Daily aggregation and hazard classification |
| 8 | `stage09_attribute_join.R` | `stage09_pa_attribute_join.R` | Terrain, U.S. attributes, and calendar fields; non-U.S. locations removed |

### Utilities

| Script | Function | Used by |
|---|---|---|
| `util_sensor_location_shp.R` | AirNow location shapefile (`FILE_AIRNOW_LOC_SHP`) | GEE extractions, utilities below |
| `util_sensor_us_attrs.R` | State and county FIPS, urban/rural class (2020 Census cartographic boundary files) | Stage 3, Stage 8 |
| `util_terrain_extract.R` | Elevation, slope, aspect (AWS Terrain Tiles, zoom 12, through `elevatr`) | Stage 8 |
| `util_pa_sensor_location_shp.R` | PurpleAir location and U.S.-attribute shapefiles | PurpleAir stages |
| `util_pa_terrain_extract.R` | PurpleAir terrain | Stage 8 (PurpleAir) |
| `util_storm_zone_resolve.R` | Zone-coded Storm Events records to counties | Stage 3 |
| `util_gvp_read.R` | GVP eruption list and volcano coordinates, joined by Volcano Number | Stage 4, `ms1_table7_*` |

### Tables and figures of the paper

| Script | Output |
|---|---|
| `ms1_table4_dataset_summary.R` | Table 5: monitor- and sensor-days, monitors and sensors, median [Q1, Q3] PM₂.₅ by hazard category |
| `ms1_table7_sensitivity_analyses_rev.R` (+ `.sub`) | Table 6: parameter sensitivity (AirNow) |
| `ms1_fig2_sensor_hazard_ranking.R` | Figure 2: primary, secondary, and tertiary hazard category per AirNow monitor |
| `ms1_fig3_concordance_check.R` | Figure 3: classification on the dates of eight documented events |
| `ms1_env_info_R.R`, `ms1_env_info_py.py` | R and Python package versions |

Script names keep the table numbers of an earlier draft (`table4` = Table 5, `table7` = Table 6). Figure 1 was drawn by hand.

### Orchestration

| File | Function |
|---|---|
| `submit_pipeline.sh`, `submit_pa_pipeline.sh` | Two-phase SLURM submission (acquisition, then processing) with job dependencies |
| `sub_*.sub`, `sub_pa_*.sub`, `sub_ms1_outputs.sub` | One job file per script. Headers (partition, memory, modules) are specific to our cluster |

---

## Environment

Versions used to produce the dataset:

- R 4.3.3 (CentOS Linux 8): data.table 1.17.8, R.utils 2.12.3, dplyr 1.1.4, tidyr 1.3.1, readr 2.1.5, lubridate 1.9.4, stringr 1.5.2, sf 1.0.19, readxl 1.4.5, fuzzyjoin 0.1.6.1, tigris 2.2.1, lutz 0.3.2, rnaturalearth 1.0.1, rnaturalearthdata 1.0.0, elevatr 0.99.1, terra 1.8.5, ggplot2 4.0.0, cowplot 1.2.0, svglite 2.1.3, ragg 1.3.3, systemfonts 1.1.0, tidyverse 2.0.0
- Python 3.13.13: earthengine-api 1.5.21, pandas 2.3.0
- Google Earth Engine (GEE)

Other versions have not been tested.

```r
install.packages(c(
  "data.table", "R.utils", "dplyr", "tidyr", "readr", "lubridate", "stringr",
  "tidyverse", "sf", "readxl", "fuzzyjoin", "tigris", "lutz",
  "rnaturalearth", "rnaturalearthdata", "elevatr", "terra",
  "ggplot2", "cowplot", "svglite", "ragg", "systemfonts"
))
```

`lutz` (time-zone lookup) is required by Stage 5. `R.utils` lets `data.table::fread` read `.gz` files. `ggplot2`, `cowplot`, `svglite`, `ragg`, and `systemfonts` are used only by the figure scripts.

```bash
pip install earthengine-api pandas
earthengine authenticate          # once per machine
```

---

## Configuration

1. Set `ROOT_DIR` in `config.R` and `config.py`. All other paths derive from it. `acq_noaa_storm_events_download.py` does not import `config.py`. Set its `ROOT_DIR` as well.
2. Replace the placeholder `YOUR_GEE_PROJECT` (`GEE_PROJECT` in `config.py` and `config_pa.py`) with your Earth Engine project. The GEE scripts read two uploaded assets under that project: the monitor location shapefile (`airnow`) and the U.S.–Canada boundary (`uscan`).
3. Place the hand-prepared inputs (next section) at the paths named in `config.R`.

Classification parameters are defined in `config.R` (Table 3 of the paper): `CLN_THR`, `PM_MAX`, `MIN_HOURS_PM`, `PM_FLOOR`, `HEAT_ABS_THR_K`, `HEAT_MIN_DAYS`, `SPEI_DROUGHT_THR`, `GVP_BUFFER_M`, and the p95 settings. GEE extraction settings (`BUFFER_M`, `BUFFER_MAXERR_M`, `SCALE_M`) are in `config.py`. The PurpleAir scripts use the same values through `config_pa.R` and `config_pa.py`.

---

## Inputs prepared by hand

| Input | Config name | Source and preparation |
|---|---|---|
| AirNow monitor locations | `FILE_AIRNOW_LOCS` | `inputs/airnow_aqsid_location_rev_010926.csv`. Output of `acq_airnow_station_metadata.R` with missing decimal coordinates entered by hand and an `AQSID_rev` column that links identifiers of the same or a co-located site (used only to share smoke and storm flags). Copy to `DIR_AIRNOW_HOURLY` |
| Lookup for old forecast zones | `FILE_ZONE_LEGACY` | `inputs/storm_zone_county_legacy.csv`. Zone numbers in the 2014–2025 Storm Events records that are absent from the NWS zone–county file. Copy to `DIR_STORM` |
| NWS zone–county correlation file | `FILE_ZONE_COUNTY` | Release `bp18mr25.dbx` from https://www.weather.gov/gis/ZoneCounty. Place in `DIR_STORM` |
| GVP eruption list | `FILE_GVP_XLSX` | Smithsonian GVP Holocene eruption list (confirmed eruptions), downloaded 2026-04-24, records through March 2026 (`GVP_Eruption_List_Holocene_20260424.xlsx`). Continuing eruptions carry the as-of date as end date |
| GVP volcano coordinates | `FILE_GVP_COORDS` | Smithsonian GVP Holocene volcano list (Volcanoes of the World v. 5.4.0), downloaded 2026-10-09. The site delivers an Excel 2003 XML `.xls`. Open it in Excel and save as `.xlsx` (`GVP_Volcano_List_Holocene_202610091715.xlsx`) |
| FIRED events | `FILE_FIRED_EVENTS` | `fired_usa_2000_to_2026_events.gpkg` (U.S. events through 2025), archived in the FIRED collection at CU Scholar (https://scholar.colorado.edu/collections/pz50gx05h) |
| Urban areas 2020 | `FILE_URBAN_SHP` | Census cartographic boundary file `cb_2020_us_ua20_500k`, saved as `cb_2020_us_ua20_500k_prj.shp` (any CRS; the script transforms it) |
| U.S.–Canada boundary | `FILE_USCAN_SHP` | `inputs/us_canada.shp` (with its companion files). Copy to `DIR_USCAN` and upload to GEE as the `uscan` asset. Monitors outside this boundary are removed in Stage 5 |
| PurpleAir hourly files | `DIR_PA_RAW` (`config_pa.R`) | See [PurpleAir input](#purpleair-input) |

---

## Execution order

`submit_pipeline.sh` (AirNow) and `submit_pa_pipeline.sh` (PurpleAir) submit the jobs below with SLURM dependencies. Run them from `code/`. Equivalent direct calls follow.

### AirNow: acquisition (parallel)

```bash
Rscript acq_airnow_download.R
Rscript acq_airnow_hourly_combine.R
Rscript acq_airnow_station_metadata.R       # then complete by hand, or use inputs/airnow_aqsid_location_rev_010926.csv
Rscript acq_hms_download.R
python  acq_maiac_aod_extract.py --band 047
python  acq_maiac_aod_extract.py --band 055
python  acq_gridmet_extract.py --run main
python  acq_gridmet_extract.py --run hist
python  acq_gridmet_extract.py --run spei
python  acq_daymet_extract.py --run main
python  acq_daymet_extract.py --run hist
python  acq_noaa_storm_events_download.py
```

### AirNow: utilities and stages

```bash
Rscript util_sensor_location_shp.R          # FILE_AIRNOW_LOC_SHP (see note below)
Rscript util_sensor_us_attrs.R              # FILE_AIRNOW_USATTRS_SHP
Rscript util_terrain_extract.R              # FILE_AIRNOW_TERRAIN_SHP (internet access)

Rscript stage01_hms_fired_join.R            # Stage 1
Rscript stage02_sensor_smoke_fire_assign.R  # Stage 1
Rscript stage03_p95_climatology.R           # Stage 2
Rscript stage04_noaa_storm_events_join.R    # Stage 3
Rscript stage05_gvp_join.R                  # Stage 4
Rscript stage06_master_hourly_integration.R # Stage 5
Rscript stage07_08_daily_classification.R   # Stage 6/7
Rscript stage09_attribute_join.R            # Stage 8 -> FILE_DAILY_INTEGRATED
```

Stages 1–4 do not depend on one another. Stage 3 needs the county codes from `util_sensor_us_attrs.R`. Stages 5–8 run in sequence.

`util_sensor_location_shp.R` does not overwrite an existing shapefile. It reports differences from it. Run with `--overwrite` to replace it. Upload the shapefile to GEE before the extractions.

### PurpleAir

```bash
Rscript acq_pa_hourly_combine.R <i> <outdoor|indoor>      # year 2019+i (i = 1-6); yearly file + sensor list
Rscript util_pa_sensor_location_shp.R <outdoor|indoor>
python  acq_pa_maiac_aod_extract.py ...                    # arguments in sub_pa_gee_*.sub
python  acq_pa_gridmet_extract.py ...
python  acq_pa_daymet_extract.py ...
Rscript util_pa_terrain_extract.R <outdoor|indoor>

Rscript stage02_pa_sensor_smoke_fire_assign.R <type>       # needs the AirNow stage01 output
Rscript stage03_pa_p95_climatology.R --type <type> --source <gridmet|daymet>
Rscript stage04_pa_noaa_storm_events_join.R <type>
Rscript stage05_pa_gvp_join.R <type>
Rscript stage06_pa_master_hourly_integration.R <type> <year>
Rscript stage07_08_pa_daily_classification.R <type>
Rscript stage09_pa_attribute_join.R <type>
```

`stage06_pa` processes one local year per run and reads the boundary hours of the previous and next yearly combined files. Run it after all combine years are complete.

### Tables and figures of the paper

Run after `stage09` and `stage09_pa` (`sub_ms1_outputs.sub` and `ms1_table7_sensitivity_analyses_rev.sub`):

```bash
Rscript ms1_table4_dataset_summary.R
Rscript ms1_fig2_sensor_hazard_ranking.R
Rscript ms1_fig3_concordance_check.R
Rscript ms1_table7_sensitivity_analyses_rev.R
```

Outputs are written to `ROOT_DIR/ms1/`. The sensitivity script reads the AirNow hourly integration table (`FILE_PM25_AOD_MET`) and re-runs a copy of the classifier with one parameter changed at a time. It does not re-run pipeline stages or edit `config.R`, and it checks that its default run reproduces the category counts and medians of the Stage 8 file. Its classifier must stay identical to `stage07_08_daily_classification.R`. The script writes a checkpoint after the parameter runs, and `--gvp-only` repeats the two GVP buffer runs from that checkpoint. It needs at least 300 GB of memory.

### Re-runs

`stage02_pa`, `stage04_pa`, and `stage06_pa` skip outputs that already exist. Move old outputs before re-running them. The AirNow stages overwrite their outputs. `acq_airnow_hourly_combine.R` appends to its output files and stops if any already exist. Move them before re-running.

To change a parameter, edit `config.R` and re-run from the stage that applies it: the GEE extraction for `BUFFER_M` and `SCALE_M`, Stage 2 for the p95 settings, Stage 4 for `GVP_BUFFER_M`, and Stage 6/7 for the classification thresholds.

---

## PurpleAir input

The PurpleAir records used in the paper are quality-assured and corrected hourly PM₂.₅ obtained from the U.S. EPA Remote Sensing Information Gateway (RSIG), which holds PurpleAir data under a Materials Transfer Agreement between PurpleAir LLC and the U.S. EPA (MTA #1650-26). These records are not redistributed.

`acq_pa_download.R` is the released alternative. It downloads hourly records through the PurpleAir API (https://api.purpleair.com, registration and a read key required), applies the channel QA and the extended U.S.-wide correction of Barkjohn et al. (2022, Equations 1–3 of the paper), and writes yearly files that `acq_pa_hourly_combine.R` reads in the same way. Supply the read key through an environment variable (see the script), never in the code. Results can differ slightly from the RSIG-based records. Do not mix files from the two routes in one folder and year.

---

## Time base

- AirNow and PurpleAir hourly times are UTC. `datetime` in the hourly tables is UTC.
- `GMT_Offset` is the local **standard** time offset (no daylight saving) of the time zone at each monitor or sensor location (`lutz`, offset in mid-January). AirNow's reported offset, which is wrong at some sites, is used only where the lookup fails.
- `date` is the local standard date (UTC + `GMT_Offset`). A monitor-day or sensor-day runs from midnight to midnight local standard time and is kept if it has at least `MIN_HOURS_PM` (18) valid hourly values (40 CFR Part 50, Appendix N). Hourly values below `PM_FLOOR` (-5 µg/m³) are set to missing first. Days with a daily mean above `PM_MAX` (1,000 µg/m³) are removed.
- Daily sources (HMS/FIRED, MAIAC AOD, GridMET, SPEI, Daymet) are joined on `date`, and the p95 thresholds on its month and day. Storm Events (converted to UTC) are joined on `datetime`. GVP eruption days are joined on `datetime` (AirNow) or `date` (PurpleAir).
- The first and last local days of each study period are partial and usually fail `MIN_HOURS_PM`.

---

## Notes on processing

### Storm Events: county- and zone-coded records

`CZ_FIPS` is a county code in county-coded records (`CZ_TYPE = "C"`) and an NWS forecast-zone number in zone-coded records (`CZ_TYPE = "Z"`). The two are processed separately.

- County-coded: all event types, joined to monitors by state and county FIPS.
- Zone-coded: Hurricane, Tropical Storm/Depression, and Dust Storm only. Each zone is resolved to its counties through the NWS file, then the lookup file, and joined by state and county. Unresolved events (marine areas, territory codes) are dropped and counted in the log.
- Marine records (`CZ_TYPE = "M"`) are dropped.
- Event times are converted from local standard time to UTC. Records that overlap at a monitor-hour are kept together (`EVENT_TYPE` values joined with `;`).
- Any Storm Events record in an hour removes that hour from Baseline, including event types that are not hazard categories (e.g., Thunderstorm Wind, Heavy Rain).

### Temperature source

GridMET covers CONUS only. Daymet also covers Alaska and Hawaii. `stage07_08` and `stage07_08_pa` take the temperature for the Heat categories and the Baseline temperature condition from:

- GridMET where `tmmx` is available: `tmmx` (K) against `tmmx_p95_1991_2020` and `HEAT_ABS_THR_K` (305.372 K)
- Daymet where `tmmx` is missing: `tmax` (°C) against `tmax_p95_1991_2020` and the same floor in °C (32.2 °C).

Where neither source has a value, Heat is not assigned and the Baseline temperature condition is not applied. Drought uses GridMET SPEI only, so Wildfire+Drought, Heat+Drought, and Wildfire+Heat+Drought are assigned in CONUS only. Daymet has no 31 December in leap years. Where GridMET is missing, the temperature condition is not evaluated on that day.

### Reading the outputs

- Column definitions: Table 4 of the paper.
- Read identifiers as text, e.g. `fread(f, colClasses = list(character = c("AQSID", "AQSID_join")))` (AirNow) or `colClasses = list(character = "STATION")` (PurpleAir). `AQSID` carries a leading apostrophe. `AQSID_join` (and `AQSID_norm`, identical) is the same identifier without it and loses leading zeros if read as a number. These two join keys are not listed in Table 4.
- `hazard_cat` strings: `Volcano`, `Wildfire+Heat+Drought`, `Wildfire+Heat`, `Wildfire+Drought`, `Wildfire`, `Dust`, `Heat+Drought`, `Heat`, `Tornado`, `Hurricane`, `TropicalStorm` (Tropical Storm in the paper), `Flood`, `Baseline`, and NA (unclassified).
- Unclassified days meet neither a hazard criterion nor the Baseline conditions. They are kept for temporal continuity and can be classified further with other data.
- HMS smoke density, FIRED identifiers, and Storm Events `EVENT_TYPE` are in the Stage 5 hourly integration table, not in the daily files.
- Monitor-days with `tmmx` missing and `tmax` present were classified with Daymet temperature.
- The location shapefile lists four monitors with territory state codes. They have no records in the daily file.

---

## Data sources and citations

| Dataset | Access in the pipeline | Citation |
|---|---|---|
| AirNow hourly PM₂.₅ | `acq_airnow_download.R` (https://s3-us-west-1.amazonaws.com/files.airnowtech.org/airnow/) | U.S. EPA AirNow |
| PurpleAir hourly PM₂.₅ | RSIG (not redistributed) or `acq_pa_download.R` (PurpleAir API) | Barkjohn et al. 2021, doi:10.5194/amt-14-4617-2021. Barkjohn et al. 2022, doi:10.3390/s22249669 |
| NOAA HMS smoke polygons | `acq_hms_download.R` (https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/) | Rolph et al. 2009, doi:10.1175/2008WAF2222165.1 |
| FIRED | Manual (see above) | Balch et al. 2020, doi:10.3390/rs12213498 |
| MODIS MAIAC AOD (MCD19A2 v6.1) | GEE `MODIS/061/MCD19A2_GRANULES` | doi:10.5067/MODIS/MCD19A2.061. Lyapustin et al. 2018, doi:10.5194/amt-11-5741-2018 |
| GridMET meteorology | GEE `IDAHO_EPSCOR/GRIDMET` | Abatzoglou 2013, doi:10.1002/joc.3413 |
| GridMET drought (SPEI) | GEE `GRIDMET/DROUGHT` | https://www.climatologylab.org/gridmet.html |
| Daymet V4 | GEE `NASA/ORNL/DAYMET_V4` | Thornton et al. 2022, doi:10.3334/ORNLDAAC/2129 |
| NOAA Storm Events | `acq_noaa_storm_events_download.py` (https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/) | NOAA NCEI Storm Events Database, https://www.ncei.noaa.gov/access/storm-events-database/ |
| NWS zone–county correlation | Manual | https://www.weather.gov/gis/ZoneCounty |
| GVP eruptions and volcanoes | Manual | Global Volcanism Program. Volcanoes of the World (v. 5.4.0; 7 Aug 2026). Smithsonian Institution, doi:10.5479/si.GVP.VOTW5-2026.5.4 |
| AWS Terrain Tiles (Mapzen) | `elevatr` | Hollister, elevatr 0.99.1, doi:10.5281/zenodo.8335450 |
| U.S. Census cartographic boundaries (2020) | `tigris` (states, counties), manual (urban areas) | https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html |

PurpleAir sensor data were provided by PurpleAir LLC (purpleair.com) under a Materials Transfer Agreement (MTA #1650-26) with the U.S. EPA and housed and processed by the RSIG team. Publications that use PurpleAir outputs produced from these records should carry this attribution.

---

## Approximate runtimes (AirNow)

Measured on our Linux cluster. GEE extraction times depend on queue load. `ms1_table7_sensitivity_analyses_rev.R` needs at least 300 GB of memory.

| Script | Approx. runtime |
|---|---|
| `acq_airnow_download.R` | 4–6 hours (network-bound) |
| `acq_maiac_aod_extract.py` (per band) | 12–24 hours (GEE) |
| `acq_gridmet_extract.py --run main` | 6–10 hours (GEE) |
| `acq_gridmet_extract.py --run hist` | 8–12 hours (GEE) |
| `acq_gridmet_extract.py --run spei` | 1–2 hours (GEE) |
| `acq_daymet_extract.py --run main` | 6–10 hours (GEE) |
| `acq_daymet_extract.py --run hist` | 8–12 hours (GEE) |
| `stage01_hms_fired_join.R` | 2–4 hours |
| `stage03_p95_climatology.R` | 1–2 hours |
| `stage04_noaa_storm_events_join.R` | 30–60 minutes |
| `stage06_master_hourly_integration.R` | 3–5 hours |
| `stage07_08_daily_classification.R` | 1–2 hours |
| `stage09_attribute_join.R` | < 30 minutes |
| `util_terrain_extract.R` | 1–3 hours (network-bound) |
| `util_sensor_us_attrs.R` | < 10 minutes |

---

## License

Code: MIT License (`LICENSE`). Data (AirNow daily file and monitor location shapefile): Creative Commons Attribution 4.0 International (CC BY 4.0).

## Citation

Cite the paper above and the Zenodo record (code and AirNow data):

> Chung, M. G., Crawford, B., Clements, N., Holder, A., St. Denis, L. A. & Damby, D. E. A multi-hazard PM2.5 attribution dataset for the United States, 2014–2025: AirNow daily data and pipeline code (v1.0.1). *Zenodo* https://doi.org/10.5281/zenodo.23930322 (2026).
