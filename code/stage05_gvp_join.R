rm(list=ls())

library(readxl)
library(dplyr)
library(sf)
library(tidyverse)
library(lubridate)
library(fuzzyjoin)

sf_use_s2(TRUE)

source("config.R")

# Read AQ data & station-location file, reduce to unique stations
aq_raw <- read_csv(FILE_AIRNOW_PM25,
                   col_types = cols(
                     Date   = col_date("%m/%d/%y"),
                     Time   = col_time("%H:%M"),
                     AQSID  = col_character(),
                     .default = col_guess())) %>%  
          distinct() 

aq <- aq_raw %>%
  mutate(
    yr  = year(Date),
    mo  = month(Date),
    dy  = day(Date),
    hr  = hour(Time),
    min = minute(Time),
    datetime = make_datetime(year = yr,
                             month = mo,
                             day = dy,
                             hour = hr,
                             min = min,
                             tz = "UTC")) %>%
  select(datetime, everything(), -yr, -mo, -dy, -hr, -min, -Date, -Time)

loc <- read_csv(FILE_AIRNOW_LOCS,
                col_types = cols(
                  AQSID        = col_character(),
                  AQSID_rev    = col_character(),
                  lat_numeric  = col_double(),
                  long_numeric = col_double()))

# pick unique station locations
stations <- aq %>% 
  distinct(AQSID) %>% 
  left_join(loc, by = "AQSID") %>% 
  left_join(
    loc %>% 
      select(AQSID_rev = AQSID,       # rename for this join
             lat2    = lat_numeric, 
             lon2    = long_numeric),
    by = "AQSID_rev") %>%
  # pick lat/lon from whichever join succeeded
  mutate(
    lat = coalesce(lat_numeric, lat2),
    lon = coalesce(long_numeric, lon2)) %>%
  filter(!is.na(lat) & !is.na(lon)) %>%
  select(AQSID, AQSID_rev, lat, lon)

# Convert to an sf point layer
stations_sf <- st_as_sf(
  stations,
  coords = c("lon", "lat"),
  crs    = 4326,
  remove = FALSE)

# Assign STATE / COUNTY FIPS via spatial join
# read & filter the GVP Excel
source("util_gvp_read.R")
gvp2 <- read_gvp_eruptions(2014L, 2025L)

# 2. Turn eruptions into an sf point layer (WGS84)
gvp_sf <- st_as_sf(
  gvp2,
  coords = c("Longitude", "Latitude"),
  crs    = 4326,     # WGS84
  remove = FALSE)

# 3. Read U.S. & Canada boundary and clip
us_can <- st_read(FILE_USCAN_SHP) %>% st_transform(4326)

# we only want eruptions that fall within those polygons
gvp_uscan <- st_filter(gvp_sf, us_can, .predicate = st_within)

# Buffer each eruption 100 km, then find nearby stations
erupt_buffers <- st_buffer(gvp_uscan, dist = GVP_BUFFER_M)

# now find all stations whose point lies in any buffer
stations_near_erupt <- st_filter(
  stations_sf, 
  erupt_buffers, 
  .predicate = st_within) %>% 
  st_join(
    erupt_buffers %>% select(Volcano_Name = `Volcano_Name`),
    join = st_within)

stations_near_erupt <-  
  stations_near_erupt %>% 
  st_drop_geometry() %>%
  inner_join(
    gvp_uscan %>%
      st_drop_geometry() %>% 
      select(Volcano_Number,
             Volcano_Name,
             Eruption_Number,
             Eruption_Category,
             VEI,
             start_date,
             end_date),
    by = c("Volcano_Name")) 

# join back to AQ records
aq_erupt <- aq %>% 
  inner_join(
    stations_near_erupt %>% st_drop_geometry() %>% select(AQSID) %>% distinct(),
    by = "AQSID")

aq_erupt <- aq_erupt %>% 
  inner_join(
    stations_near_erupt %>% 
      select(AQSID, 
             Volcano_Number,
             Volcano_Name,
             Eruption_Number,
             Eruption_Category,
             VEI,
             start_date,
             end_date) %>%
      distinct(),                        # one row per station×eruption
    by = "AQSID"
  )

aq_with_gvp <- aq_erupt %>%
  filter(datetime >= start_date,
         datetime <= end_date) %>%
  select(
    datetime, AQSID, GMT_Offset, Parameter, Units, Value,
    Volcano_Number,
    Volcano_Name,
    Eruption_Number,
    Eruption_Category,
    VEI)

## write csv.gz
write_csv(aq_with_gvp, FILE_GVP_OUT)

message("DONE")