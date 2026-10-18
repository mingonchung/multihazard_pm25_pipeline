## =============================================================================
## util_storm_zone_resolve.R
## Shared helper for stage04_noaa_storm_events_join.R (AirNow) and
## stage04_pa_noaa_storm_events_join.R (PurpleAir). Sourced, not run.
##
## NOAA Storm Events gives each record a location code, CZ_FIPS, whose meaning
## depends on CZ_TYPE:
##   CZ_TYPE = "C"   CZ_FIPS is a county FIPS code
##   CZ_TYPE = "Z"   CZ_FIPS is an NWS public forecast zone number
## A zone number is never compared with a county code. Zone-coded events are
## first resolved to the counties of their zone (this file) and only then
## joined to sensors by county, exactly like county-coded events.
##
## Resolution order for a zone-coded event (state code + zone number):
##   1. NWS zone-county correlation file (FILE_ZONE_COUNTY)
##        https://www.weather.gov/gis/ZoneCounty
##        pipe-delimited, no header:
##        STATE|ZONE|CWA|NAME|STATE_ZONE|COUNTY|FIPS|TIME_ZONE|FE_AREA|LAT|LON
##        It describes present-day zones only.
##   2. Lookup file (FILE_ZONE_LEGACY) for zones that were renumbered or
##        redrawn during the study period and are absent from the NWS file.
##        CSV with columns STATE_FIPS, ZONE, FIPS (5-digit county code);
##        rows without a 5-digit FIPS are ignored. The NWS file takes
##        precedence when a zone is in both.
##   3. Otherwise the event is dropped and counted (marine area codes,
##        territory codes 96-99, and any zone found in neither file).
##
## Limit: a zone that covers part of a county is assigned to the whole county.
## =============================================================================

read_zone_county <- function(nws_file, legacy_file = NULL) {
  if (!file.exists(nws_file))
    stop("NWS zone-county file not found: ", nws_file)

  nws <- data.table::fread(nws_file, sep = "|", header = FALSE, quote = "",
                           colClasses = "character", fill = TRUE)
  if (ncol(nws) < 7L)
    stop("Unexpected layout in NWS zone-county file (need at least 7 fields): ", nws_file)
  data.table::setnames(nws, c(2L, 7L), c("ZONE", "FIPS"))
  nws <- nws[grepl("^[0-9]+$", ZONE) & grepl("^[0-9]{5}$", FIPS)]
  zc  <- unique(nws[, .(STATE_FIPS  = as.integer(substr(FIPS, 1, 2)),
                        ZONE_NUM    = as.integer(ZONE),
                        CNTY_FIPS   = as.integer(substr(FIPS, 3, 5)),
                        zone_source = "NWS file")])

  if (!is.null(legacy_file) && file.exists(legacy_file)) {
    leg <- data.table::fread(legacy_file, colClasses = "character",
                             na.strings = c("", "NA"))
    need <- c("STATE_FIPS", "ZONE", "FIPS")
    if (!all(need %in% names(leg)))
      stop("Lookup file needs columns ", paste(need, collapse = ", "), ": ", legacy_file)
    leg <- leg[!is.na(FIPS) & grepl("^[0-9]{5}$", FIPS)]
    bad <- leg[as.integer(substr(FIPS, 1, 2)) != as.integer(STATE_FIPS)]
    if (nrow(bad))
      stop("Lookup file: county FIPS outside the row's state for zone(s) ",
           paste(unique(paste(bad$STATE_FIPS, bad$ZONE)), collapse = "; "))
    leg <- unique(leg[, .(STATE_FIPS  = as.integer(STATE_FIPS),
                          ZONE_NUM    = as.integer(ZONE),
                          CNTY_FIPS   = as.integer(substr(FIPS, 3, 5)),
                          zone_source = "lookup file")])
    ## NWS file takes precedence: keep lookup rows only for zones it lacks
    leg <- leg[!unique(zc[, .(STATE_FIPS, ZONE_NUM)]), on = .(STATE_FIPS, ZONE_NUM)]
    zc  <- rbind(zc, leg)
  } else {
    message("  Zone lookup file not found; using the NWS file only.")
  }
  zc[]
}

## z_events:    data.table with EVENT_ID, EVENT_TYPE, STATE_FIPS, ZONE_NUM,
##              begin_dt, end_dt (plus any other columns, which are kept)
## zone_county: output of read_zone_county()
## Returns a list:
##   $events   one row per event x county, ZONE_NUM replaced by CNTY_FIPS
##   $log      number of events by outcome
##   $dropped  dropped (state code, zone number) pairs with event counts
resolve_zone_events <- function(z_events, zone_county) {
  z   <- data.table::copy(z_events)
  res <- zone_county[z, on = .(STATE_FIPS, ZONE_NUM), allow.cartesian = TRUE]

  ok   <- res[!is.na(CNTY_FIPS)]
  drop <- res[is.na(CNTY_FIPS)]
  drop[, reason := data.table::fcase(
    STATE_FIPS %in% 96:99, "territory code (96-99)",
    STATE_FIPS > 56L,      "marine or other area code",
    default =              "zone in neither the NWS file nor the lookup file")]

  ## Event counts by outcome (built so that zero events gives an empty table)
  log <- rbind(
    ok[,   .(EVENT_ID, outcome = paste0("resolved by ", zone_source))],
    drop[, .(EVENT_ID, outcome = paste0("dropped: ", reason))]
  )[, .(events = data.table::uniqueN(EVENT_ID)), by = outcome][order(outcome)]

  ok[, ZONE_NUM := NULL]
  list(events  = ok[],
       log     = log,
       dropped = drop[, .(events = .N), by = .(STATE_FIPS, ZONE_NUM, reason)][order(-events)])
}
