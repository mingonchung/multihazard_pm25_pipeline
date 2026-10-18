## =============================================================================
## util_gvp_read.R
## Read GVP eruption records for stage05_gvp_join.R, stage05_pa_gvp_join.R and
## ms1_table7_sensitivity_analyses_rev.R (sourced; defines read_gvp_eruptions()).
##
## Inputs (config.R):
##   FILE_GVP_XLSX    GVP Holocene eruption list (GVP_Eruption_List_Holocene_*.xlsx,
##                    downloaded 2026-04-24). Columns used: Volcano Number,
##                    Volcano Name, Eruption Number, Eruption Category, VEI,
##                    Start/End Year, Month, Day. Missing values are written "NULL".
##                    This export has no coordinates.
##   FILE_GVP_COORDS  GVP Holocene volcano list (GVP_Volcano_List_Holocene_*.xlsx;
##                    the site delivers an Excel 2003 XML ".xls", re-saved as
##                    .xlsx in Excel), or any GVP export with Volcano Number,
##                    Latitude, Longitude. Coordinates are joined by Volcano Number. If FILE_GVP_XLSX itself has
##                    Latitude/Longitude (older eruption-search export), those
##                    are used and FILE_GVP_COORDS is not read.
##
## Rules (unchanged from the earlier stage05 code):
##   - eruptions overlapping [year_min, year_max] by Start/End Year;
##   - start_date / end_date from Year-Month-Day; records with an incomplete
##     start or end date are dropped;
##   - for continuing eruptions (End Year Modifier ">") the end date is the
##     database "as of" date given in the record.
## =============================================================================

## data.table is used through its namespace (not attached) so that sourcing
## this file does not mask lubridate functions in the calling script.
suppressPackageStartupMessages(library(readxl))
if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' is required")

## Read the first table (sheet) whose header row contains all `need` columns;
## returns a data.table of text columns.
.gvp_read <- function(file, need) {
  for (sh in excel_sheets(file)) {
    probe <- suppressMessages(read_excel(file, sheet = sh, col_names = FALSE,
                                         n_max = 10, col_types = "text"))
    hit <- which(apply(probe, 1, function(r) all(need %in% r)))
    if (length(hit)) {
      dt <- data.table::as.data.table(suppressMessages(
        read_excel(file, sheet = sh, skip = hit[1] - 1L, col_types = "text")))
      for (cn in names(dt)) dt[get(cn) %in% c("NULL", ""), (cn) := NA_character_]
      return(dt)
    }
  }
  stop("No sheet in ", file, " has columns: ", paste(need, collapse = ", "))
}

.num <- function(x) suppressWarnings(as.numeric(x))
.int <- function(x) suppressWarnings(as.integer(x))

read_gvp_eruptions <- function(year_min, year_max,
                               file_erupt  = FILE_GVP_XLSX,
                               file_coords = if (exists("FILE_GVP_COORDS")) FILE_GVP_COORDS else NA) {

  if (!file.exists(file_erupt)) stop("GVP eruption file not found: ", file_erupt)

  er <- .gvp_read(file_erupt, c("Volcano Number", "Eruption Number", "Start Year"))

  if (!all(c("Latitude", "Longitude") %in% names(er))) {
    if (is.na(file_coords) || !file.exists(file_coords))
      stop("GVP eruption file has no coordinates and FILE_GVP_COORDS is missing: ",
           file_coords)
    co <- .gvp_read(file_coords, c("Volcano Number", "Latitude", "Longitude"))
    co <- unique(co[, .(`Volcano Number`, Latitude, Longitude)], by = "Volcano Number")
    er <- merge(er, co, by = "Volcano Number", all.x = TRUE, sort = FALSE)
  }

  er[, `:=`(
    Start_Year = .int(`Start Year`),  End_Year = .int(`End Year`),
    Latitude   = .num(Latitude),      Longitude = .num(Longitude)
  )]
  er <- er[!is.na(End_Year) & !is.na(Start_Year) &
             End_Year >= year_min & Start_Year <= year_max]

  er[, `:=`(
    ## explicit format: incomplete dates (e.g. "2014-NA-NA") become NA
    start_date = as.Date(sprintf("%04d-%02d-%02d", Start_Year,
                   .int(`Start Month`), .int(`Start Day`)), format = "%Y-%m-%d"),
    end_date   = as.Date(sprintf("%04d-%02d-%02d", End_Year,
                   .int(`End Month`),   .int(`End Day`)), format = "%Y-%m-%d")
  )]

  n_nodate  <- er[is.na(start_date) | is.na(end_date), .N]
  no_coord  <- er[is.na(Latitude) | is.na(Longitude), unique(`Volcano Name`)]
  er <- er[!is.na(start_date) & !is.na(end_date) & !is.na(Latitude) & !is.na(Longitude)]

  message(sprintf("GVP: %d eruptions overlap %d-%d with complete dates and coordinates",
                  nrow(er), year_min, year_max))
  if (n_nodate > 0)
    message(sprintf("GVP: %d eruptions dropped (incomplete start or end date)", n_nodate))
  if (length(no_coord) > 0)
    message(sprintf("GVP: no coordinates for %d volcano(es), dropped: %s%s",
                    length(no_coord), paste(head(sort(no_coord), 30), collapse = "; "),
                    if (length(no_coord) > 30) "; ..." else ""))

  er[, .(
    Volcano_Number    = .int(`Volcano Number`),
    Volcano_Name      = `Volcano Name`,
    Eruption_Number   = .int(`Eruption Number`),
    Eruption_Category = `Eruption Category`,
    VEI               = .int(VEI),
    Latitude, Longitude, start_date, end_date
  )]
}
