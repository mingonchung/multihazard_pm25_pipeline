## =============================================================================
## acq_airnow_hourly_combine.R
## Combine individual AirNow hourly .dat files into per-parameter CSV files.
##
## Reads all HourlyData_YYYYMMDDhh.dat files recursively from DIR_AIRNOW_HOURLY,
## splits by Parameter, and appends each parameter's records to a single
## compressed CSV (e.g., airnow_PM2.5_2014_2025.csv.gz). PM2.5 is the file
## used downstream (FILE_AIRNOW_PM25 in config.R).
##
## Date and Time in the hourly files are GMT (used as UTC), hour-beginning.
## GMT_Offset is kept as reported by AirNow; stage06 replaces it with the
## standard offset of the time zone at the site coordinates.
##
## Output: DIR_AIRNOW_HOURLY/combine/airnow_<PARAM>_2014_2025.csv.gz
##
## Not re-run safe by design: records are appended to the output files, so the
## script stops if any combined file already exists. Move or delete the old
## files before re-running.
## =============================================================================

rm(list = ls(all.names = TRUE))

library(readr)

source("config.R")

input_base  <- DIR_AIRNOW_HOURLY
output_base <- file.path(DIR_AIRNOW_HOURLY, "combine")

if (!dir.exists(output_base)) dir.create(output_base, recursive = TRUE)

existing <- list.files(output_base, pattern = "^airnow_.+_2014_2025\\.csv\\.gz$",
                       full.names = TRUE)
if (length(existing) > 0)
  stop(length(existing), " combined file(s) already exist in ", output_base,
       " (e.g. ", basename(existing[1]), ").\n",
       "This script appends to them; move or delete them before re-running.")

header <- c("Date", "Time", "AQSID", "SiteName", "GMT_Offset",
            "Parameter", "Units", "Value", "Source")

dat_files <- list.files(path = input_base, pattern = "^HourlyData_\\d{10}\\.dat$",
                        recursive = TRUE, full.names = TRUE)

for (file in dat_files) {
  df <- read_delim(file, delim = "|", col_names = FALSE,
                   col_types = cols(.default = col_character()),
                   trim_ws = TRUE, na = c("", "NA"))
  colnames(df) <- header
  df$AQSID <- paste0("'", df$AQSID)

  param_list <- split(df, df$Parameter)

  for (param in names(param_list)) {
    out_file <- file.path(output_base, paste0("airnow_", param, "_2014_2025.csv.gz"))
    if (file.exists(out_file)) {
      con <- gzfile(out_file, open = "at")
      write.table(param_list[[param]], file = con, sep = ",",
                  row.names = FALSE, col.names = FALSE, quote = TRUE)
      close(con)
    } else {
      con <- gzfile(out_file, open = "wt")
      write.table(param_list[[param]], file = con, sep = ",",
                  row.names = FALSE, col.names = TRUE, quote = TRUE)
      close(con)
    }
  }
  print(file)
}
