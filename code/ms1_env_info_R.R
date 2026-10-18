## ms1_env_info_R.R
## Run in MC0617: Rscript ms1_env_info_R.R
## Prints the R version, package versions (README list), the figure font, and
## the size of the two US daily classified files.

library(data.table)

cat("============================================================\n")
cat("MS1 Environment Info — R\n")
cat("============================================================\n\n")

## ── 1. R version and platform ─────────────────────────────────
cat("── R Version ──\n")
cat(R.version.string, "\n")
cat(sessionInfo()$running, "\n\n")

## ── 2. R package versions ─────────────────────────────────────
cat("── R Package Versions ──\n")
r_pkgs <- c(
  "data.table", "R.utils", "dplyr", "tidyr", "readr", "lubridate",
  "stringr", "sf", "readxl", "fuzzyjoin", "tigris", "lutz",
  "rnaturalearth", "rnaturalearthdata", "elevatr", "terra",
  "ggplot2", "cowplot", "svglite", "ragg", "systemfonts", "tidyverse"
)
for (p in r_pkgs) {
  ver <- tryCatch(as.character(packageVersion(p)), error = function(e) "not installed")
  cat(sprintf("  %-18s %s\n", p, ver))
}
cat("\n")

## ── 3. Figure font (ms1_fig2 / ms1_fig3) ──────────────────────
cat("── Figure Font ──\n")
fams <- tryCatch(unique(systemfonts::system_fonts()$family), error = function(e) character())
user_arial <- file.exists(path.expand(file.path(Sys.getenv("MS1_FONT_DIR", "~/.fonts"), "arial.ttf")))
cat(sprintf("  Arial installed: %s | arial.ttf in font folder: %s | Liberation Sans: %s\n\n",
            "Arial" %in% fams, user_arial, "Liberation Sans" %in% fams))

## ── 4. Memory estimates ──────────────────────────────────────
cat("── File Sizes & Memory ──\n")

source("config_pa.R")   # also sources config.R

files <- c(
  airnow    = FILE_DAILY_INTEGRATED,
  purpleair = FILE_PA_DAILY_INTEGRATED_OUTDOOR
)

for (nm in names(files)) {
  f <- files[[nm]]
  if (!file.exists(f)) {
    cat(sprintf("  %s: file not found (%s)\n", nm, f))
    next
  }
  fsize_mb <- file.info(f)$size / 1024^2
  cat(sprintf("  %s compressed: %.0f MB\n", nm, fsize_mb))

  dt <- fread(f)
  mem_gb <- as.numeric(object.size(dt)) / 1024^3
  cat(sprintf("  %s in-memory (data.table): %.2f GB\n", nm, mem_gb))
  cat(sprintf("  %s rows: %s  cols: %d\n\n", nm, format(nrow(dt), big.mark = ","), ncol(dt)))
  rm(dt); gc(verbose = FALSE)
}

cat("============================================================\n")
