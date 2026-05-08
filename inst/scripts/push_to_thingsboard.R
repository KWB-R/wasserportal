#!/usr/bin/env Rscript
#
# Push a small demo selection of Wasserportal time series to a ThingsBoard
# tenant. Reads the daily surface-water-level ZIP that the pkgdown workflow
# publishes to the gh-pages branch every morning at 05:00 UTC.
#
# Designed to be invoked from GitHub Actions:
#   Rscript inst/scripts/push_to_thingsboard.R
#
# Required environment variables:
#   TB_HOST       e.g. https://eu.thingsboard.cloud
#   TB_API_KEY    Account-level API key (Bearer credential)
#
# Optional environment variables:
#   TB_STATION_IDS  Comma-separated Messstellennummer values to push.
#                   Defaults to five well-known Berlin surface water gauges.
#   TB_ZIP_URL      URL of the ZIP file to read.
#                   Default: https://kwb-r.github.io/wasserportal/daily_surface-water_water-level.zip
#   TB_DEVICE_PREFIX  Prefix for the ThingsBoard device names.
#                     Default: "wasserportal-"

stopifnot(
  nzchar(Sys.getenv("TB_HOST")),
  nzchar(Sys.getenv("TB_API_KEY"))
)

station_ids <- strsplit(
  Sys.getenv(
    "TB_STATION_IDS",
    "5803900,5805600,5867000,5826200,5824300"
  ),
  ","
)[[1L]]

zip_url <- Sys.getenv(
  "TB_ZIP_URL",
  "https://kwb-r.github.io/wasserportal/daily_surface-water_water-level.zip"
)

device_prefix <- Sys.getenv("TB_DEVICE_PREFIX", "wasserportal-")

message(sprintf(
  "Pushing %d station(s) from %s",
  length(station_ids), zip_url
))

tmp_dir <- tempfile("wasserportal-push-")
dir.create(tmp_dir)

zip_path <- file.path(tmp_dir, basename(zip_url))
utils::download.file(zip_url, zip_path, mode = "wb")

archive::archive_extract(zip_path, dir = tmp_dir)

csv_files <- list.files(tmp_dir, pattern = "\\.csv$", full.names = TRUE)
stopifnot(length(csv_files) == 1L)

water_level <- readr::read_csv(
  csv_files,
  show_col_types = FALSE,
  col_types = readr::cols(
    Messstellennummer = readr::col_character(),
    Datum             = readr::col_date(),
    Tagesmittelwert   = readr::col_double()
  )
)

water_level <- water_level[water_level$Messstellennummer %in% station_ids, ]

if (nrow(water_level) == 0L) {
  stop(
    "None of the configured station ids were found in the ZIP. ",
    "Check TB_STATION_IDS."
  )
}

device_tokens <- wasserportal::tb_setup_devices(
  station_ids = unique(water_level$Messstellennummer),
  name_prefix = device_prefix
)

for (station_id in names(device_tokens)) {
  one_station <- water_level[water_level$Messstellennummer == station_id, ]

  message(sprintf(
    "Station %s: %d daily values",
    station_id, nrow(one_station)
  ))

  wasserportal::tb_push_station_telemetry(
    data         = one_station,
    device_token = device_tokens[[station_id]],
    ts_col       = "Datum",
    value_col    = "Tagesmittelwert",
    key_col      = "Parameter"
  )
}

message("Done.")
