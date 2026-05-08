#!/usr/bin/env Rscript
#
# Push a small demo selection of Wasserportal time series to a ThingsBoard
# tenant. Reads the daily surface-water ZIP files that the pkgdown workflow
# publishes to the gh-pages branch every morning at 05:00 UTC, so this
# script needs no Wasserportal scrape of its own.
#
# By default the seven daily surface-water parameter files are pulled and
# pushed for the same set of demo stations, so each ThingsBoard device ends
# up with multiple telemetry keys (e.g. "Wasserstand", "Abfluss",
# "Wassertemperatur", ...).
#
# Designed to be invoked from GitHub Actions:
#   Rscript inst/scripts/push_to_thingsboard.R
#
# Required environment variables:
#   TB_HOST       e.g. https://eu.thingsboard.cloud
#   TB_API_KEY    Account-level API key (Bearer credential)
#
# Optional environment variables:
#   TB_STATION_IDS    Comma-separated Messstellennummer values to push.
#                     Defaults to five well-known Berlin surface water gauges.
#   TB_GH_PAGES_URL   Base URL where the daily ZIPs are hosted.
#                     Default: https://kwb-r.github.io/wasserportal
#   TB_ZIP_FILES      Comma-separated list of ZIP file names under
#                     TB_GH_PAGES_URL. Default: all seven daily surface-water
#                     parameter ZIPs.
#   TB_DEVICE_PREFIX  Prefix for ThingsBoard device names. Default
#                     "wasserportal-".

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

base_url <- sub(
  "/+$", "",
  Sys.getenv("TB_GH_PAGES_URL", "https://kwb-r.github.io/wasserportal")
)

zip_files <- strsplit(
  Sys.getenv(
    "TB_ZIP_FILES",
    paste(
      "daily_surface-water_water-level.zip",
      "daily_surface-water_flow.zip",
      "daily_surface-water_temperature.zip",
      "daily_surface-water_conductivity.zip",
      "daily_surface-water_ph.zip",
      "daily_surface-water_oxygen-concentration.zip",
      "daily_surface-water_oxygen-saturation.zip",
      sep = ","
    )
  ),
  ","
)[[1L]]

device_prefix <- Sys.getenv("TB_DEVICE_PREFIX", "wasserportal-")

message(sprintf(
  "Pushing %d station(s) from %d ZIP file(s) at %s",
  length(station_ids), length(zip_files), base_url
))

device_tokens <- wasserportal::tb_setup_devices(
  station_ids = station_ids,
  name_prefix = device_prefix
)

read_zip_to_long <- function(zip_url) {
  tmp_dir <- tempfile("wasserportal-push-")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  zip_path <- file.path(tmp_dir, basename(zip_url))
  utils::download.file(zip_url, zip_path, mode = "wb", quiet = TRUE)
  archive::archive_extract(zip_path, dir = tmp_dir)

  csv_files <- list.files(tmp_dir, pattern = "\\.csv$", full.names = TRUE)
  stopifnot(length(csv_files) == 1L)

  readr::read_csv(
    csv_files,
    show_col_types = FALSE,
    col_types = readr::cols(
      Messstellennummer = readr::col_character(),
      Datum             = readr::col_date(),
      Tagesmittelwert   = readr::col_double(),
      Parameter         = readr::col_character(),
      .default          = readr::col_character()
    )
  )
}

total_points <- 0L

for (zip_file in zip_files) {
  zip_url <- paste0(base_url, "/", zip_file)
  message(sprintf("\n=== %s ===", zip_file))

  data <- tryCatch(
    read_zip_to_long(zip_url),
    error = function(e) {
      message(sprintf("  skipped: %s", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(data)) next

  data <- data[data$Messstellennummer %in% station_ids, ]
  if (nrow(data) == 0L) {
    message("  no rows for the configured stations; skipped")
    next
  }

  for (station_id in unique(data$Messstellennummer)) {
    one_station <- data[data$Messstellennummer == station_id, ]

    message(sprintf(
      "  station %s: %d values",
      station_id, nrow(one_station)
    ))

    wasserportal::tb_push_station_telemetry(
      data         = one_station,
      device_token = device_tokens[[station_id]],
      ts_col       = "Datum",
      value_col    = "Tagesmittelwert",
      key_col      = "Parameter",
      verbose      = FALSE
    )
    total_points <- total_points + nrow(one_station)
  }
}

message(sprintf("\nDone. Pushed %d data points total.", total_points))
