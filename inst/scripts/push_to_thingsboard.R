#!/usr/bin/env Rscript
#
# Push a small demo selection of Wasserportal **groundwater** data to a
# ThingsBoard tenant. Reads the JSONs and ZIPs that the pkgdown workflow
# publishes to the gh-pages branch every morning at 05:00 UTC, so this
# script needs no Wasserportal scrape of its own.
#
# Demo focus: stations that have **both** groundwater level (gwl) and
# groundwater quality (gwq) measurements. Per device the script uploads
#
#   * combined master data (gwl + gwq) as ThingsBoard attributes,
#   * the gwl time series  (key per Parameter, e.g. "Grundwasserstand"),
#   * the gwq time series  (key per Parameter, e.g. "Nitrat", "Chlorid").
#
# Designed to be invoked from GitHub Actions:
#   Rscript inst/scripts/push_to_thingsboard.R
#
# Required environment variables:
#   TB_HOST       e.g. https://eu.thingsboard.cloud
#   TB_API_KEY    Account-level API key (Bearer credential)
#
# Optional environment variables:
#   TB_STATION_IDS    Comma-separated Messstellennummer values. If unset,
#                     the first five stations that occur in **both** master
#                     JSONs are picked automatically.
#   TB_GH_PAGES_URL   Base URL where the data files are hosted.
#                     Default: https://kwb-r.github.io/wasserportal
#   TB_DEVICE_PREFIX  Prefix for ThingsBoard device names. Default
#                     "wasserportal-gw-".
#   TB_MAX_DEVICES    Maximum number of devices to set up when station ids
#                     are auto-selected. Default 5 (ThingsBoard Cloud free
#                     tier limit).

stopifnot(
  nzchar(Sys.getenv("TB_HOST")),
  nzchar(Sys.getenv("TB_API_KEY"))
)

base_url <- sub(
  "/+$", "",
  Sys.getenv("TB_GH_PAGES_URL", "https://kwb-r.github.io/wasserportal")
)

device_prefix <- Sys.getenv("TB_DEVICE_PREFIX", "wasserportal-gw-")

max_devices <- as.integer(Sys.getenv("TB_MAX_DEVICES", "5"))

# ---- 1. master data ----------------------------------------------------------

read_json <- function(path) {
  jsonlite::fromJSON(paste0(base_url, "/", path))
}

message("Loading master data from gh-pages ...")
gwl_master <- read_json("stations_gwl_master.json")
gwq_master <- read_json("stations_gwq_master.json")

stopifnot("Nummer" %in% names(gwl_master), "Nummer" %in% names(gwq_master))

gwl_master$Nummer <- as.character(gwl_master$Nummer)
gwq_master$Nummer <- as.character(gwq_master$Nummer)

both_ids <- intersect(gwl_master$Nummer, gwq_master$Nummer)
message(sprintf(
  "%d stations have both gwl and gwq master data",
  length(both_ids)
))

env_ids <- Sys.getenv("TB_STATION_IDS", "")
station_ids <- if (nzchar(env_ids)) {
  strsplit(env_ids, ",")[[1L]]
} else {
  utils::head(both_ids, max_devices)
}

if (length(station_ids) == 0L) {
  stop("No station ids selected. Set TB_STATION_IDS or check master JSONs.")
}

message(sprintf(
  "Pushing %d station(s): %s",
  length(station_ids), paste(station_ids, collapse = ", ")
))

# ---- 2. devices --------------------------------------------------------------

device_tokens <- wasserportal::tb_setup_devices(
  station_ids = station_ids,
  name_prefix = device_prefix
)

# ---- 3. attributes (master data) --------------------------------------------

# ThingsBoard attributes are key/value pairs. Drop list-typed columns and NA
# values, coerce to character/numeric, prefix to keep gwl and gwq separate.
flatten_master_row <- function(row, prefix) {
  if (nrow(row) == 0L) return(list())
  row <- row[1L, , drop = FALSE]
  out <- list()
  for (col in names(row)) {
    val <- row[[col]]
    if (is.list(val) || is.data.frame(val)) next
    if (length(val) != 1L) next
    if (is.na(val) || identical(val, "")) next
    out[[paste0(prefix, col)]] <- if (is.character(val)) as.character(val) else val
  }
  out
}

for (station_id in station_ids) {
  attrs <- c(
    list(Messstellennummer = station_id),
    flatten_master_row(
      gwl_master[gwl_master$Nummer == station_id, , drop = FALSE],
      prefix = "level."
    ),
    flatten_master_row(
      gwq_master[gwq_master$Nummer == station_id, , drop = FALSE],
      prefix = "quality."
    )
  )

  message(sprintf(
    "  station %s: pushing %d attributes",
    station_id, length(attrs)
  ))

  wasserportal::tb_push_station_attributes(
    attributes   = attrs,
    device_token = device_tokens[[station_id]]
  )
}

# ---- 4. telemetry (gwl + gwq) -----------------------------------------------

push_long_json <- function(json_path, label) {
  message(sprintf("\n=== %s (%s) ===", json_path, label))

  data <- tryCatch(
    read_json(json_path),
    error = function(e) {
      message(sprintf("  skipped: %s", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(data) || nrow(data) == 0L) return(invisible(0L))

  data$Messstellennummer <- as.character(data$Messstellennummer)
  data <- data[data$Messstellennummer %in% station_ids, , drop = FALSE]

  if (nrow(data) == 0L) {
    message("  no rows for the selected stations; skipped")
    return(invisible(0L))
  }

  data$Messwert <- suppressWarnings(as.numeric(data$Messwert))
  data <- data[!is.na(data$Messwert), , drop = FALSE]

  pushed <- 0L
  for (station_id in unique(data$Messstellennummer)) {
    one <- data[data$Messstellennummer == station_id, , drop = FALSE]

    message(sprintf("  station %s: %d values", station_id, nrow(one)))

    wasserportal::tb_push_station_telemetry(
      data         = one,
      device_token = device_tokens[[station_id]],
      ts_col       = "Datum",
      value_col    = "Messwert",
      key_col      = "Parameter",
      verbose      = FALSE
    )
    pushed <- pushed + nrow(one)
  }

  invisible(pushed)
}

total <- 0L
total <- total + push_long_json("stations_gwl_data.json", "groundwater level")
total <- total + push_long_json("stations_gwq_data.json", "groundwater quality")

message(sprintf("\nDone. Pushed %d data points total.", total))
