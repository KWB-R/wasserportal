#!/usr/bin/env Rscript
#
# Push a small demo selection of Wasserportal **groundwater** data to a
# ThingsBoard tenant. Reads the JSONs that the pkgdown workflow publishes
# to the gh-pages branch every morning at 05:00 UTC, so this script needs
# no Wasserportal scrape of its own.
#
# Demo focus: stations that have **both** groundwater level (gwl) and
# groundwater quality (gwq) measurements. When TB_STATION_IDS is unset,
# the five best candidates are picked automatically, scored as
#
#     score = (n_gwl_rows + n_gwq_rows) * n_distinct_gwq_parameters
#
# so stations with both long histories and many quality parameters win.
#
# Per device the script uploads
#   * combined master data (gwl + gwq) as ThingsBoard attributes,
#   * the gwl time series  (Parameter -> telemetry key, e.g. "GW-Stand"),
#   * the gwq time series  (Parameter -> telemetry key, e.g. "Nitrat").
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
#                     the top TB_MAX_DEVICES stations are picked using the
#                     score above.
#   TB_GH_PAGES_URL   Base URL where the data files are hosted.
#                     Default: https://kwb-r.github.io/wasserportal
#   TB_DEVICE_PREFIX  Prefix for ThingsBoard device names. Default
#                     "wasserportal-gw-".
#   TB_MAX_DEVICES    Maximum number of devices to set up. Default 5
#                     (ThingsBoard Cloud free tier limit).
#   TB_HISTORY_DAYS   Limit telemetry to the most recent N days per
#                     station. Default 0 (= push all history).

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

# Limit telemetry to the most recent N days per station. Set to 0 to push
# all history. Useful while diagnosing whether the ThingsBoard Cloud free
# tier silently rejects historical timestamps with HTTP 500.
history_days <- as.integer(Sys.getenv("TB_HISTORY_DAYS", "0"))

read_json <- function(path) {
  jsonlite::fromJSON(paste0(base_url, "/", path))
}

# Read a CSV that is published on gh-pages inside a ZIP file. Used for the
# groundwater time series, where the JSON files turn out to load as a
# nested structure that yields only a single Messstellennummer through
# `$Messstellennummer`. The ZIPs ship the same data as a flat CSV with a
# stable five-column schema (Messstellennummer, Datum, Parameter, Einheit,
# Messwert).
read_zip_csv <- function(zip_filename) {
  zip_url  <- paste0(base_url, "/", zip_filename)
  tmp_dir  <- tempfile("wasserportal-zip-")
  dir.create(tmp_dir)
  zip_path <- file.path(tmp_dir, basename(zip_filename))
  utils::download.file(zip_url, zip_path, mode = "wb", quiet = TRUE)
  archive::archive_extract(zip_path, dir = tmp_dir)
  csv_files <- list.files(tmp_dir, pattern = "\\.csv$", full.names = TRUE)
  stopifnot(length(csv_files) == 1L)
  readr::read_csv(
    csv_files[1L],
    show_col_types = FALSE,
    col_types = readr::cols(
      Messstellennummer = readr::col_character(),
      Datum             = readr::col_date(),
      Parameter         = readr::col_character(),
      Einheit           = readr::col_character(),
      Messwert          = readr::col_double()
    )
  )
}

# Convert ETRS89 / UTM zone 33N (EPSG:25833) -- the official CRS used by
# Wasserportal Berlin for Rechtswert_UTM_33_N / Hochwert_UTM_33_N -- to
# WGS84 longitude/latitude (EPSG:4326). ThingsBoard map widgets look for
# attributes named exactly "latitude" and "longitude".
utm33n_to_wgs84 <- function(easting, northing) {
  e <- suppressWarnings(as.numeric(easting))
  n <- suppressWarnings(as.numeric(northing))
  if (is.na(e) || is.na(n)) return(list(latitude = NA_real_, longitude = NA_real_))
  pt <- sf::st_sfc(sf::st_point(c(e, n)), crs = 25833L)
  ll <- sf::st_coordinates(sf::st_transform(pt, crs = 4326L))
  list(latitude = ll[1L, "Y"], longitude = ll[1L, "X"])
}

# ---- 1. master + data --------------------------------------------------------

message("Loading master data from gh-pages ...")
gwl_master <- read_json("stations_gwl_master.json")
gwq_master <- read_json("stations_gwq_master.json")

stopifnot("Nummer" %in% names(gwl_master), "Nummer" %in% names(gwq_master))
gwl_master$Nummer <- as.character(gwl_master$Nummer)
gwq_master$Nummer <- as.character(gwq_master$Nummer)

message("Loading time series from gh-pages ...")
gwl_data <- read_zip_csv("groundwater_level.zip")
gwq_data <- read_zip_csv("groundwater_quality.zip")

gwl_data$Messstellennummer <- as.character(gwl_data$Messstellennummer)
gwq_data$Messstellennummer <- as.character(gwq_data$Messstellennummer)
gwl_data$Messwert <- suppressWarnings(as.numeric(gwl_data$Messwert))
gwq_data$Messwert <- suppressWarnings(as.numeric(gwq_data$Messwert))
gwl_data <- gwl_data[!is.na(gwl_data$Messwert), , drop = FALSE]
gwq_data <- gwq_data[!is.na(gwq_data$Messwert), , drop = FALSE]

# ---- 2. station selection ---------------------------------------------------

env_ids <- Sys.getenv("TB_STATION_IDS", "")

if (nzchar(env_ids)) {
  station_ids <- strsplit(env_ids, ",")[[1L]]
  message(sprintf(
    "Using %d station id(s) from TB_STATION_IDS",
    length(station_ids)
  ))
} else {
  master_intersect <- intersect(gwl_master$Nummer, gwq_master$Nummer)
  with_gwl <- intersect(master_intersect, unique(gwl_data$Messstellennummer))
  with_both <- intersect(with_gwl, unique(gwq_data$Messstellennummer))

  message(sprintf(
    paste0(
      "Station counts:\n",
      "  gwl_master   = %d\n",
      "  gwq_master   = %d\n",
      "  master_intersect (both files)     = %d\n",
      "  + present in stations_gwl_data    = %d\n",
      "  + present in stations_gwq_data    = %d"
    ),
    length(unique(gwl_master$Nummer)),
    length(unique(gwq_master$Nummer)),
    length(master_intersect),
    length(with_gwl),
    length(with_both)
  ))

  both_ids <- with_both

  message(sprintf(
    "%d stations have both gwl and gwq data; scoring ...",
    length(both_ids)
  ))

  l_counts <- table(gwl_data$Messstellennummer)
  q_counts <- table(gwq_data$Messstellennummer)
  q_param_counts <- vapply(
    both_ids,
    function(id) length(unique(
      gwq_data$Parameter[gwq_data$Messstellennummer == id]
    )),
    integer(1L)
  )

  scoreboard <- data.frame(
    Nummer    = both_ids,
    n_gwl     = as.integer(l_counts[both_ids]),
    n_gwq     = as.integer(q_counts[both_ids]),
    n_q_param = q_param_counts,
    stringsAsFactors = FALSE
  )
  scoreboard$n_gwl[is.na(scoreboard$n_gwl)] <- 0L
  scoreboard$n_gwq[is.na(scoreboard$n_gwq)] <- 0L
  scoreboard$score <- with(
    scoreboard,
    (n_gwl + n_gwq) * pmax(n_q_param, 1L)
  )

  scoreboard <- scoreboard[order(-scoreboard$score), , drop = FALSE]
  picked <- utils::head(scoreboard, max_devices)

  message("Top candidates by (gwl_rows + gwq_rows) * gwq_parameters:")
  for (i in seq_len(nrow(picked))) {
    message(sprintf(
      "  %s : gwl=%d, gwq=%d, q-params=%d, score=%d",
      picked$Nummer[i], picked$n_gwl[i], picked$n_gwq[i],
      picked$n_q_param[i], picked$score[i]
    ))
  }

  station_ids <- picked$Nummer
}

if (length(station_ids) == 0L) {
  stop("No station ids selected. Set TB_STATION_IDS or check JSON inputs.")
}

# ---- 3. devices --------------------------------------------------------------

device_tokens <- wasserportal::tb_setup_devices(
  station_ids = station_ids,
  name_prefix = device_prefix
)

# ---- 4. attributes (master data) --------------------------------------------

# The gwl and gwq master JSONs share the same columns, so for stations that
# appear in both we push the gwl row and fall back to gwq only when a field
# is missing in gwl. Result: clean, unprefixed device attributes.
flatten_master_row <- function(row) {
  if (nrow(row) == 0L) return(list())
  row <- row[1L, , drop = FALSE]
  out <- list()
  for (col in names(row)) {
    val <- row[[col]]
    if (is.list(val) || is.data.frame(val)) next
    if (length(val) != 1L) next
    if (is.na(val) || identical(val, "")) next
    out[[col]] <- if (is.character(val)) as.character(val) else val
  }
  out
}

for (station_id in station_ids) {
  gwl_attrs <- flatten_master_row(
    gwl_master[gwl_master$Nummer == station_id, , drop = FALSE]
  )
  gwq_attrs <- flatten_master_row(
    gwq_master[gwq_master$Nummer == station_id, , drop = FALSE]
  )

  attrs <- c(list(Messstellennummer = station_id), gwl_attrs)
  for (key in names(gwq_attrs)) {
    if (is.null(attrs[[key]])) attrs[[key]] <- gwq_attrs[[key]]
  }

  ll <- utm33n_to_wgs84(
    attrs[["Rechtswert_UTM_33_N"]],
    attrs[["Hochwert_UTM_33_N"]]
  )
  if (!is.na(ll$latitude)) {
    attrs[["latitude"]]  <- ll$latitude
    attrs[["longitude"]] <- ll$longitude
  }

  message(sprintf(
    "  station %s: pushing %d attributes",
    station_id, length(attrs)
  ))

  wasserportal::tb_push_station_attributes(
    attributes   = attrs,
    device_token = device_tokens[[station_id]]
  )
}

# ---- 5. telemetry ------------------------------------------------------------

push_telemetry_subset <- function(data, label) {
  message(sprintf("\n=== %s ===", label))
  data <- data[data$Messstellennummer %in% station_ids, , drop = FALSE]

  if (history_days > 0L) {
    cutoff <- Sys.Date() - history_days
    before <- nrow(data)
    data <- data[as.Date(data$Datum) >= cutoff, , drop = FALSE]
    message(sprintf(
      "  TB_HISTORY_DAYS=%d: kept %d/%d rows (>= %s)",
      history_days, nrow(data), before, format(cutoff)
    ))
  }

  if (nrow(data) == 0L) {
    message("  no rows for the selected stations; skipped")
    return(0L)
  }

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
  pushed
}

total <- 0L
total <- total + push_telemetry_subset(gwl_data, "groundwater level")
total <- total + push_telemetry_subset(gwq_data, "groundwater quality")

message(sprintf("\nDone. Pushed %d data points total.", total))
