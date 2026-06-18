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
#   TB_HOST       e.g. https://eu.thingsboard.cloud or
#                 https://dashboards.inowas.org (self-hosted)
#
# Authentication -- provide ONE of the following (username/password wins):
#   TB_USERNAME + TB_PASSWORD  ThingsBoard login -> JWT Bearer token. Works
#                 on every edition and is the ONLY option for self-hosted
#                 Community Edition (which has no account-level API keys).
#                 For self-hosted instances also set TB_PLAN=ce.
#   TB_API_KEY    Account-level API key, ThingsBoard Cloud only
#                 (Account > Security > API keys > Generate).
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
#                     (ThingsBoard Cloud free tier limit). Set to 0 for
#                     no limit (push every candidate station).
#   TB_STATION_SCOPE  Which groundwater stations the auto-pick considers
#                     (ignored when TB_STATION_IDS is set): "both"
#                     (default) = stations with level AND quality data;
#                     "any" = level OR quality; "gwl" / "gwq" = has that
#                     series (may also have the other); "gwl-only" /
#                     "gwq-only" = has ONLY that series (excludes
#                     both-stations).
#   TB_HISTORY_DAYS   Limit telemetry to the most recent N days per
#                     station. Default 0 (= push all history).
#   TB_PLAN           One of "free" (default), "prototype", "pilot",
#                     "startup", "business" or "ce" (self-hosted).
#                     Selects the push tunables (mode, chunk_size,
#                     throttle_seconds) via tb_plan_defaults() to stay
#                     within the per-device transport rate limits
#                     documented at
#                     https://thingsboard.io/docs/paas/eu/subscriptions/.
#   TB_TELEMETRY_MODE Override the plan-derived mode ("single" /
#                     "bulk"). Free rejects bulk arrays.
#   TB_CHUNK_SIZE     Override the plan-derived chunk size for bulk.
#   TB_THROTTLE_SECONDS Override the plan-derived inter-request sleep.
#   TB_TELEMETRY_TYPES Comma-separated subset of "gwl,gwq" (default
#                     both). Useful to skip the slow gwl re-push when
#                     iterating only on the gwq fix.

# Helper: Sys.getenv() returns "" (not the default) when the variable is
# set to an empty string -- which happens whenever a workflow_dispatch
# input is left blank or an unset secret is interpolated by GitHub
# Actions. env_or() falls back to `default` for both unset and empty.
env_or <- function(name, default) {
  v <- Sys.getenv(name, unset = "")
  if (nzchar(v)) v else default
}

if (!nzchar(Sys.getenv("TB_HOST"))) {
  stop(paste0(
    "TB_HOST is required (e.g. https://eu.thingsboard.cloud or ",
    "https://dashboards.inowas.org)."
  ))
}

# Authenticate with either a username/password login (JWT -- works on every
# ThingsBoard edition, required for self-hosted Community Edition) or an
# account-level API key (ThingsBoard Cloud only). Username/password wins.
# tb_setup_devices() resolves the same TB_USERNAME / TB_PASSWORD / TB_API_KEY
# env vars itself; this is just a fail-fast preflight with a clear message.
has_login   <- nzchar(Sys.getenv("TB_USERNAME")) &&
  nzchar(Sys.getenv("TB_PASSWORD"))
has_api_key <- nzchar(Sys.getenv("TB_API_KEY"))
if (!has_login && !has_api_key) {
  stop(paste0(
    "No ThingsBoard credentials. Set TB_USERNAME + TB_PASSWORD ",
    "(self-hosted / Community Edition) or TB_API_KEY (ThingsBoard Cloud)."
  ))
}
message(sprintf(
  "ThingsBoard auth: %s",
  if (has_login) "username/password (JWT Bearer)" else "account API key"
))

base_url <- sub("/+$", "", env_or(
  "TB_GH_PAGES_URL", "https://kwb-r.github.io/wasserportal"
))

device_prefix <- env_or("TB_DEVICE_PREFIX", "wasserportal-gw-")

# Maximum number of devices/stations to set up. 0 (or negative) = no limit
# (push every candidate station -- only sensible on self-hosted / paid tiers).
max_devices <- as.integer(env_or("TB_MAX_DEVICES", "5"))

# Candidate pool for the auto-pick (used only when TB_STATION_IDS is unset):
# "both" (default) = level AND quality; "any" = level OR quality;
# "gwl" / "gwq" = has that series (may also have the other);
# "gwl-only" / "gwq-only" = has only that series (excludes both-stations).
station_scope <- tolower(env_or("TB_STATION_SCOPE", "both"))

# Limit telemetry to the most recent N days per station. Set to 0 to push
# all history. Useful while diagnosing whether the ThingsBoard Cloud free
# tier silently rejects historical timestamps with HTTP 500.
history_days <- as.integer(env_or("TB_HISTORY_DAYS", "0"))

# Resolve push tunables from the ThingsBoard plan via tb_plan_defaults().
# TB_PLAN takes precedence; individual TB_TELEMETRY_MODE / TB_CHUNK_SIZE /
# TB_THROTTLE_SECONDS env vars override the plan-derived defaults if set,
# so e.g. a Free user can still test bulk mode by setting both
# TB_PLAN=free and TB_TELEMETRY_MODE=bulk.
plan_defaults <- wasserportal::tb_plan_defaults(
  env_or("TB_PLAN", "free")
)
telemetry_mode <- env_or("TB_TELEMETRY_MODE", plan_defaults$mode)
chunk_size <- as.integer(env_or(
  "TB_CHUNK_SIZE",
  as.character(plan_defaults$chunk_size)
))
throttle_seconds <- as.numeric(env_or(
  "TB_THROTTLE_SECONDS",
  as.character(plan_defaults$throttle_seconds)
))
plan_max_active <- if (is.null(plan_defaults$max_active)) 10L else
  plan_defaults$max_active
max_active <- as.integer(env_or(
  "TB_MAX_ACTIVE", as.character(plan_max_active)
))

message(sprintf(
  paste0(
    "Push tunables: plan='%s', mode='%s', chunk_size=%d, ",
    "throttle_seconds=%g, max_active=%d"
  ),
  env_or("TB_PLAN", "free"),
  telemetry_mode, chunk_size, throttle_seconds, max_active
))

# Which telemetry datasets to push. Default both. Set to "gwl" or "gwq"
# only to skip a long retry after a partial success.
telemetry_types <- strsplit(
  env_or("TB_TELEMETRY_TYPES", "gwl,gwq"),
  ","
)[[1L]]

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
  master_union     <- union(gwl_master$Nummer, gwq_master$Nummer)
  ids_gwl <- unique(gwl_data$Messstellennummer)
  ids_gwq <- unique(gwq_data$Messstellennummer)

  # Candidate pool depends on TB_STATION_SCOPE (default "both" -- the proven
  # demo set). "gwl"/"gwq" = has that series (may also have the other);
  # "gwl-only"/"gwq-only" = has ONLY that series (excludes both-stations).
  # The push still honours TB_TELEMETRY_TYPES per station, so a gwl-only
  # station picked under "any"/"gwl" simply contributes no gwq rows.
  candidate_ids <- switch(
    station_scope,
    both       = intersect(intersect(master_intersect, ids_gwl), ids_gwq),
    any        = intersect(master_union, union(ids_gwl, ids_gwq)),
    gwl        = intersect(master_union, ids_gwl),
    gwq        = intersect(master_union, ids_gwq),
    `gwl-only` = setdiff(intersect(master_union, ids_gwl), ids_gwq),
    `gwq-only` = setdiff(intersect(master_union, ids_gwq), ids_gwl),
    stop(sprintf(
      paste0(
        "Unknown TB_STATION_SCOPE '%s' ",
        "(use both | any | gwl | gwq | gwl-only | gwq-only)."
      ),
      station_scope
    ))
  )

  message(sprintf(
    paste0(
      "Station selection (TB_STATION_SCOPE='%s'):\n",
      "  with gwl data     = %d\n",
      "  with gwq data     = %d\n",
      "  with gwl AND gwq  = %d\n",
      "  only gwl (no gwq) = %d\n",
      "  only gwq (no gwl) = %d\n",
      "  -> candidate pool = %d"
    ),
    station_scope,
    length(intersect(master_union, ids_gwl)),
    length(intersect(master_union, ids_gwq)),
    length(intersect(intersect(master_intersect, ids_gwl), ids_gwq)),
    length(setdiff(intersect(master_union, ids_gwl), ids_gwq)),
    length(setdiff(intersect(master_union, ids_gwq), ids_gwl)),
    length(candidate_ids)
  ))

  if (length(candidate_ids) == 0L) {
    stop(sprintf(
      "No candidate stations for TB_STATION_SCOPE='%s'.", station_scope
    ))
  }

  l_counts <- table(gwl_data$Messstellennummer)
  q_counts <- table(gwq_data$Messstellennummer)
  # Distinct gwq parameters per station, computed once via tapply. The old
  # per-id vapply rescanned the whole gwq table for every candidate, which
  # got slow once the pool grew from ~170 to several hundred stations.
  q_param_by_id <- tapply(
    gwq_data$Parameter, gwq_data$Messstellennummer,
    function(p) length(unique(p))
  )

  scoreboard <- data.frame(
    Nummer    = candidate_ids,
    n_gwl     = as.integer(l_counts[candidate_ids]),
    n_gwq     = as.integer(q_counts[candidate_ids]),
    n_q_param = as.integer(q_param_by_id[candidate_ids]),
    stringsAsFactors = FALSE
  )
  scoreboard$n_gwl[is.na(scoreboard$n_gwl)] <- 0L
  scoreboard$n_gwq[is.na(scoreboard$n_gwq)] <- 0L
  scoreboard$n_q_param[is.na(scoreboard$n_q_param)] <- 0L
  scoreboard$score <- with(
    scoreboard,
    (n_gwl + n_gwq) * pmax(n_q_param, 1L)
  )

  scoreboard <- scoreboard[order(-scoreboard$score), , drop = FALSE]

  # TB_MAX_DEVICES = 0 (or negative) means "no limit": push every candidate.
  no_limit <- is.na(max_devices) || max_devices <= 0L
  picked <- if (no_limit) scoreboard else utils::head(scoreboard, max_devices)

  message(sprintf(
    "Pushing %d of %d candidate stations (TB_MAX_DEVICES=%s).",
    nrow(picked), nrow(scoreboard),
    if (no_limit) "0 -> all" else as.character(max_devices)
  ))
  message("Top stations by (gwl_rows + gwq_rows) * gwq_parameters:")
  for (i in seq_len(min(nrow(picked), 10L))) {
    message(sprintf(
      "  %s : gwl=%d, gwq=%d, q-params=%d, score=%d",
      picked$Nummer[i], picked$n_gwl[i], picked$n_gwq[i],
      picked$n_q_param[i], picked$score[i]
    ))
  }
  if (nrow(picked) > 10L) {
    message(sprintf("  ... and %d more", nrow(picked) - 10L))
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

# Note: earlier revisions of this script ran a "smoke test" before the bulk
# telemetry push that posted a single value per station via
# tb_push_latest_telemetry() ({"key": value}; the server stamped the
# current wall-clock time). The intent was fail-fast on Maker-tier
# auth/payload issues, but the side effect was a stale "GW-Stand =
# <whatever was last historically> @ <push time>" row that drowned out the
# real most-recent measurement in the device's "Latest telemetry" view.
# The bulk push fails fast on its own first POST, so the smoke test has
# been removed. If you ever need to clear telemetry for one of the demo
# devices, do it interactively in the ThingsBoard UI (Device > Latest
# telemetry > tick the row(s) > trash icon) or call
# `wasserportal::tb_delete_device_telemetry()` from an R session against
# the device UUID returned by `wasserportal::tb_get_device_id()`.

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
      data             = one,
      device_token     = device_tokens[[station_id]],
      max_active       = max_active,
      ts_col           = "Datum",
      value_col        = "Messwert",
      key_col          = "Parameter",
      mode             = telemetry_mode,
      chunk_size       = chunk_size,
      throttle_seconds = throttle_seconds,
      verbose          = TRUE
    )
    pushed <- pushed + nrow(one)
  }
  pushed
}

total <- 0L
if ("gwl" %in% telemetry_types) {
  total <- total + push_telemetry_subset(gwl_data, "groundwater level")
} else {
  message("\nSkipping groundwater level (TB_TELEMETRY_TYPES does not include 'gwl').")
}
if ("gwq" %in% telemetry_types) {
  total <- total + push_telemetry_subset(gwq_data, "groundwater quality")
} else {
  message("\nSkipping groundwater quality (TB_TELEMETRY_TYPES does not include 'gwq').")
}

message(sprintf("\nDone. Pushed %d data points total.", total))
