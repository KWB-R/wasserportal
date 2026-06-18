# tb_default_host --------------------------------------------------------------

# Resolve TB_HOST with empty-string-aware fallback. Unlike
# Sys.getenv("TB_HOST", unset = "..."), this also treats TB_HOST="" (set but
# empty in .Renviron or a blank workflow_dispatch input) as unset and returns
# the public-cloud default, so users don't silently hit
# https://thingsboard.cloud after blanking the var.
tb_default_host <- function()
{
  host <- Sys.getenv("TB_HOST")
  if (nzchar(host)) host else "https://thingsboard.cloud"
}

# tb_push_station_telemetry ----------------------------------------------------

#' Push Time Series of one Wasserportal Station to ThingsBoard
#'
#' Sends long-format measurements of a single Wasserportal monitoring station
#' to the device telemetry endpoint of a ThingsBoard instance
#' (`/api/v1/{token}/telemetry`). Works against ThingsBoard Cloud
#' (e.g. the free Maker tier on `https://thingsboard.cloud`),
#' self-hosted ThingsBoard Community Edition and `https://demo.thingsboard.io`
#' since the device-token API is identical on all of them.
#'
#' Long-format input is pivoted on the fly so that every distinct value of
#' `key_col` becomes one telemetry key inside ThingsBoard, sharing the same
#' timestamp.
#'
#' @param data data frame for **one** station, in the long format produced by
#'   \code{\link{get_groundwater_data}} (columns `Messstellennummer`, `Datum`,
#'   `Parameter`, `Einheit`, `Messwert`) or
#'   \code{\link{get_daily_surfacewater_data}} (`Messstellennummer`, `Datum`,
#'   `Tagesmittelwert`, `Parameter`, `Einheit`).
#' @param device_token ThingsBoard device access token (taken from the device
#'   detail view in the ThingsBoard UI).
#' @param ts_col name of the timestamp column. Default `"Datum"`.
#' @param value_col name of the numeric value column. Default `"Messwert"` (set
#'   to `"Tagesmittelwert"` for surface water data).
#' @param key_col name of the column whose values become telemetry keys.
#'   Default `"Parameter"`. Set to `NULL` to push `value_col` itself under a
#'   single fixed key (see `single_key`).
#' @param single_key telemetry key used when `key_col` is `NULL`. Default
#'   `"value"`.
#' @param host base URL of the ThingsBoard instance, without trailing slash.
#'   Defaults to env var `TB_HOST` if set, otherwise
#'   `"https://thingsboard.cloud"`.
#' @param chunk_size maximum number of timestamps per HTTP POST when
#'   `mode = "bulk"`. Default `100`. Ignored in single mode.
#' @param mode one of `"single"` (default) or `"bulk"`. The Maker free
#'   tier on ThingsBoard Cloud rejects the bulk array form with an
#'   opaque HTTP 500 even though the same device accepts the per-record
#'   `{"ts": ms, "values": {...}}` object; single mode therefore POSTs
#'   each record on its own. Use `"bulk"` against self-hosted CE for
#'   the much faster array-of-records form. See
#'   \code{\link{tb_plan_defaults}} for plan-aware presets that pick
#'   `mode`, `chunk_size` and `throttle_seconds` together.
#' @param throttle_seconds inter-request sleep, in seconds, between
#'   consecutive HTTP POSTs. `NULL` (default) picks `0.05` for
#'   `mode = "single"` and `0.1` for `mode = "bulk"`. Increase to stay
#'   safely below the per-second / per-minute transport rate limits of
#'   the target ThingsBoard plan; set to `0` to push as fast as the
#'   server permits (e.g. self-hosted CE).
#' @param max_active number of concurrent HTTP POSTs in single mode
#'   (passed to `httr2::req_perform_parallel()`). Default `10`, which
#'   stays below the ThingsBoard Cloud Free tier's 50 messages/second
#'   per-device transport rate limit. Ignored in bulk mode.
#' @param verbose print one line per chunk in bulk mode and one line
#'   per parallel batch in single mode (default `TRUE`).
#' @return invisibly the number of telemetry timestamps that were sent.
#' @export
#' @examples
#' \dontrun{
#' stations <- wasserportal::get_stations()
#' gw <- wasserportal::get_groundwater_data(stations)
#' one_station <- dplyr::filter(
#'   gw$groundwater.level,
#'   .data$Messstellennummer == "149"
#' )
#' tb_push_station_telemetry(
#'   data = one_station,
#'   device_token = Sys.getenv("TB_DEVICE_TOKEN_149")
#' )
#' }
tb_push_station_telemetry <- function(
    data,
    device_token,
    ts_col = "Datum",
    value_col = "Messwert",
    key_col = "Parameter",
    single_key = "value",
    host = tb_default_host(),
    chunk_size = 100L,
    mode = c("single", "bulk"),
    throttle_seconds = NULL,
    max_active = 10L,
    verbose = TRUE
)
{
  mode <- match.arg(mode)
  if (is.null(throttle_seconds)) {
    throttle_seconds <- if (mode == "single") 0.05 else 0.1
  }
  throttle_seconds <- max(0, as.numeric(throttle_seconds))
  max_active <- max(1L, as.integer(max_active))

  stopifnot(
    is.data.frame(data),
    nzchar(device_token),
    nzchar(host),
    ts_col %in% names(data),
    value_col %in% names(data),
    is.null(key_col) || key_col %in% names(data)
  )

  if (nrow(data) == 0L) {
    if (verbose) message("No rows to push.")
    return(invisible(0L))
  }

  payload <- build_telemetry_payload(
    data = data,
    ts_col = ts_col,
    value_col = value_col,
    key_col = key_col,
    single_key = single_key,
    # In single mode keep one (ts, key, value) per record -- otherwise
    # ThingsBoard Cloud Maker rejects the "fat" values dict produced
    # when many parameters share a timestamp (groundwater quality
    # often has 30+ analytes per sampling event) with an opaque
    # empty-body HTTP 500. In bulk mode keep grouping so each chunk
    # POST stays compact.
    group_by_ts = mode != "single"
  )

  url <- sprintf("%s/api/v1/%s/telemetry", sub("/+$", "", host), device_token)

  n <- length(payload)

  if (mode == "single") {
    # Send each record as a standalone `{"ts": ms, "values": {...}}` object,
    # one per HTTP POST. The bulk array format is rejected on ThingsBoard
    # Cloud Maker; this mode is the only reliable shape there.
    #
    # Sequential single POSTs are network-bound (~700 ms per request once
    # TLS, server processing and TB's retry overhead are added up). To
    # reclaim that latency we send `max_active` requests concurrently via
    # httr2::req_perform_parallel(); the batch size matches max_active so
    # `throttle_seconds` paces *every* group of concurrent requests, not
    # only every Nth batch -- otherwise even max_active = 10 quickly
    # overshoots Free's 600 messages/minute per-device sustained limit.
    # is_transient is widened so the inevitable 500s from rate-limit
    # bursts get retried with exponential backoff, and retry_on_failure
    # is enabled so transport-layer dropouts (TCP "Broken pipe",
    # peer-closed TLS session, brief DNS hiccups) on a 25 min push
    # also get retried instead of aborting the rest of the batch.
    # All ThingsBoard telemetry POSTs are idempotent on (ts, key) so
    # a retried record never duplicates data.
    is_transient_500 <- function(resp) {
      httr2::resp_status(resp) %in% c(408L, 429L, 500L, 502L, 503L, 504L)
    }
    reqs <- lapply(payload, function(record) {
      httr2::request(url) |>
        httr2::req_body_json(record, auto_unbox = TRUE, digits = NA) |>
        httr2::req_retry(
          max_tries        = 4L,
          backoff          = function(j) 2^j,
          is_transient     = is_transient_500,
          retry_on_failure = TRUE
        ) |>
        httr2::req_error(body = tb_error_body)
    })

    batch_size <- max(max_active, 1L)
    starts <- seq.int(1L, n, by = batch_size)
    batch_max_tries <- 4L

    for (start in starts) {
      end <- min(start + batch_size - 1L, n)
      batch_reqs <- reqs[start:end]

      # Wrap the parallel batch in a retry loop. The per-request
      # `retry_on_failure = TRUE` inside req_retry() recovers from a
      # transient HTTP/curl error on a *fresh* libcurl handle, but the
      # connection-pool entry that the upstream gateway has silently
      # dropped stays poisoned across all four configured per-request
      # retries: every retry hits the same dead handle and dies with
      # "Send failure: Broken pipe" within milliseconds. The result is
      # a curl error that bubbles up through req_perform_parallel() and
      # aborts the whole batch. Retrying the *batch* as a whole forces
      # httr2 to allocate a new connection on the next attempt and is
      # safe because the underlying (ts, key) telemetry POSTs are
      # idempotent on the ThingsBoard side -- a re-POST of an already
      # accepted record overwrites itself with the same value, never
      # creates a duplicate row.
      for (batch_try in seq_len(batch_max_tries)) {
        batch_ok <- tryCatch({
          httr2::req_perform_parallel(
            batch_reqs,
            max_active = max_active,
            progress   = FALSE
          )
          TRUE
        }, error = function(e) {
          if (batch_try < batch_max_tries) {
            wait <- 2^batch_try
            message(sprintf(
              "  batch %d-%d failed (%s); batch retry %d/%d in %g s",
              start, end, conditionMessage(e),
              batch_try, batch_max_tries - 1L, wait
            ))
            Sys.sleep(wait)
            FALSE
          } else {
            stop(e)
          }
        })
        if (isTRUE(batch_ok)) break
      }

      if (verbose) {
        message(sprintf(
          "  POSTed %d/%d records (parallel max_active=%d)",
          end, n, max_active
        ))
      }
      if (throttle_seconds > 0 && end < n) Sys.sleep(throttle_seconds)
    }
    return(invisible(n))
  }

  # mode == "bulk" -- works on self-hosted CE, fast.
  starts <- seq.int(1L, n, by = chunk_size)

  for (start in starts) {
    end <- min(start + chunk_size - 1L, n)
    chunk <- payload[start:end]

    if (verbose) {
      message(sprintf(
        "POST %d/%d points to ThingsBoard ...",
        end, n
      ))
    }

    httr2::request(url) |>
      httr2::req_body_json(chunk, auto_unbox = TRUE, digits = NA) |>
      httr2::req_retry(max_tries = 4L, backoff = function(i) 2^i, retry_on_failure = TRUE) |>
      httr2::req_error(body = tb_error_body) |>
      httr2::req_perform()

    if (length(starts) > 1L && throttle_seconds > 0) {
      Sys.sleep(throttle_seconds)
    }
  }

  invisible(n)
}

# tb_error_body ----------------------------------------------------------------

#' Surface the ThingsBoard response body in httr2 errors
#'
#' By default httr2 stops on >= 400 responses with a generic
#' "HTTP 500 Internal Server Error" message that does not include the
#' actual ThingsBoard error payload. Wire this helper into
#' `httr2::req_error(body = tb_error_body)` so the JSON message field
#' is added to the R error.
#'
#' @param resp an httr2 response object.
#' @return character body excerpt or `NULL` if the body cannot be read.
#' @keywords internal
#' @noRd
tb_error_body <- function(resp)
{
  status <- tryCatch(
    httr2::resp_status(resp),
    error = function(e) NA_integer_
  )
  ctype <- tryCatch(
    httr2::resp_header(resp, "Content-Type"),
    error = function(e) NA_character_
  )
  txt <- tryCatch(
    httr2::resp_body_string(resp),
    error = function(e) sprintf("(could not read body: %s)", conditionMessage(e))
  )
  if (is.null(txt) || !nzchar(txt)) {
    txt <- "(empty body)"
  } else if (nchar(txt) > 800L) {
    txt <- paste0(substr(txt, 1L, 800L), " ...")
  }
  sprintf(
    "ThingsBoard response [status=%s, content-type=%s]: %s",
    status, ctype %||% "NA", txt
  )
}

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

# tb_push_station_attributes ---------------------------------------------------

#' Push Static Attributes of one Wasserportal Station to ThingsBoard
#'
#' Sends station metadata (coordinates, level reference, operator, ...) as
#' client-side attributes to the ThingsBoard device attributes endpoint
#' (`/api/v1/{token}/attributes`). Attributes are key/value pairs without a
#' timestamp; ThingsBoard overwrites the previous value on every push.
#'
#' @param attributes named list (or a one-row data frame, which is converted to
#'   a list). All entries must be JSON-serialisable scalars.
#' @param device_token ThingsBoard device access token.
#' @param host base URL of the ThingsBoard instance. Defaults to env var
#'   `TB_HOST` if set, otherwise `"https://thingsboard.cloud"`.
#' @return invisibly the number of attributes that were sent.
#' @export
#' @examples
#' \dontrun{
#' tb_push_station_attributes(
#'   attributes = list(
#'     name = "Pegel Mueggelheim",
#'     latitude = 52.4291,
#'     longitude = 13.6450,
#'     pegelnullpunkt_m_NHN = 32.18
#'   ),
#'   device_token = Sys.getenv("TB_DEVICE_TOKEN_5867000")
#' )
#' }
tb_push_station_attributes <- function(
    attributes,
    device_token,
    host = tb_default_host()
)
{
  stopifnot(nzchar(device_token), nzchar(host))

  if (is.data.frame(attributes)) {
    stopifnot(nrow(attributes) == 1L)
    attributes <- as.list(attributes[1L, , drop = FALSE])
  }

  stopifnot(is.list(attributes), !is.null(names(attributes)))

  url <- sprintf("%s/api/v1/%s/attributes", sub("/+$", "", host), device_token)

  httr2::request(url) |>
    httr2::req_body_json(attributes, auto_unbox = TRUE, digits = NA) |>
    httr2::req_retry(max_tries = 4L, backoff = function(i) 2^i, retry_on_failure = TRUE) |>
    httr2::req_error(body = tb_error_body) |>
    httr2::req_perform()

  invisible(length(attributes))
}

# tb_plan_defaults -------------------------------------------------------------

#' Recommended Push Defaults per ThingsBoard Subscription Plan
#'
#' Wraps the per-device transport rate limits documented at
#' <https://thingsboard.io/docs/paas/eu/subscriptions/> into the
#' parameters this package's push functions take. Pass the result into
#' `tb_push_station_telemetry()` via `mode`, `chunk_size` and
#' `throttle_seconds` so you stay below your plan's
#' "Telemetry Transport messages/data points (Device)" thresholds.
#'
#' Across all paid PaaS tiers the per-device sustained limits are
#' identical (2 000 telemetry data points per minute, 15 000 per hour),
#' the only thing that changes is how aggressive a burst the platform
#' tolerates before it drops a request. Free additionally rejects the
#' bulk array form on the device telemetry endpoint, so its default is
#' `mode = "single"`.
#'
#' Self-hosted ThingsBoard CE has no per-tenant rate limit by default,
#' hence the much larger chunk size and zero throttle.
#'
#' @param plan one of
#'   * `"free"` -- proven Single-record mode (`mode = "single"`,
#'     `chunk_size = 1`, `throttle_seconds = 0.05`).
#'   * `"free-bulk"` -- bulk preset tuned to stay under Free's
#'     per-device 100 dp/s and 2,000 dp/min caps (`chunk_size = 10`,
#'     `throttle_seconds = 1.0`). Confirmed not to work on the
#'     public ThingsBoard Cloud Maker free tier as of 2026-05: the
#'     gateway returns an opaque empty-body HTTP 500 to the array
#'     form regardless of how small the chunk is. Kept as a
#'     reproducible baseline; on Free use `"free"`.
#'   * `"prototype"`, `"pilot"`, `"startup"`, `"business"` -- the
#'     paid PaaS tiers. All use `mode = "bulk"`,
#'     `chunk_size = 30`, `throttle_seconds = 1.0` (~30 dp/s,
#'     well under the 2,000 dp/min cap that all paid tiers share).
#'   * `"ce"` -- self-hosted Community Edition: `mode = "bulk"`,
#'     `chunk_size = 1000`, `throttle_seconds = 0`.
#'
#'   Case-insensitive. Unknown values raise an error.
#' @return named list with `mode`, `chunk_size` and `throttle_seconds`,
#'   ready to be spread into `tb_push_station_telemetry()`.
#' @export
#' @examples
#' tb_plan_defaults("free")
#' tb_plan_defaults("free-bulk")
#' tb_plan_defaults("ce")
tb_plan_defaults <- function(plan = "free")
{
  plan <- tolower(plan)
  presets <- list(
    free = list(
      mode = "single",
      chunk_size = 1L,
      throttle_seconds = 1.0,
      max_active = 10L
    ),
    `free-bulk` = list(
      # Confirmed not to work on the public ThingsBoard Cloud Maker
      # free tier as of 2026-05: even chunk_size=10 + throttle=1.0s
      # (10 dp/s, well under the documented 100 dp/s burst and
      # 2,000 dp/min sustained per-device caps) returns the same
      # opaque empty-body HTTP 500 as the original 100-record
      # attempt. The Maker plan apparently rejects the array form
      # at the gateway irrespective of payload size; single mode
      # remains the only reliable shape on Free. The preset is kept
      # so the experiment is reproducible and so it can serve as a
      # baseline if ThingsBoard ever lifts the restriction.
      mode = "bulk",
      chunk_size = 10L,
      throttle_seconds = 1.0,
      max_active = 1L
    ),
    prototype = list(
      mode = "bulk",
      chunk_size = 30L,
      throttle_seconds = 1.0,
      max_active = 1L
    ),
    pilot = list(
      mode = "bulk",
      chunk_size = 30L,
      throttle_seconds = 1.0,
      max_active = 1L
    ),
    startup = list(
      mode = "bulk",
      chunk_size = 30L,
      throttle_seconds = 1.0,
      max_active = 1L
    ),
    business = list(
      mode = "bulk",
      chunk_size = 30L,
      throttle_seconds = 1.0,
      max_active = 1L
    ),
    ce = list(
      mode = "bulk",
      chunk_size = 1000L,
      throttle_seconds = 0,
      max_active = 1L
    )
  )
  if (!plan %in% names(presets)) {
    stop_formatted(
      "Unknown plan '%s'. Valid: %s",
      plan, paste(names(presets), collapse = ", ")
    )
  }
  presets[[plan]]
}

# tb_push_latest_telemetry -----------------------------------------------------

#' Push a Single "Latest" Telemetry Record (no Timestamp)
#'
#' Sends a flat `{"key": value, ...}` JSON object to the ThingsBoard
#' telemetry endpoint, letting the server stamp it with the current time.
#' This is the simplest possible telemetry POST and is useful both as a
#' smoke test for the device-token auth path and as a fallback when the
#' bulk-with-ts format is rejected (some ThingsBoard Cloud Maker tier
#' configurations return an opaque HTTP 500 to the array-of-records form
#' even though the same device accepts attributes and `latest`-style
#' single records).
#'
#' @param values named list (or named numeric vector) of telemetry
#'   key/value pairs.
#' @param device_token ThingsBoard device access token.
#' @param host base URL of the ThingsBoard instance. Defaults to env var
#'   `TB_HOST` if set, otherwise `"https://thingsboard.cloud"`.
#' @return invisibly the number of keys that were sent.
#' @export
#' @examples
#' \dontrun{
#' tb_push_latest_telemetry(
#'   values = list(`GW-Stand` = 35.6, `Wassertemperatur` = 11.2),
#'   device_token = Sys.getenv("TB_DEVICE_TOKEN")
#' )
#' }
tb_push_latest_telemetry <- function(
    values,
    device_token,
    host = tb_default_host()
)
{
  stopifnot(nzchar(device_token), nzchar(host), length(values) >= 1L)

  if (!is.list(values)) values <- as.list(values)
  stopifnot(!is.null(names(values)), all(nzchar(names(values))))
  names(values) <- sanitize_tb_key(names(values))

  url <- sprintf("%s/api/v1/%s/telemetry", sub("/+$", "", host), device_token)

  httr2::request(url) |>
    httr2::req_body_json(values, auto_unbox = TRUE, digits = NA) |>
    httr2::req_retry(max_tries = 4L, backoff = function(i) 2^i, retry_on_failure = TRUE) |>
    httr2::req_error(body = tb_error_body) |>
    httr2::req_perform()

  invisible(length(values))
}

# build_telemetry_payload ------------------------------------------------------

#' Build ThingsBoard Telemetry Payload from a Long-Format Data Frame
#'
#' Internal helper that converts long-format Wasserportal data to the list
#' structure expected by ThingsBoard's telemetry endpoint:
#' `[{"ts": <ms>, "values": {"<key>": <value>, ...}}, ...]`.
#'
#' @param data data frame.
#' @param ts_col timestamp column name.
#' @param value_col value column name.
#' @param key_col key column name or `NULL`.
#' @param single_key telemetry key used when `key_col` is `NULL`.
#' @param group_by_ts when `TRUE` (default), several rows that share
#'   the same timestamp are merged into a single record whose
#'   `values` dict carries every Parameter measured at that point in
#'   time. Set to `FALSE` to keep one record per row -- ThingsBoard
#'   Cloud Maker rejects "fat" `values` dicts (the ~30-key
#'   groundwater-quality records hit an opaque HTTP 500 even though
#'   each individual key works on its own), so the script flips this
#'   off in single mode.
#' @return list of `list(ts = <numeric ms>, values = list(<key> = <value>))`.
#' @keywords internal
#' @noRd
build_telemetry_payload <- function(
    data, ts_col, value_col, key_col, single_key,
    group_by_ts = TRUE
)
{
  ts_ms <- to_epoch_ms(data[[ts_col]])

  values <- data[[value_col]]

  # ThingsBoard rejects pre-epoch (negative) timestamps with HTTP 500 on
  # several plan tiers. Drop them to keep the push robust; Wasserportal
  # groundwater stations occasionally start in the 1950s.
  finite <- !is.na(ts_ms) & !is.na(values) & ts_ms > 0
  ts_ms  <- ts_ms[finite]
  values <- values[finite]

  if (length(ts_ms) == 0L) {
    return(list())
  }

  if (is.null(key_col)) {
    keys <- rep(single_key, length(values))
  } else {
    keys <- as.character(data[[key_col]][finite])
  }
  keys <- sanitize_tb_key(keys)

  ord <- order(ts_ms)
  ts_ms <- ts_ms[ord]
  values <- values[ord]
  keys   <- keys[ord]

  if (!group_by_ts) {
    return(lapply(seq_along(ts_ms), function(i) {
      list(
        ts = ts_ms[i],
        values = stats::setNames(list(values[i]), keys[i])
      )
    }))
  }

  splits <- split(seq_along(ts_ms), ts_ms)

  lapply(splits, function(idx) {
    list(
      ts = ts_ms[idx[1L]],
      values = stats::setNames(as.list(values[idx]), keys[idx])
    )
  }) |> unname()
}

# tb_setup_devices -------------------------------------------------------------

#' Create ThingsBoard Devices and Return their Access Tokens
#'
#' Convenience wrapper for the initial setup against a fresh ThingsBoard
#' tenant. Authenticates with either a username/password login (JWT -- works
#' on every ThingsBoard edition, required for self-hosted Community Edition)
#' or an account-level API key (ThingsBoard Cloud), then:
#' \enumerate{
#'   \item Create one device per name (or fetch the device if it already
#'     exists),
#'   \item Read each device's access token via the credentials endpoint.
#' }
#' The returned named character vector can be fed directly into
#' \code{\link{tb_push_station_telemetry}} as `device_token`.
#'
#' Set `TB_USERNAME` + `TB_PASSWORD` for the login route, or generate an API
#' key in the ThingsBoard Cloud UI under
#' *Account > Security > API keys > Generate* and set `TB_API_KEY`.
#'
#' @param station_ids character vector of Wasserportal `Messstellennummer`
#'   values. Each becomes a ThingsBoard device named
#'   `paste0(name_prefix, station_id)`.
#' @param api_key account-level API key (ThingsBoard Cloud only), generated
#'   under *Account > Security > API keys > Generate*. Sent in the
#'   `X-Authorization: ApiKey <key>` request header. Defaults to env var
#'   `TB_API_KEY`. Ignored when `username` and `password` are supplied.
#' @param host base URL of the ThingsBoard instance, without trailing slash.
#'   Defaults to env var `TB_HOST` if set, otherwise
#'   `"https://thingsboard.cloud"`. Use `"https://eu.thingsboard.cloud"` for
#'   the EU cloud or e.g. `"https://dashboards.inowas.org"` for a self-hosted
#'   instance.
#' @param name_prefix prefix added in front of every station id when forming
#'   the ThingsBoard device name. Default `"wasserportal-"`.
#' @param device_type ThingsBoard device profile / type. Default
#'   `"wasserportal"`. The profile is auto-created on first use.
#' @param username ThingsBoard user for the username/password (JWT) login.
#'   Defaults to env var `TB_USERNAME`. When set together with `password` it
#'   takes precedence over `api_key` -- this is the route to use for
#'   self-hosted Community Edition.
#' @param password ThingsBoard password. Defaults to env var `TB_PASSWORD`.
#' @return named character vector. Names are the input `station_ids`, values
#'   are device access tokens.
#' @export
#' @examples
#' \dontrun{
#' # Self-hosted ThingsBoard Community Edition (username/password login):
#' Sys.setenv(
#'   TB_HOST = "https://dashboards.inowas.org",
#'   TB_USERNAME = "me@example.org",
#'   TB_PASSWORD = "secret"
#' )
#' tokens <- tb_setup_devices(c("149", "5867000", "5803900"))
#'
#' # ThingsBoard Cloud (account API key):
#' Sys.setenv(
#'   TB_HOST = "https://eu.thingsboard.cloud",
#'   TB_API_KEY = "<paste-your-API-key-here>"
#' )
#' tokens <- tb_setup_devices(c("149", "5867000", "5803900"))
#' }
tb_setup_devices <- function(
    station_ids,
    api_key = Sys.getenv("TB_API_KEY"),
    host = tb_default_host(),
    name_prefix = "wasserportal-",
    device_type = "wasserportal",
    username = Sys.getenv("TB_USERNAME"),
    password = Sys.getenv("TB_PASSWORD")
)
{
  stopifnot(
    is.character(station_ids),
    length(station_ids) >= 1L,
    nzchar(host)
  )

  host <- sub("/+$", "", host)
  auth <- tb_auth_header(
    api_key = api_key, host = host,
    username = username, password = password
  )

  tokens <- vapply(station_ids, function(station_id) {
    device_name <- paste0(name_prefix, station_id)
    device_id   <- tb_get_or_create_device(
      device_name, device_type, auth, host
    )
    tb_get_device_access_token(device_id, auth, host)
  }, character(1L), USE.NAMES = TRUE)

  stats::setNames(tokens, station_ids)
}

# tb_get_device_id -------------------------------------------------------------

#' Look Up a ThingsBoard Device's UUID by Name
#'
#' Lightweight read-only companion to \code{\link{tb_setup_devices}}: when you
#' only need a device's internal UUID (e.g. to call the telemetry-delete
#' endpoint), this returns it directly without touching the access token or
#' creating the device on the side. Returns `NA_character_` when the device
#' does not exist.
#'
#' @param device_name device name as shown in the ThingsBoard UI.
#' @param api_key account-level API key. Defaults to env var `TB_API_KEY`.
#' @param host base URL of the ThingsBoard instance. Defaults to env var
#'   `TB_HOST` if set, otherwise `"https://thingsboard.cloud"`.
#' @param username ThingsBoard user for the username/password (JWT) login
#'   (self-hosted / Community Edition). Defaults to env var `TB_USERNAME`.
#' @param password ThingsBoard password. Defaults to env var `TB_PASSWORD`.
#' @return device UUID (character) or `NA_character_` if the lookup did not
#'   resolve.
#' @export
#' @examples
#' \dontrun{
#' tb_get_device_id("wasserportal-gw-6038")
#' }
tb_get_device_id <- function(
    device_name,
    api_key = Sys.getenv("TB_API_KEY"),
    host = tb_default_host(),
    username = Sys.getenv("TB_USERNAME"),
    password = Sys.getenv("TB_PASSWORD")
)
{
  stopifnot(nzchar(host), nzchar(device_name))
  host <- sub("/+$", "", host)
  auth <- tb_auth_header(
    api_key = api_key, host = host,
    username = username, password = password
  )

  resp <- tryCatch(
    httr2::request(sprintf("%s/api/tenant/devices", host)) |>
      httr2::req_url_query(deviceName = device_name) |>
      httr2::req_headers(`X-Authorization` = auth) |>
      httr2::req_error(is_error = function(r) httr2::resp_status(r) >= 500L) |>
      httr2::req_perform(),
    error = function(e) NULL
  )

  if (is.null(resp) || httr2::resp_status(resp) >= 300L) {
    return(NA_character_)
  }

  body <- httr2::resp_body_json(resp)
  if (is.null(body$id$id)) NA_character_ else body$id$id
}

# tb_list_device_telemetry_keys -----------------------------------------------

#' List the Telemetry Keys Currently Stored for a ThingsBoard Device
#'
#' Wraps `GET /api/plugins/telemetry/DEVICE/{id}/keys/timeseries`. Useful to
#' discover what's actually in the device-side time-series store before a
#' wipe, or to compare against the `Parameter` column of the gh-pages
#' source data.
#'
#' @param device_id device UUID. Use \code{\link{tb_get_device_id}} to
#'   resolve a name.
#' @param api_key account-level API key. Defaults to env var `TB_API_KEY`.
#' @param host base URL of the ThingsBoard instance. Defaults to env var
#'   `TB_HOST` if set, otherwise `"https://thingsboard.cloud"`.
#' @param username ThingsBoard user for the username/password (JWT) login
#'   (self-hosted / Community Edition). Defaults to env var `TB_USERNAME`.
#' @param password ThingsBoard password. Defaults to env var `TB_PASSWORD`.
#' @param auth optional pre-resolved `X-Authorization` header value. When
#'   supplied, the credential arguments are ignored and no extra
#'   `POST /api/auth/login` round-trip is performed. Mainly useful when this
#'   function is chained from another helper that has already obtained an
#'   auth header (e.g. \code{\link{tb_delete_device_telemetry}}).
#' @return character vector of telemetry key names. May be of length 0.
#' @export
#' @examples
#' \dontrun{
#' id <- tb_get_device_id("wasserportal-gw-6038")
#' tb_list_device_telemetry_keys(id)
#' }
tb_list_device_telemetry_keys <- function(
    device_id,
    api_key = Sys.getenv("TB_API_KEY"),
    host = tb_default_host(),
    username = Sys.getenv("TB_USERNAME"),
    password = Sys.getenv("TB_PASSWORD"),
    auth = NULL
)
{
  stopifnot(nzchar(host), nzchar(device_id))
  host <- sub("/+$", "", host)
  if (is.null(auth)) {
    auth <- tb_auth_header(
      api_key = api_key, host = host,
      username = username, password = password
    )
  }

  resp <- httr2::request(sprintf(
    "%s/api/plugins/telemetry/DEVICE/%s/keys/timeseries",
    host, device_id
  )) |>
    httr2::req_headers(`X-Authorization` = auth) |>
    httr2::req_error(body = tb_error_body) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  as.character(unlist(resp))
}

# tb_delete_device_telemetry --------------------------------------------------

#' Delete All Time-Series Data for Selected Keys on a ThingsBoard Device
#'
#' Wipes historical telemetry rows from ThingsBoard for the given device and
#' keys via `DELETE /api/plugins/telemetry/DEVICE/{id}/timeseries/delete`.
#' Pass `keys = NULL` (the default) to clear every key the device currently
#' knows -- the function then calls
#' \code{\link{tb_list_device_telemetry_keys}} first to discover them.
#'
#' Server-side attributes (latitude, longitude, Bezirk, ...) and the device
#' itself are NOT touched, only the time-series telemetry store. Re-running
#' the demo push afterwards re-fills the cleared keys with the real
#' Wasserportal timestamps.
#'
#' @param device_id device UUID.
#' @param keys character vector of telemetry keys to delete, or `NULL` to
#'   clear every key the device currently knows.
#' @param api_key account-level API key. Defaults to env var `TB_API_KEY`.
#' @param host base URL of the ThingsBoard instance. Defaults to env var
#'   `TB_HOST` if set, otherwise `"https://thingsboard.cloud"`.
#' @param delete_latest if `TRUE` (default) ThingsBoard also drops the
#'   cached "latest telemetry" entry so the device-detail tab in the UI
#'   immediately reflects the deletion. Set to `FALSE` to keep the latest
#'   value visible for keys that get repopulated by the next push anyway.
#' @param username ThingsBoard user for the username/password (JWT) login
#'   (self-hosted / Community Edition). Defaults to env var `TB_USERNAME`.
#' @param password ThingsBoard password. Defaults to env var `TB_PASSWORD`.
#' @return invisibly the number of keys submitted for deletion.
#' @export
#' @examples
#' \dontrun{
#' id <- tb_get_device_id("wasserportal-gw-6038")
#' # wipe everything currently stored:
#' tb_delete_device_telemetry(id)
#' # wipe just the smoke-test GW-Stand value:
#' tb_delete_device_telemetry(id, keys = "GW-Stand")
#' }
tb_delete_device_telemetry <- function(
    device_id,
    keys = NULL,
    api_key = Sys.getenv("TB_API_KEY"),
    host = tb_default_host(),
    delete_latest = TRUE,
    username = Sys.getenv("TB_USERNAME"),
    password = Sys.getenv("TB_PASSWORD")
)
{
  stopifnot(nzchar(host), nzchar(device_id))
  host <- sub("/+$", "", host)
  auth <- tb_auth_header(
    api_key = api_key, host = host,
    username = username, password = password
  )

  if (is.null(keys)) {
    keys <- tb_list_device_telemetry_keys(
      device_id = device_id, host = host, auth = auth
    )
  }

  if (length(keys) == 0L) {
    return(invisible(0L))
  }

  httr2::request(sprintf(
    "%s/api/plugins/telemetry/DEVICE/%s/timeseries/delete",
    host, device_id
  )) |>
    httr2::req_method("DELETE") |>
    httr2::req_headers(`X-Authorization` = auth) |>
    httr2::req_url_query(
      keys = paste(keys, collapse = ","),
      deleteAllDataForKeys = "true",
      deleteLatest = if (delete_latest) "true" else "false"
    ) |>
    httr2::req_retry(max_tries = 4L, backoff = function(i) 2^i, retry_on_failure = TRUE) |>
    httr2::req_error(body = tb_error_body) |>
    httr2::req_perform()

  invisible(length(keys))
}

# tb_get_or_create_device ------------------------------------------------------

#' Look Up or Create a Device on ThingsBoard
#'
#' Returns the device id for a given device name. If the device does not yet
#' exist, it is created with the given type/profile.
#'
#' @param device_name device name as shown in the ThingsBoard UI.
#' @param device_type device profile name.
#' @param auth resolved \code{X-Authorization} header value (see
#'   \code{tb_auth_header}).
#' @param host base URL of the ThingsBoard instance.
#' @return device id (uuid string).
#' @keywords internal
#' @noRd
tb_get_or_create_device <- function(device_name, device_type, auth, host)
{
  lookup <- tryCatch(
    httr2::request(sprintf("%s/api/tenant/devices", host)) |>
      httr2::req_url_query(deviceName = device_name) |>
      httr2::req_headers(`X-Authorization` = auth) |>
      httr2::req_error(is_error = function(resp) {
        httr2::resp_status(resp) >= 500L
      }) |>
      httr2::req_perform(),
    error = function(e) NULL
  )

  if (!is.null(lookup) && httr2::resp_status(lookup) < 300L) {
    body <- httr2::resp_body_json(lookup)
    return(body$id$id)
  }

  created <- httr2::request(sprintf("%s/api/device", host)) |>
    httr2::req_headers(`X-Authorization` = auth) |>
    httr2::req_body_json(
      list(name = device_name, type = device_type),
      auto_unbox = TRUE
    ) |>
    httr2::req_error(body = tb_error_body) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  created$id$id
}

# tb_get_device_access_token ---------------------------------------------------

#' Read the Access Token of a ThingsBoard Device
#'
#' @param device_id device uuid as returned by \code{tb_get_or_create_device}.
#' @param auth resolved \code{X-Authorization} header value (see
#'   \code{tb_auth_header}).
#' @param host base URL of the ThingsBoard instance.
#' @return device access token (character).
#' @keywords internal
#' @noRd
tb_get_device_access_token <- function(device_id, auth, host)
{
  resp <- httr2::request(
    sprintf("%s/api/device/%s/credentials", host, device_id)
  ) |>
    httr2::req_headers(`X-Authorization` = auth) |>
    httr2::req_error(body = tb_error_body) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  resp$credentialsId
}

# to_epoch_ms ------------------------------------------------------------------

#' Sanitise a String for Use as a ThingsBoard Telemetry Key
#'
#' ThingsBoard's transport layer accepts arbitrary Unicode JSON keys in
#' theory, but the Cloud Maker free tier returns an opaque HTTP 500 when
#' the values dict contains keys with spaces, parentheses, micro/degree
#' signs or umlauts (e.g. "Leitfaehigkeit 25 grd C vor Ort"). This helper
#' folds umlauts, drops bracket characters and replaces other unsafe
#' punctuation with underscores so Wasserportal Parameter names land as
#' clean keys.
#'
#' @param x character vector.
#' @return character vector, same length as `x`, with each element
#'   transliterated.
#' @keywords internal
#' @noRd
sanitize_tb_key <- function(x)
{
  if (length(x) == 0L) return(x)
  out <- as.character(x)
  out <- chartr("äöüÄÖÜ",
                "aouAOU", out)
  out <- gsub("ß", "ss", out, perl = TRUE)
  out <- gsub("µ", "u",  out, perl = TRUE)  # micro sign
  out <- gsub("°", "",   out, perl = TRUE)  # degree sign
  out <- gsub("[()\\[\\]{}]", "", out, perl = TRUE)
  out <- gsub("[ /.,;:]+", "_", out, perl = TRUE)
  out <- gsub("_+", "_", out, perl = TRUE)
  out <- gsub("^_|_$", "", out, perl = TRUE)
  out
}

# to_epoch_ms ------------------------------------------------------------------

#' Convert Date/POSIXct/Character to Epoch Milliseconds
#'
#' @param x a vector of `Date`, `POSIXct` or character (parsed as date).
#' @return numeric vector of milliseconds since 1970-01-01 UTC.
#' @keywords internal
#' @noRd
to_epoch_ms <- function(x)
{
  if (inherits(x, "POSIXct")) {
    return(as.numeric(x) * 1000)
  }

  if (inherits(x, "Date")) {
    return(as.numeric(as.POSIXct(x, tz = "UTC")) * 1000)
  }

  parsed <- suppressWarnings(as.POSIXct(x, tz = "UTC"))

  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(as.POSIXct(as.Date(x), tz = "UTC"))
  }

  as.numeric(parsed) * 1000
}

# tb_login ---------------------------------------------------------------------

#' Obtain a JWT Bearer Token from ThingsBoard (Username / Password Login)
#'
#' Calls `POST /api/auth/login` with a username/password pair and returns the
#' JWT access token. This is the **standard ThingsBoard REST API
#' authentication** and the only one available on self-hosted ThingsBoard
#' Community Edition: unlike the account-level *API key* (a ThingsBoard Cloud
#' convenience, generated under *Account > Security > API keys*), every
#' edition -- CE, PE and Cloud -- accepts a username/password login.
#'
#' The token is short-lived (ThingsBoard's default JWT expiration is 2.5 h),
#' which is ample for the one-off device setup done by
#' \code{\link{tb_setup_devices}}: the subsequent telemetry push uses the
#' per-device access token, not this JWT, so no token refresh is implemented.
#'
#' Transient failures (HTTP 408 / 429 / 500 / 502 / 503 / 504 and transport
#' dropouts) are retried with exponential backoff, matching the predicate
#' used for the telemetry POSTs so a flaky upstream does not abort the
#' device-setup run on the first 5xx.
#'
#' @param username ThingsBoard user (usually the account e-mail). Defaults to
#'   env var `TB_USERNAME`.
#' @param password ThingsBoard password. Defaults to env var `TB_PASSWORD`.
#' @param host base URL of the ThingsBoard instance, without trailing slash.
#'   Defaults to env var `TB_HOST` if set, otherwise
#'   `"https://thingsboard.cloud"`. Use e.g.
#'   `"https://dashboards.inowas.org"` for a self-hosted instance.
#' @return the JWT access token as a character scalar, ready to be sent in an
#'   `X-Authorization: Bearer <token>` request header.
#' @section Credentials in error output:
#'   On a non-2xx response this helper surfaces an excerpt of the server's
#'   response body (via `tb_error_body()`, up to ~800 chars) in the R error
#'   message, and `httr2::req_retry()` prints retry messages to stderr.
#'   Stock ThingsBoard only echoes back the error description, not the
#'   request payload, so the password does not leak. If a self-hosted
#'   instance or reverse proxy is configured to echo request fields back in
#'   the error body, that excerpt would surface in R errors and -- when
#'   captured with `2>&1` -- in CI logs. Mask the relevant secrets in such
#'   environments.
#' @export
#' @examples
#' \dontrun{
#' Sys.setenv(
#'   TB_HOST = "https://dashboards.inowas.org",
#'   TB_USERNAME = "me@example.org",
#'   TB_PASSWORD = "secret"
#' )
#' token <- tb_login()
#' }
tb_login <- function(
    username = Sys.getenv("TB_USERNAME"),
    password = Sys.getenv("TB_PASSWORD"),
    host = tb_default_host()
)
{
  stopifnot(nzchar(username), nzchar(password), nzchar(host))
  host <- sub("/+$", "", host)

  # Match the transient-failure set used by tb_push_station_telemetry(): the
  # httr2 default only retries 429/503, but a self-hosted ThingsBoard sitting
  # behind nginx / a load balancer can briefly return 500/502/504 on cold
  # starts or restarts. /api/auth/login is idempotent, so retrying is safe.
  is_transient_500 <- function(resp) {
    httr2::resp_status(resp) %in% c(408L, 429L, 500L, 502L, 503L, 504L)
  }

  resp <- httr2::request(sprintf("%s/api/auth/login", host)) |>
    httr2::req_body_json(
      list(username = username, password = password),
      auto_unbox = TRUE
    ) |>
    httr2::req_retry(
      max_tries        = 4L,
      backoff          = function(i) 2^i,
      is_transient     = is_transient_500,
      retry_on_failure = TRUE
    ) |>
    httr2::req_error(body = tb_error_body) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  token <- resp$token
  if (is.null(token) || !nzchar(token)) {
    stop_formatted("ThingsBoard login to '%s' returned no token.", host)
  }
  token
}

# tb_auth_header ---------------------------------------------------------------

#' Resolve the X-Authorization Header for the ThingsBoard Tenant REST API
#'
#' Picks the authentication scheme from the credentials that are available:
#' a username/password pair yields a fresh JWT via \code{\link{tb_login}},
#' sent as `Bearer <token>` (works on every ThingsBoard edition and is
#' required for self-hosted Community Edition); otherwise an account-level
#' API key is sent as `ApiKey <key>` (ThingsBoard Cloud only).
#' Username/password win when both are configured.
#'
#' @param api_key account-level API key (Cloud). Defaults to `TB_API_KEY`.
#' @param host base URL. Defaults to `TB_HOST` / `https://thingsboard.cloud`.
#' @param username ThingsBoard user. Defaults to `TB_USERNAME`.
#' @param password ThingsBoard password. Defaults to `TB_PASSWORD`.
#' @return the ready-to-use `X-Authorization` header value (character).
#' @keywords internal
#' @noRd
tb_auth_header <- function(
    api_key = Sys.getenv("TB_API_KEY"),
    host = tb_default_host(),
    username = Sys.getenv("TB_USERNAME"),
    password = Sys.getenv("TB_PASSWORD")
)
{
  has_username <- nzchar(username)
  has_password <- nzchar(password)
  if (has_username && has_password) {
    paste("Bearer", tb_login(username, password, host))
  } else if (nzchar(api_key)) {
    if (has_username || has_password) {
      warning(
        "Only one of TB_USERNAME / TB_PASSWORD is set; ",
        "falling back to API-key auth. Set both to use JWT login."
      )
    }
    paste("ApiKey", api_key)
  } else {
    stop(paste0(
      "No ThingsBoard credentials found. Set TB_USERNAME + TB_PASSWORD ",
      "(self-hosted / Community Edition, recommended) or TB_API_KEY ",
      "(ThingsBoard Cloud)."
    ))
  }
}
