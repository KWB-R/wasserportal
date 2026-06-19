# Recommended Push Defaults per ThingsBoard Subscription Plan

Wraps the per-device transport rate limits documented at
<https://thingsboard.io/docs/paas/eu/subscriptions/> into the parameters
this package's push functions take. Pass the result into
[`tb_push_station_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_push_station_telemetry.md)
via `mode`, `chunk_size` and `throttle_seconds` so you stay below your
plan's "Telemetry Transport messages/data points (Device)" thresholds.

Across all paid PaaS tiers the per-device sustained limits are identical
(2 000 telemetry data points per minute, 15 000 per hour), the only
thing that changes is how aggressive a burst the platform tolerates
before it drops a request. Free additionally rejects the bulk array form
on the device telemetry endpoint, so its default is `mode = "single"`.

Self-hosted ThingsBoard CE has no per-tenant rate limit by default,
hence the much larger chunk size and zero throttle.

## Usage

``` r
tb_plan_defaults(plan = "free")
```

## Arguments

- plan:

  one of

  - `"free"` – proven Single-record mode (`mode = "single"`,
    `chunk_size = 1`, `throttle_seconds = 0.05`).

  - `"free-bulk"` – bulk preset tuned to stay under Free's per-device
    100 dp/s and 2,000 dp/min caps (`chunk_size = 10`,
    `throttle_seconds = 1.0`). Confirmed not to work on the public
    ThingsBoard Cloud Maker free tier as of 2026-05: the gateway returns
    an opaque empty-body HTTP 500 to the array form regardless of how
    small the chunk is. Kept as a reproducible baseline; on Free use
    `"free"`.

  - `"prototype"`, `"pilot"`, `"startup"`, `"business"` – the paid PaaS
    tiers. All use `mode = "bulk"`, `chunk_size = 30`,
    `throttle_seconds = 1.0` (~30 dp/s, well under the 2,000 dp/min cap
    that all paid tiers share).

  - `"ce"` – self-hosted Community Edition: `mode = "bulk"`,
    `chunk_size = 1000`, `throttle_seconds = 0`.

  Case-insensitive. Unknown values raise an error.

## Value

named list with `mode`, `chunk_size` and `throttle_seconds`, ready to be
spread into
[`tb_push_station_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_push_station_telemetry.md).

## Examples

``` r
tb_plan_defaults("free")
#> $mode
#> [1] "single"
#> 
#> $chunk_size
#> [1] 1
#> 
#> $throttle_seconds
#> [1] 1
#> 
#> $max_active
#> [1] 10
#> 
tb_plan_defaults("free-bulk")
#> $mode
#> [1] "bulk"
#> 
#> $chunk_size
#> [1] 10
#> 
#> $throttle_seconds
#> [1] 1
#> 
#> $max_active
#> [1] 1
#> 
tb_plan_defaults("ce")
#> $mode
#> [1] "bulk"
#> 
#> $chunk_size
#> [1] 1000
#> 
#> $throttle_seconds
#> [1] 0
#> 
#> $max_active
#> [1] 1
#> 
```
