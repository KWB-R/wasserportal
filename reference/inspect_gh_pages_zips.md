# Download and Inspect Wasserportal ZIP Files Hosted on gh-pages

Convenience helper for local debugging of the daily ZIP artefacts
published at <https://kwb-r.github.io/wasserportal>. Downloads each ZIP,
extracts the CSV, reads it with
[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
and prints a short summary (columns, row count, unique
`Messstellennummer` count, head of the data). The intersection of
Messstellennummer values across all loaded files is reported at the end
so you can quickly see how many stations have measurements in *every*
file.

## Usage

``` r
inspect_gh_pages_zips(
  files = c("groundwater_level.zip", "groundwater_quality.zip"),
  base_url = "https://kwb-r.github.io/wasserportal",
  destdir = tempfile("wasserportal-inspect-"),
  head_rows = 5L
)
```

## Arguments

- files:

  character vector of ZIP file names hosted under `base_url`. Defaults
  to the two groundwater ZIPs.

- base_url:

  base URL where the ZIPs are hosted, without trailing slash. Default:
  `https://kwb-r.github.io/wasserportal`.

- destdir:

  directory used to download and extract the ZIPs. Default is a fresh
  tempdir; pass an explicit path to keep the unpacked CSVs around for
  further inspection.

- head_rows:

  number of rows to print from the top of every loaded data frame.
  Default 5.

## Value

invisibly a named list of `tibble`s, one per input file. Names are
derived from the ZIP basename without the extension.

## Details

Returns the loaded data frames invisibly so the caller can further
inspect them in R, e.g. `dat$groundwater_level$Parameter |> table()`.

## Examples

``` r
if (FALSE) { # \dontrun{
# default: groundwater level + groundwater quality
dat <- inspect_gh_pages_zips()

# any ZIPs you want to inspect:
dat <- inspect_gh_pages_zips(files = c(
  "daily_surface-water_water-level.zip",
  "daily_surface-water_temperature.zip"
))

# keep the extracted CSVs:
dat <- inspect_gh_pages_zips(destdir = "~/tmp/wasserportal-inspect")
} # }
```
