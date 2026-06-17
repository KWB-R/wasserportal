# inspect_gh_pages_zips -------------------------------------------------------

#' Download and Inspect Wasserportal ZIP Files Hosted on gh-pages
#'
#' Convenience helper for local debugging of the daily ZIP artefacts
#' published at <https://kwb-r.github.io/wasserportal>. Downloads each ZIP,
#' extracts the CSV, reads it with `readr::read_csv()` and prints a short
#' summary (columns, row count, unique `Messstellennummer` count, head of
#' the data). The intersection of Messstellennummer values across all
#' loaded files is reported at the end so you can quickly see how many
#' stations have measurements in *every* file.
#'
#' Returns the loaded data frames invisibly so the caller can further
#' inspect them in R, e.g. `dat$groundwater_level$Parameter |> table()`.
#'
#' @param files character vector of ZIP file names hosted under
#'   `base_url`. Defaults to the two groundwater ZIPs.
#' @param base_url base URL where the ZIPs are hosted, without trailing
#'   slash. Default: `https://kwb-r.github.io/wasserportal`.
#' @param destdir directory used to download and extract the ZIPs. Default
#'   is a fresh tempdir; pass an explicit path to keep the unpacked CSVs
#'   around for further inspection.
#' @param head_rows number of rows to print from the top of every loaded
#'   data frame. Default 5.
#' @return invisibly a named list of `tibble`s, one per input file. Names
#'   are derived from the ZIP basename without the extension.
#' @export
#' @importFrom archive archive_extract
#' @importFrom readr read_csv cols
#' @importFrom utils download.file head
#' @examples
#' \dontrun{
#' # default: groundwater level + groundwater quality
#' dat <- inspect_gh_pages_zips()
#'
#' # any ZIPs you want to inspect:
#' dat <- inspect_gh_pages_zips(files = c(
#'   "daily_surface-water_water-level.zip",
#'   "daily_surface-water_temperature.zip"
#' ))
#'
#' # keep the extracted CSVs:
#' dat <- inspect_gh_pages_zips(destdir = "~/tmp/wasserportal-inspect")
#' }
inspect_gh_pages_zips <- function(
    files = c("groundwater_level.zip", "groundwater_quality.zip"),
    base_url = "https://kwb-r.github.io/wasserportal",
    destdir = tempfile("wasserportal-inspect-"),
    head_rows = 5L
)
{
  base_url <- sub("/+$", "", base_url)

  if (!dir.exists(destdir)) dir.create(destdir, recursive = TRUE)

  message(sprintf("Working directory: %s", destdir))

  results <- lapply(stats::setNames(files, sub("\\.zip$", "", files)),
                    function(zip_name) {
    zip_url  <- paste0(base_url, "/", zip_name)
    zip_path <- file.path(destdir, zip_name)

    message(sprintf("\n--- %s ---", zip_name))
    message(sprintf("Downloading %s", zip_url))
    utils::download.file(zip_url, zip_path, mode = "wb", quiet = TRUE)
    message(sprintf("File size: %s",
                    format(structure(file.info(zip_path)$size,
                                     class = "object_size"),
                           units = "auto")))

    extract_dir <- file.path(destdir, sub("\\.zip$", "", zip_name))
    if (!dir.exists(extract_dir)) dir.create(extract_dir)
    archive::archive_extract(zip_path, dir = extract_dir)

    csv_files <- list.files(extract_dir, pattern = "\\.csv$", full.names = TRUE)
    if (length(csv_files) == 0L) {
      message("No CSV inside the ZIP.")
      return(NULL)
    }
    if (length(csv_files) > 1L) {
      message(sprintf("Multiple CSVs found, using the first: %s",
                      basename(csv_files[1L])))
    }

    data <- readr::read_csv(
      csv_files[1L],
      show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_guess())
    )

    message(sprintf("Rows:    %d", nrow(data)))
    message(sprintf("Columns: %s", paste(names(data), collapse = ", ")))
    if ("Messstellennummer" %in% names(data)) {
      ids <- unique(as.character(data$Messstellennummer))
      message(sprintf("Unique Messstellennummer: %d", length(ids)))
    }
    if ("Parameter" %in% names(data)) {
      pars <- unique(data$Parameter)
      message(sprintf("Unique Parameter values:  %d", length(pars)))
    }
    if ("Datum" %in% names(data)) {
      message(sprintf("Datum range: %s -> %s",
                      min(data$Datum, na.rm = TRUE),
                      max(data$Datum, na.rm = TRUE)))
    }

    message("Head:")
    print(utils::head(data, head_rows))

    data
  })

  # Cross-file overlap on Messstellennummer
  has_id <- vapply(
    results,
    function(d) "Messstellennummer" %in% names(d),
    logical(1L)
  )
  if (sum(has_id) >= 2L) {
    id_lists <- lapply(results[has_id], function(d) {
      unique(as.character(d$Messstellennummer))
    })
    common <- Reduce(intersect, id_lists)
    message(sprintf(
      "\nMessstellennummer present in all %d files: %d",
      sum(has_id), length(common)
    ))
    if (length(common) > 0L) {
      preview <- utils::head(sort(as.character(common)), 20L)
      message(sprintf("First %d: %s",
                      length(preview),
                      paste(preview, collapse = ", ")))
    }
  }

  invisible(results)
}
