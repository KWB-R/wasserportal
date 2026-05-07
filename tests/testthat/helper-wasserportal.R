skip_if_wasserportal_unreachable <- function() {
  testthat::skip_on_cran()
  res <- tryCatch(
    httr::HEAD(
      "https://wasserportal.berlin.de/",
      httr::timeout(5)
    ),
    error = function(e) NULL
  )
  if (is.null(res) || httr::status_code(res) >= 400) {
    testthat::skip("wasserportal.berlin.de is not reachable from this host")
  }
}
