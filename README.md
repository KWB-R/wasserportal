[![R-CMD-check](https://github.com/KWB-R/wasserportal/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/wasserportal/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/wasserportal/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/wasserportal/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/wasserportal/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/wasserportal)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/wasserportal)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/wasserportal)](https://kwb-r.r-universe.dev/)
[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.6602573.svg)](https://doi.org/10.5281/zenodo.6602573)

# wasserportal

R Package with Functions for Scraping Data of
Wasserportal Berlin (https://wasserportal.berlin.de), which contains
real-time data of surface water and groundwater monitoring stations.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'wasserportal' from GitHub
remotes::install_github("KWB-R/wasserportal")
```

## Documentation

Release: [https://kwb-r.github.io/wasserportal](https://kwb-r.github.io/wasserportal)

Development: [https://kwb-r.github.io/wasserportal/dev](https://kwb-r.github.io/wasserportal/dev)
