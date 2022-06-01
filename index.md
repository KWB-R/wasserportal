[![R-CMD-check](https://github.com/KWB-R/wasserportal/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/wasserportal/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/wasserportal/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/wasserportal/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/wasserportal/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/wasserportal)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/wasserportal)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/wasserportal)](https://kwb-r.r-universe.dev/)

R Package with Functions for Scraping Data of
Wasserportal Berlin (https://wasserportal.berlin.de), which contains
real-time data of surface water and groundwater monitoring stations.

## Installation

For installing the latest release of this R package run the following code below:

```r
# Enable repository from kwb-r
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
  
# Download and install wasserportal in R
install.packages('wasserportal')

# Browse the wasserportal manual pages
help(package = 'wasserportal')
```
## Usage 

Checkout the [Tutorial](articles/tutorial.html) article on how to use this R package
for scraping data from the [Wasserportal Berlin](https://wasserportal.berlin.de), 
which is performed automatically every day at 5 am UTC using [GitHub actions](https://github.com/KWB-R/wasserportal/actions/workflows/pkgdown.yaml).

How this data can be used directly from within R is shown in the [Groundwater](articles/groundwater.html) article.





