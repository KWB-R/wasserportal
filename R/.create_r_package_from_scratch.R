### How to build an R package from scratch
remotes::install_github("kwb-r/kwb.pkgbuild")

usethis::create_package(".")
fs::file_delete(path = "DESCRIPTION")


author <- list(name = "Hauke Sonnenberg",
               orcid = "0000-0001-9134-2871",
               url = "https://github.com/hsonne")

pkg <- list(name = "wasserportal",
            title = "R Package with Functions for Scraping Data of Wasserportal Berlin",
            desc  = paste("R Package with Functions for Scraping Data of",
                          "Wasserportal Berlin (https://wasserportal.berlin.de),",
                          "which contains real-time data of surface water and",
                          "groundwater monitoring stations."))


kwb.pkgbuild::use_pkg(author,
                      pkg,
                      version = "0.0.0.9000",
                      stage = "experimental")


usethis::use_vignette("documention")

### R functions
if(FALSE) {
  ## add your dependencies (-> updates: DESCRIPTION)
  pkg_dependencies <- c('dplyr', 'httr', 'kwb.utils', 'magrittr', 'rlang', 'rvest',
                        'stringr', 'tidyr', 'xml2')

  sapply(pkg_dependencies, usethis::use_package)

  desc::desc_add_remotes("kwb-r/kwb.datetime",normalize = TRUE)
  desc::desc_add_remotes("kwb-r/kwb.utils",normalize = TRUE)
  usethis::use_pipe()
}

kwb.pkgbuild::use_ghactions()

kwb.pkgbuild::create_empty_branch_ghpages("wasserportal")
