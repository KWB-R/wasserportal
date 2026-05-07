# Norman Lists

## Install R Package

``` r

# Enable this universe
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Install R package
install.packages('wasserportal')
```

## Get Norman Lists

``` r

library(wasserportal)

download_file <- function(url, 
                            tdir = tempdir()
) {
filename <- basename(url)
t_path <- file.path(tdir, filename)
download.file(url, dest= t_path, mode="wb") 
t_path
}

### Download S0 | SUSDAT | Merged NORMAN Suspect List: SusDat
### Version: NORMAN-SLE-S0.0.4.1 (2021-01-18)
### DOI: 10.5281/zenodo.5873975

#norman_s0_path  <- download_file("https://zenodo.org/record/5873975/files/susdat_2022-01-18-104316.csv")
#norman_s0 <- readr::read_csv(norman_s0_path)

### Download S36 | UBAPMT | Potential Persistent, Mobile and Toxic (PMT) substances
### Version: NORMAN-SLE-S36.0.2.1 (2020-12-15)
### DOI: "10.5281/zenodo.4323239"

norman_s36_ubapmt_path  <- download_file("https://zenodo.org/record/4323239/files/S36_UBAPMT_Dec2020.csv")
norman_s36_ubapmt <- readr::read_csv(norman_s36_ubapmt_path)
#> Rows: 258 Columns: 35
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (33): CAS_Number, Name, List, ProtectedCAS, REACH_Emission_Likelihood, P...
#> dbl  (2): Largest_Fragment_mass, PubChemCID_largestFragment
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.


### Download S90 | ZEROPMBOX1 | ZeroPM Box 1 Substances
### Version: Version NORMAN-SLE-S90.0.1.0 (2021-01-15)
### DOI: 10.5281/zenodo.5854252

norman_s90_zeropm_path  <- download_file("https://zenodo.org/record/5854252/files/ZeroPM_Box1.csv")
norman_s90_zeropm <- readr::read_csv(norman_s90_zeropm_path)
#> New names:
#> * Synonym -> Synonym...11
#> * Synonym -> Synonym...12
#> * Synonym -> Synonym...13
#> Rows: 38 Columns: 13── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (11): CAS, Name, DTXSID, InChIKey, SMILES, InChI, MolecularFormula, IUPA...
#> dbl  (2): PubChem_CID, MonoisotopicMass
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.


cas_wasserportal <- wasserportal::readPackageFile(file = "cas_wasserportal.csv",
                                                  encoding = "UTF-8")
cas_reach <- wasserportal::readPackageFile(file = "cas_reach.csv")

ubapmt_publication <- cas_reach %>%
  dplyr::filter(.data$cas_number %in% unique(cas_wasserportal$cas_number))


ubapmt_zenodo <- norman_s36_ubapmt %>%
  dplyr::filter(.data$CAS_Number %in% unique(cas_wasserportal$cas_number)) %>%
  dplyr::rename(cas_number = .data$CAS_Number)

missing_on_zenodo <- cas_reach %>%
  dplyr::mutate(zenodo = dplyr::if_else(.data$cas_number %in% unique(ubapmt_zenodo$cas_number),
                                        "yes",
                                        NA_character_),
                publication = dplyr::if_else(.data$cas_number %in% unique(ubapmt_publication$cas_number),
                                        "yes",
                                        NA_character_)) %>%
  dplyr::filter(publication == "yes" | zenodo == "yes") %>%
  dplyr::relocate(tidyselect::all_of(c("publication", "zenodo")), .before = .data$emission_likelihood)


DT::datatable(missing_on_zenodo, filter = "top", rownames = FALSE)
```

\# norman_s0_in_wasserportal % \# dplyr::mutate(cas_number =
stringr::str_remove(.data\$CAS_RN_Dashboard, \# pattern =
"^CAS_RN:\\s")) %\>% \# dplyr::filter(!is.na(.data\$cas_number), \#
.data\$cas_number %in% unique(cas_wasserportal\$cas_number))
\#DT::datatable(norman_s0_in_wasserportal, filter = "top", rownames =
FALSE) norman_lists_table % rvest::html_element(css = ".table") %\>%
rvest::html_table() %\>% janitor::clean_names()
DT::datatable(norman_lists_table)

## Get GW Quality from Wasserportal

``` r

# Load R package
library(wasserportal)

### For details see:
### https://kwb-r.github.io/wasserportal/articles/groundwater.html
### JSON files (see below) are build every day automatically at 5a.m. with
### continious integration, for build status, see here:
### https://github.com/KWB-R/wasserportal/actions/workflows/pkgdown.yaml

### GW quality (all available parameters!)
gwq_master <- jsonlite::fromJSON("https://kwb-r.github.io/wasserportal/stations_gwq_master.json")
gwq_data <- jsonlite::fromJSON("https://kwb-r.github.io/wasserportal/stations_gwq_data.json") %>%
  dplyr::filter(Parameter %in% cas_wasserportal$Parameter) %>%
  dplyr::inner_join(cas_wasserportal, by = "Parameter") %>%
  dplyr::inner_join(norman_s0_in_wasserportal, by = "cas_number") %>% 
  dplyr::mutate(Messstellennummer = as.character(Messstellennummer),
## CensorCode: either "below" (less than) for concentration below detection limit
## (value is detection limit) or "nc" (not censored) for concentration above
## detection limit
                CensorCode = dplyr::case_when(Messwert <= 0 ~ "lt",
                                              TRUE ~ "nc"),
                Messwert = dplyr::case_when(Messwert < 0 ~ abs(Messwert),
### Only two decimal numbers are exported by Wasserportal, but some sustances
### have lower detection limit, e.g. 0.002 which results in -0.00 export, thus
### the dummy detection limit 0.00999 was introduced (until fixed by Senate:
### Christoph will sent a email to Matthias Schröder)
                                            Messwert == 0 ~ 0.009999,
                                            TRUE ~ Messwert)) %>%
  dplyr::left_join(gwq_master, by = c("Messstellennummer" = "Nummer"))

gwq_subs <- gwq_data %>%
  dplyr::count(.data$cas_number, .data$CensorCode) %>%
  tidyr::pivot_wider(names_from = CensorCode, values_from = n) %>%
  dplyr::mutate(lt = ifelse(is.na(lt), 0, lt),
                nc = ifelse(is.na(nc), 0, nc),
                n_total = lt + nc,
                percent_nc = 100*nc/n_total) %>%
  dplyr::rename(n_lt = lt,
                n_nc = nc) %>%
  dplyr::left_join(norman_s0_in_wasserportal) %>%
  dplyr::rename(name_norman = .data$Name_Dashboard)

 readr::write_csv(gwq_subs, "gwq_subs.csv")
 DT::datatable(gwq_subs, filter = "top", rownames = FALSE)

samples <- gwq_data %>%
   dplyr::rename(name_norman = .data$Name_Dashboard) %>%
   dplyr::select(name_norman,
                 cas_number,
                 Messstellennummer,
                 Datum,
                 CensorCode,
                 Messwert,
                 Einheit)


samples_by_para_and_station <- gwq_data %>%
  dplyr::count(.data$cas_number,
                  .data$Messstellennummer,
                  .data$CensorCode) %>%
  tidyr::pivot_wider(names_from = CensorCode, values_from = n) %>%
  dplyr::mutate(lt = ifelse(is.na(lt), 0, lt),
                nc = ifelse(is.na(nc), 0, nc),
                n_total = lt + nc,
                percent_nc = 100*nc/n_total) %>%
  dplyr::rename(n_lt = lt,
                n_nc = nc) %>%
  dplyr::left_join(norman_s0_in_wasserportal) %>%
  dplyr::rename(name_norman = .data$Name_Dashboard) %>%
  dplyr::left_join(gwq_master, by = c(Messstellennummer = "Nummer")) %>%
  dplyr::arrange(dplyr::desc(percent_nc))


# samples_by_category_and_station <- samples_by_para_and_station  %>%
#   dplyr::group_by(.data$category,
#                   .data$category_name,
#                   .data$Messstellennummer) %>%
#   dplyr::summarise(n_lt = sum(n_lt),
#                    n_nc = sum(n_nc),
#                    n_total = sum(n_total)) %>%
#   dplyr::mutate(percent_nc = 100*n_nc/n_total) %>%
#   dplyr::arrange(dplyr::desc(percent_nc))

gwq_subs_stations_n_abovedetection <- samples_by_para_and_station  %>%
  dplyr::filter(n_nc > 0) %>%
  dplyr::group_by(.data$cas_number) %>%
  dplyr::summarise(n_stations_abovedetection = dplyr::n())

gwq_subs_stations_n_paras_abovedetection <- samples_by_para_and_station  %>%
  dplyr::filter(n_nc > 0) %>%
  dplyr::group_by(#.data$category,
                  #.data$category_name,
                  .data$Messstellennummer) %>%
  dplyr::summarise(n_paras_abovedetection = dplyr::n()) %>%
  dplyr::left_join(gwq_master, by = c("Messstellennummer" = "Nummer"))

# gwq_subs_stations_n_paras_abovedetection_wide <- gwq_subs_stations_n_paras_abovedetection %>%
#   dplyr::ungroup() %>%
#   dplyr::select(Messstellennummer, category, n_paras_abovedetection) %>%
#   tidyr::pivot_wider(names_from = "category",
#                      names_prefix = "cat_",
#                      values_from = "n_paras_abovedetection") %>%
#   dplyr::left_join(gwq_master, by = c("Messstellennummer" = "Nummer"))


samples_by_para_and_station_n <- samples_by_para_and_station %>%
  dplyr::group_by(name_norman,
                  cas_number) %>%
  dplyr::summarise(n_stations_sampled = dplyr::n(),
                   n_stations_total = length(unique(gwq_master$Nummer)),
                   n_lt = sum(n_lt),
                   n_nc = sum(n_nc),
                   n_total = sum(n_total)) %>%
  dplyr::left_join(gwq_subs_stations_n_abovedetection) %>%
  dplyr::mutate(n_stations_abovedetection = ifelse(is.na(n_stations_abovedetection),
                                                   0,
                                                   n_stations_abovedetection),
                n_abovedetection = ifelse(is.na(n_nc), 0, n_nc),
                n_belowdetection = ifelse(is.na(n_lt), 0, n_lt),
                percent_samples_abovedetection = 100*n_nc/n_total,
                percent_stations_abovedetection = 100*n_stations_abovedetection/n_stations_total,
                percent_stations_sampled = 100*n_stations_sampled/n_stations_total) %>%
  dplyr::select(name_norman,
                cas_number,
                n_stations_abovedetection,
                n_stations_sampled,
                n_stations_total,
                percent_stations_abovedetection,
                percent_stations_sampled,
                n_belowdetection,
                n_abovedetection,
                n_total,
                percent_samples_abovedetection) %>%
  dplyr::arrange(dplyr::desc(percent_stations_abovedetection),
                 dplyr::desc(percent_samples_abovedetection))





### Export data to EXCEL
gwq_data_list <- list(norman_lists_table = norman_lists_table, 
                      cas_wasserportal = cas_wasserportal,
                      cas_reach = cas_reach, 
                      ubapmt_missing_on_zenodo = missing_on_zenodo,
                      norman_s0_in_wasserportal = norman_s0_in_wasserportal,
                      samples = samples,
                      samples_by_para = gwq_subs %>%
                        dplyr::arrange(dplyr::desc(percent_nc)),
                      samples_by_para_and_station = samples_by_para_and_station,
                      samples_by_para_and_station_n = samples_by_para_and_station_n)
                      #samples_by_stations_para_above = gwq_subs_stations_n_paras_abovedetection_wide)
                      #samples_by_category_and_station = samples_by_category_and_station)



openxlsx::write.xlsx(x = gwq_data_list,
                     file = "wasserportal_gwq_norman-list_s0_v1.0.0.xlsx",
                     overwrite = TRUE)
```
