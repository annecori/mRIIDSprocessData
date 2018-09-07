library(dplyr)
## Import linelist and get the incidence curve from it.

linelist <- get(load("data/CaseCounts/drc/linelist_29082018.RData"))
linelist$date_onset_new <- as.Date(linelist$date_onset_new)

## Fix name difference
linelist$location_province <-
    stringr::str_replace_all(linelist$location_province,
                             "NORD- KIVU",
                             "NORD-KIVU")

incid_all <- incidence::incidence(linelist$date_onset_new,
                                  groups = linelist$location_province) %>%
    as.data.frame()


readr::write_csv(x = incid_all,
                 path = here::here("data/CaseCounts/drc",
                                   "incid_drc_04052018.csv"))

## Extract centroids for the places of interest
centroids <- here::here("data/Geography/centroids/processed",
                        "adm0_centroids_fixed.tsv") %>%
    readr::read_tsv()

## Countries that share a border with Liberia, Guinea or Sierra
wafrica <- c("Liberia",
             "Guinea",
             "Sierra Leone",
             "Guinea-Bissau",
             "Gambia",
             "Senegal",
             "Mali",
             "Côte d'Ivoire"
             )
filter(centroids, ADM0 %in% wafrica) %>%
    readr::write_csv(path = here::here("data/Geography/centroids/processed",
                                       "wafrica_adm0_centroids.csv"))

## First create the relative risk profile using gravity model alone
## Then use the epicurve to weight the profiles. This step requires
## SI mean and sd.
## Then create the map
## params <- list(from = "ituri",
##                alpha = 2.01,
##                rho = 72.96,
##                tau = 1.12)

## 05092018 Alternative parameter values
## params <- list(from = "ituri",
##                alpha = 1.91,
##                rho = 88.23,
##                tau = 1.22)

## 05092018 Parameters from model fitted to Zambia
## params <- list(from = "ituri",
##                alpha = 1.70,
##                rho = 38.47,
##                tau = 0.91)

## 05092018 Parameters from model fitted to Tanzania
## Table 1
## params <- list(from = "ituri",
##                alpha = 3.62,
##                rho = 365.0375,
##                tau = 0.86)

## 07092018 Reformatted report to make it more parameterised.
sources <- c("Guinea", "Liberia", "Sierra Leone")

## Can't call this object params else running render in a loop won't
## work.

params2 <- list(model = "gravity_alt",
               modelpars = list(alpha = 2.01,
                                rho = 72.96,
                                tau = 1.12),
               centroids = "data/Geography/centroids/processed/wafrica_adm0_centroids.csv"
               )

purrr::map(sources, function(x) {
    params2$from <- x
    rmarkdown::render(here::here("reports/relative_risk.Rmd"),
                      params = params2)
 })

outfile_suffix <- paste(sapply(params2$modelpars, paste, collapse=""),
                        collapse = "_")
outfiles <-  paste0("output/flow_from_",
                    sources,
                    "_",
                    outfile_suffix,
                    ".csv")

names(outfiles) <- sources

## Now we are ready to determine importation risk.
wtd_risk_out <- paste0("output/wtd_rel_risk_", outfile_suffix, ".csv")
params_imptn <- list(sources = sources,
                     cases = "data/CaseCounts/processed/HealthMap_Ebola_wide.csv",
                     risk = outfiles,
                     simean = 15.3,
                     sisd = 9.1,
                     R = 1.03,
                     onday = 200,
                     outfile = wtd_risk_out)
rm(params)
rmarkdown::render(here::here("reports/importation_risk.Rmd"),
                  params = params_imptn)

## The quotes/spaces are converted to periods.
## Sort out manually for now and then sort out later.

wtd_risk <- readr::read_csv(here::here("output/wtd_rel_risk_2.01_72.96_1.12.csv"))

idx <- which(wtd_risk$flow_to == "Côte.d.Ivoire")
wtd_risk$flow_to[idx] <- "Côte d'Ivoire"


idx <- which(wtd_risk$flow_to == "Sierra.Leone")
wtd_risk$flow_to[idx] <- "Sierra Leone"

idx <- which(wtd_risk$flow_to == "Guinea.Bissau")
wtd_risk$flow_to[idx] <- "Guinea-Bissau"

readr::write_csv(x = wtd_risk,
                 path = (here::here("output/wtd_rel_risk_2.01_72.96_1.12.csv")))
