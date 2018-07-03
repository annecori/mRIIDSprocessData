## First create the relative risk profile using gravity model alone
## Then use the epicurve to weight the profiles. This step requires
## SI mean and sd.
## Then create the map
library(dplyr)
params <- list(from = "équateur",
               alpha = 1.70,
               rho = 38.47,
               tau = 0.91)
## params$pow_N_from <- alpha
## params$pow_N_to <- beta
## params$pow_dist <- gamma
here::here("reports", "relative_risk.Rmd") %>%
    rmarkdown::render(params = params)
outfile_suffix <- paste(sapply(params, paste, collapse=""),
                        collapse = "_")
outfile1 <-  paste0("pmovement_from_",
                    outfile_suffix,
                    ".csv")

params$from <- "mbandaka"
here::here("reports", "relative_risk.Rmd") %>%
    rmarkdown::render(params = params)
outfile_suffix <- paste(sapply(params, paste, collapse=""),
                        collapse = "_")
outfile2 <-  paste0("pmovement_from_",
                    outfile_suffix,
                    ".csv")

params <- list(cases = "21-May-2018.csv",
               risk = c(outfile1, outfile2),
               simean = 15.3,
               sisd = 9.1,
               R = 1.03,
               pmove = c(eq = outfile1,
                         mb = outfile2))
here::here("reports", "importation_risk.Rmd") %>%
    rmarkdown::render(params = params)



## Read in the files and determine quantiles.
## flow_from_équateur_0.649743898585439_1.36075410223566_2.58955122099724_flow_from_mbandaka_0.649743898585439_1.36075410223566_2.58955122099724.csv
outfile2 <- paste0("flow_from_équateur_",
                   gamma,
                   "_",
                   alpha,
                   "_",
                   beta,
                   "_flow_from_mbandaka_",
                   gamma,
                   "_",
                   alpha,
                   "_",
                   beta,
                   ".csv")

import_risk <- purrr::map_dfr(outfile2, function(x)
    readr::read_csv(here::here("output", x)))

import_risk <- import_risk %>%
    group_by(flow_to, adm0) %>%
    summarise(relrisk_qtile =
                  enquantile(wtd_rel_risk, c(0.25, 0.5, 0.75), na.rm = TRUE)) %>%
    tidyr::unnest() %>%
    tidyr::spread(quantile, value)

readr::write_csv(x = import_risk, path = here::here("output",
                                                    "importation_risk.csv"))

# outfile <- purrr::map_chr(params$risk,
#                          function(x) stringr::str_replace(x ,
#                                                           ".csv",
#                                                           "")) %>%
#    paste(collapse = "_")
# outfile <- paste0(outfile, ".csv")
# params <- list(relrisk = outfile)
# here::here("reports", "risk_profile_map.Rmd") %>%
#    rmarkdown::render(params = params)


