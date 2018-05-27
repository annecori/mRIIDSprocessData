## First create the relative risk profile using gravity model alone
## Then use the epicurve to weight the profiles. This step requires
## SI mean and sd.
## Then create the map
library(dplyr)
params <- list(pow_dist = 0.42,
               K = 0.000409735,
               pow_N_from = 2.42,
               pow_N_to = -0.002,
               from = "équateur")

here::here("reports", "relative_risk.Rmd") %>%
    rmarkdown::render(params = params)
outfile1 <- paste0("flow_from_",
                   params$from,
                   "_",
                   params$pow_dist,
                   ".csv")
params$from <- "mbandaka"
here::here("reports", "relative_risk.Rmd") %>%
    rmarkdown::render(params = params)

outfile2 <- paste0("flow_from_",
                   params$from,
                   "_",
                   params$pow_dist,
                   ".csv")


params <- list(cases = "22_may_2018.csv",
               risk = c(outfile1, outfile2),
               simean = 14,
               sisd = 1)
here::here("reports", "importation_risk.Rmd") %>%
    rmarkdown::render(params = params)


outfile <- purrr::map_chr(params$risk,
                          function(x) stringr::str_replace(x ,
                                                           ".csv",
                                                           "")) %>%
    paste(collapse = "_")
outfile <- paste0(outfile, ".csv")
params <- list(relrisk = outfile)
here::here("reports", "risk_profile_map.Rmd") %>%
    rmarkdown::render(params = params)


