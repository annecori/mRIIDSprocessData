library(devtools)
library(magrittr)
library(ggplot2)

devtools::load_all()

species <- "Humans"
disease <- "Ebola"
case_type <- "SCC"

healthmap <- 'data/CaseCounts/raw/HealthMap_Ebola_GNE_WHO.csv' %>%
                read.csv(stringsAsFactors = FALSE)

by.location <- split(healthmap, healthmap$Country)

                                        # For each country in our dataset, we do the following analysis
                                        # 1. Convert the raw data into a clean incidence object
                                        # 2. Determine the relative risk for each location with the focal point as Sierra Leone. This step can be carried out independently as it does not depend on the incidence data but only the populations and the distance data.
                                        # 3. Use p.spread to project forward, plot and compare.

lapply(by.location, function(case.count){

    outfile <- paste(species, disease, case.count$Country[1], "raw", sep="_") %>%
               paste(".pdf", sep="")

    p <- ggplot(case.count, aes()) + geom_point()
    ggsave(outfile, p)

    case.count %<>%  incidence.from.DS1(species, disease,
                                         case.type,
                                         location,
                                         merge_rule = "median")

    outfile <- paste(species, disease, case.count$Country[1], "processed", sep="_") %>%
               paste(".pdf", sep="")

    p <- ggplot(case.count, aes(Date, Cases)) + geom_point()
    ggsave(outfile, p)

})
