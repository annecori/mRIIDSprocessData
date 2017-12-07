library(magrittr)
library(ggplot2)
library(dplyr)

devtools::load_all()

adm2_centroids <- "data/Geography/GravityModel/raw/adm2.txt" %>%
                   read.csv(stringsAsFactors = FALSE,
                            sep = "\t",
                            header = TRUE) %>%
                  filter(ADM0 == "Liberia")


adm2_centroids$ADM2  %<>%
    gsub(' ', '', .) %<>%
    toupper %<>%
    factor

## We will focus on the flow from one district (KAILAHUN) to all others.
## Arrange the districts in order of increasing distance from KAILAHUN.
longitude  <- adm2_centroids[, "Centroid_Lon"]
latitude   <- adm2_centroids[, "Centroid_Lat"]
distances  <- geosphere::distm(cbind(longitude, latitude)) %>% `[`(1, )
sortingperm <- order(distances)

adm2_centroids %<>% `[`(sortingperm, )
distances %<>% `[`(sortingperm)


#adm2_centroids %<>% filter(ADM2 %in% c("BO", "KAILAHUN"))

exponent  <- seq(1, 3, by = 1)
probs     <- c(0.7, 0.8, 0.9, 1)

params    <- expand.grid(exponent, probs)
pmovements <- plyr::alply(params, 1,
                          function(row){
                            pow_dist <- row$Var1
                            p.stay   <- row$Var2
                            flow.matrix  <- flow_matrix(
                                               longitude = adm2_centroids[, "Centroid_Lon"],
                                               latitude  = adm2_centroids[, "Centroid_Lat"],
                                               population = adm2_centroids[, "Pop"],                                                             place.names = adm2_centroids[, "ADM2"],
                                               model = "gravity",
                                               K = 1,
                                               pow_N_from = 1,
                                               pow_N_to   = 1,
                                               pow_dist   = pow_dist)
                            relative.risk <- flow.matrix / rowSums(flow.matrix, na.rm=TRUE)
                            p <- probability_movement(relative.risk, p.stay) %>% `[`(1, )
                            data.frame(distance = distances, pmove = p)})

labels <- apply(params, 1, function(r) paste0("alpha = ",r[1], " , p.stay =  ",r[2]))
pmovements %>%
    bind_rows(.id = "params") %>%
    ggplot(aes(distance, pmove, colour = params)) +
    geom_point() +
    scale_colour_discrete(labels = labels)


## We can see that the p movement values are too small to be distinguishable.
library(scales)
pmovements %>%
    bind_rows(.id = "params") %>% filter(params %in% c(1, 2, 3)) %>%
    ggplot(aes(distance, pmove, colour = params)) +
    geom_point() +
    scale_colour_discrete(labels = labels) +
    scale_y_continuous(trans=log10_trans())
