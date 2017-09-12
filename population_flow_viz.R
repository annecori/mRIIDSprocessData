library(magrittr)
adm0_centroids <- "data/Geography/GravityModel/raw/adm0_centroids.tsv" %>%
                   read.csv(stringsAsFactors = FALSE, sep = "\t", header = FALSE)

names(adm0_centroids) <- c("country", "id", "lon", "lat", "pop")

distances <- geosphere::distm(adm0_centroids[,c('lon', 'lat')])
distances <- distances[lower.tri(distances)] # Extract the distances vector
pairs     <- nrow(adm0_centroids) %>% combn(2)
n_from    <- adm0_centroids[pairs[1,], 'pop']
n_to      <- adm0_centroids[pairs[2,], 'pop']


flow.matrix           <-  matrix(NA, nrow(adm0_centroids), nrow(adm0_centroids))
rownames(flow.matrix) <- adm0_centroids$country
colnames(flow.matrix) <- adm0_centroids$country

## fill in the matrix from the vectors
flow_from_to <- flow_vector(n_from, n_to, distances, K=K,
                            pow_N_from = pow_N_from,
                            pow_N_to = pow_N_to,
                            pow_dist = pow_dist)
flow.matrix[lower.tri(flow.matrix)] <- flow_from_to
flow.matrix <- t(flow.matrix) # fill out the upper triangle

flow_to_from <- flow_vector(n_to, n_from, distances, K=K,
                            pow_N_from = pow_N_from,
                            pow_N_to = pow_N_to,
                            pow_dist = pow_dist)
flow.matrix[lower.tri(flow.matrix)] <- flow_to_from # fill out the lower triangle

## Relative risk
relative.risk    <- flow.matrix / rowSums(flow.matrix, na.rm=TRUE)
from.SierraLeone <- relative.risk["Sierra Leone", ]
from.SL.df       <- data.frame(Country = adm0_centroids$country, Relative.Risk = from.SierraLeone)



library(rgeos)
library(ggplot2)


africa <- maptools::readShapeSpatial("data/Geography/Africa_SHP/Africa.shp") %>%
            broom::tidy(region = "COUNTRY")

## Clean up the country names so that we get a better match.
clean_names <- function(countries){
    countries %>%
        stringi::stri_trans_general("latin-ascii") %>%
        tolower %>%
        gsub(" ", "", ., fixed = TRUE)
}
africa$id          %<>% clean_names
from.SL.df$Country %<>% clean_names

## Furthermore, some names are different between the two sets.
## africa$id[which(! africa$id %in% from.SL.df$Country)] %>% unique
## [1] "canarias"           "congo"              "congodrc"
## [4] "coted'ivoire"       "madeira"            "saotomeandprincipe"
## grep("con", from.SL.df$Country)
## [1] 47 48
## > from.SL.df[47,]
##                                                      Country Relative.Risk
## Democratic Republic of the Congo democraticrepublicofthecongo    0.02016558
## > from.SL.df[48,]
##                          Country Relative.Risk
## Republic of Congo republicofcongo   0.001213046
## and so on.

from.SL.df[47, "Country"] <- "congodrc"
from.SL.df[48, "Country"] <- "congo"
from.SL.df[45, "Country"] <- "coted'ivoir"

## For some reason, relative risk from a country to itself is NA.
## Forcing it to be 1 for now.
from.SL.df[200, "Relative.Risk"] <- 1

merged <- dplyr::left_join(africa, from.SL.df, by = c("id" = "Country"))
out    <- merged[complete.cases(merged), ]
out    <- dplyr::mutate(out, ln_risk = log(Relative.Risk))
p      <- ggplot() + geom_map(data = out, aes(map_id = id, fill = ln_risk),
                         map = africa) + expand_limits(x = africa$long, y = africa$lat)
p <- p + scale_fill_gradient2(low = scales::muted("red"),
                              mid = "white", midpoint = -5,
                              high = scales::muted("blue"),
                              limits = c(-10, 0),
                              breaks = c(-10, -5, 0),
                              labels = c("Low Risk", "" ,"High Risk"),
                              name = "")
p <- p + theme_minimal() + theme(line = element_blank(),
                                 axis.title = element_blank(),
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank())

ggsave("relative-risk.png", p)
