library(devtools)
library(magrittr)
library(ggplot2)
library(EpiEstim)
source("useful.R")
devtools::load_all()

## params for estimating reproduction number
## specify a serial interval distribution
mean_SI <- 14.2
# from http://www.nejm.org/doi/suppl/10.1056/NEJMc1414992/suppl_file/nejmc1414992_appendix.pdf
CV_SI <- 9.6 / 14.2
SItrunc <- 40
SI_Distr <- sapply(0:SItrunc, function(e) DiscrSI(e, mean_SI, mean_SI * CV_SI))
SI_Distr <- SI_Distr / sum(SI_Distr)



## Collect the incidence count for all locations

species <- "Humans"
disease <- "Ebola"
case.type <- "SCC"

healthmap <- "data/CaseCounts/raw/HealthMap_Ebola_GNE_WHO.csv" %>%
               read.csv(stringsAsFactors = FALSE)

w.africa <- healthmap$Country %>% unique

by.location <- healthmap %>%
               split(.$Country) %>%
              lapply(function(case.count){
                   location <- case.count$Country[1]
                   case.count %<>%  incidence.from.DS1(species, disease,
                                                      case.type,
                                                      location,
                                                     merge_rule = "median")
                    colnames(case.count) <- c("Date", paste0(location,".Cases"),
                                      paste0(location, ".incid"))
                   return(case.count)}) %>%
                Reduce(function(d1, d2) dplyr::left_join(d1, d2, by="Date"), .)


## Determine the flow matrix for the countries of interest only.
adm0_centroids <- "data/Geography/GravityModel/raw/adm0_centroids.tsv" %>%
                   read.csv(stringsAsFactors = FALSE, sep = "\t", header = FALSE) %>%
                   dplyr::filter(V1 %in% w.africa)
names(adm0_centroids) <- c("country", "id", "lon", "lat", "pop")

distances <- geosphere::distm(adm0_centroids[,c('lon', 'lat')])
distances <- distances[lower.tri(distances)] # Extract the distances vector
pairs <-  nrow(adm0_centroids) %>% combn(2)
n_from <- adm0_centroids[pairs[1,],'pop']
n_to <- adm0_centroids[pairs[2,],'pop']

## Gravity model parameters ##
pow_N_from <- 1
pow_N_to <- 1
pow_dist <- 2
K <- 1

flow.matrix <-  matrix(NA, nrow(adm0_centroids), nrow(adm0_centroids))
rownames(flow.matrix) <- adm0_centroids$country
colnames(flow.matrix) <- adm0_centroids$country

                                        # fill in the matrix from the vectors
flow_from_to <- flow_vector(n_from, n_to, distances, K=K, pow_N_from=pow_N_from, pow_N_to=pow_N_to, pow_dist=pow_dist)
flow.matrix[lower.tri(flow.matrix)] <- flow_from_to
flow.matrix <- t(flow.matrix) # fill out the upper triangle

flow_to_from <- flow_vector(n_to, n_from, distances, K=K, pow_N_from=pow_N_from, pow_N_to=pow_N_to, pow_dist=pow_dist)
flow.matrix[lower.tri(flow.matrix)] <- flow_to_from # fill out the lower triangle

## Relative risk
relative.risk <- flow.matrix %>%
                 apply(1, function(row) row / sum(row, na.rm=TRUE))

## Estimate the reproduction number matrix
time_window <- 7
start       <- 3:(length(by.location$Date) - time_window)
end         <- start + time_window
res         <- healthmap %>% apply(2, function(col) EstimateR(col, T.Start = start , T.End = end, method = "NonParametricSI", SI.Distr = SI_Distr, plot = FALSE , CV.Posterior = 1 , Mean.Prior = 1 , Std.Prior = 0.5)

by.location <- healthmap %>%
               split(.$Country) %>%


## matrix characterising the population movement between geographical units
n.countries <- w.africa %>% length
p.stay      <- 0.05 # this can be a vector
p.mat       <- matrix(0, nrow = n.countries, ncol = n.countries)
p.mat[lower.tri(p.mat)] <- mapply(rep, 1 - p.stay, (n.countries - 1):1) %>%
                            unlist

p.mat <- t(p.mat)
p.mat[lower.tri(p.mat)] <- p.mat[upper.tri(p.mat)]
diag(p.mat) <- p.stay



p.movement <- relative.risk * p.mat

# time to simulate for
t.sim <- 7

for(t in 1:t.sim){
    for(j in 1:n.countries){

    }
}
