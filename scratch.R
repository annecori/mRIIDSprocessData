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


## Gravity model parameters ##
pow_N_from <- 1
pow_N_to <- 1
pow_dist <- 2
K <- 1

flow.dir <- "data/Geography/GravityModel/processed/estimated_flow_from_to_gravity_model_powers_Nfrom_"
flow.file <- paste0(flow.dir, pow_N_from, "_Nto_", pow_N_to,
                                      "_dist_", pow_dist,
                                      ".csv")
flow.matrix <- read.csv(flow.file) %>% apply(1, as.numeric)
countries_names <- flow.matrix[,1]

relative.risk <- flow.matrix[, -1] %>%
   apply(1, function(row) row / sum(row, na.rm=TRUE))

n.countries <- flow.matrix %>% nrow

# Other params.
p.stay <- 0.05 # this can be a vector
p.mat  <- matrix(0, nrow = n.countries, ncol = n.countries)
p.mat[lower.tri(p.mat)] <- mapply(rep, 1 - p.stay, (n.countries - 1):1) %>%
                            unlist

p.mat <- t(p.mat)
p.mat[lower.tri(p.mat)] <- p.mat[upper.tri(mat)]
diag(p.mat) <- p.stay


## matrix characterising the population movement between geographical units
p.movement <- relative.risk * p.mat

## Collect the incidence count for all locations

species <- "Humans"
disease <- "Ebola"
case.type <- "SCC"

healthmap <- "data/CaseCounts/raw/HealthMap_Ebola_GNE_WHO.csv" %>%
              read.csv(stringsAsFactors = FALSE) %>%
              split(.$Country) %>%
              lapply(function(case.count){
                 location <- case.count$Country[1]
                 case.count %<>%  incidence.from.DS1(species, disease,
                                                      case.type,
                                                      location,
                                                       merge_rule = "median")}) %>%
                  Reduce(function(d1, d2) left_join(d1, d2, by="Date"), .)


## Estimate the reproduction number matrix
start <- 3:(length(healthmap$Date) - time_window)
end   <- start + time_window
res   <- healthmap %>% apply(2, function(col) EstimateR(col, T.Start = start , T.End = end ,
                       method = "NonParametricSI", SI.Distr = SI_Distr ,
                       plot = FALSE , CV.Posterior = 1 , Mean.Prior = 1 , Std.Prior = 0.5)


# time to simulate for
t.sim <- 7

for(t in 1:t.sim){
    for(j in 1:n.countries){

    }
}
