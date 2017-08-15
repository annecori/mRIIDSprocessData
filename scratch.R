library(devtools)
library(magrittr)
library(ggplot2)
library(EpiEstim)
#source("useful.R")
devtools::load_all()

## Collect the incidence count for all locations

species   <- "Humans"
disease   <- "Ebola"
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

## Even at this point we have several NAs because for any given location
## we don't have data for all locations.
## Fortunately the dates are regularly spaced even after the above step.

by.location %<>% `[`(complete.cases(.), )

## Now divide the dataset into training and validation sets.
validation  <-   utils::tail(by.location, n = 10L)
by.location %<>% utils::head(n = -10L)

by.location[, c("Date", grep("incid", names(by.location), value = TRUE))] %>%
    reshape2::melt(id.vars = c("Date")) %>%
    ggplot(aes(Date, value, color = variable)) + geom_point()


## Determine the flow matrix for the countries of interest only.
adm0_centroids <- "data/Geography/GravityModel/raw/adm0_centroids.tsv" %>%
                   read.csv(stringsAsFactors = FALSE, sep = "\t", header = FALSE) %>%
                   dplyr::filter(V1 %in% w.africa)
names(adm0_centroids) <- c("country", "id", "lon", "lat", "pop")

distances <- geosphere::distm(adm0_centroids[,c('lon', 'lat')])
distances <- distances[lower.tri(distances)] # Extract the distances vector
pairs     <- nrow(adm0_centroids) %>% combn(2)
n_from    <- adm0_centroids[pairs[1,],'pop']
n_to      <- adm0_centroids[pairs[2,],'pop']

## Gravity model parameters ##
pow_N_from <- 1
pow_N_to   <- 1
pow_dist   <- 2
K          <- 1

flow.matrix           <-  matrix(NA, nrow(adm0_centroids), nrow(adm0_centroids))
rownames(flow.matrix) <- adm0_centroids$country
colnames(flow.matrix) <- adm0_centroids$country

## fill in the matrix from the vectors
flow_from_to <- flow_vector(n_from, n_to, distances, K=K,
                            pow_N_from=pow_N_from,
                            pow_N_to=pow_N_to,
                            pow_dist=pow_dist)
flow.matrix[lower.tri(flow.matrix)] <- flow_from_to
flow.matrix <- t(flow.matrix) # fill out the upper triangle

flow_to_from <- flow_vector(n_to, n_from, distances, K=K,
                            pow_N_from=pow_N_from,
                            pow_N_to=pow_N_to,
                            pow_dist=pow_dist)
flow.matrix[lower.tri(flow.matrix)] <- flow_to_from # fill out the lower triangle

## Relative risk
relative.risk <- flow.matrix %>%
                 apply(1, function(row) row / sum(row, na.rm=TRUE))

## matrix characterising the population movement between geographical units
n.countries <- w.africa %>% length
p.stay      <- 0.05 # this can be a vector
p.mat       <- matrix(0, nrow = n.countries, ncol = n.countries)
p.mat[lower.tri(p.mat)] <- mapply(rep, 1 - p.stay, (n.countries - 1):1) %>%
                            unlist

p.mat                   <- t(p.mat)
p.mat[lower.tri(p.mat)] <- p.mat[upper.tri(p.mat)]
diag(p.mat)             <- p.stay
p.movement              <- relative.risk * p.mat
diag(p.movement)        <- p.stay

## params for estimating reproduction number
## specify a serial interval distribution
mean_SI <- 14.2
# from http://www.nejm.org/doi/suppl/10.1056/NEJMc1414992/suppl_file/nejmc1414992_appendix.pdf
CV_SI    <- 9.6 / 14.2
SItrunc  <- 40
SI_Distr <- sapply(0:SItrunc, function(e) DiscrSI(e, mean_SI, mean_SI * CV_SI))
SI_Distr <- SI_Distr / sum(SI_Distr)


## Replacing the instantaneous update of r_t with
## a stable estimate. r.t is a vector of reproduction
## numbers. r.t[1:t.proj] will be replaced by r.t[t.proj]
stabilise.r.t <-  function(r.t, t.proj){

    stable <- seq(from = t.proj, to = nrow(r.t), by = t.proj)
    for(i in stable) r.t[(i - t.proj + 1):i, ] <- r.t[i, ]
    return(r.t)
}

## Estimate the reproduction number matrix
time_window <- 7
start       <- 1:(length(by.location$Date) - time_window)
end         <- start + time_window
t.proj      <- 21

## r.j.t contains estimates of the reproduction rate at times
## from 1 through to the (last date - time_window)
r.j.t <- by.location[, grep("incid", names(by.location))] %>%
         apply(2, function(incid) {
                  res <- EstimateR(incid, T.Start = start , T.End = end,
                         method = "NonParametricSI",
                         SI.Distr = SI_Distr,
                         plot = FALSE ,
                         CV.Posterior = 1 ,
                         Mean.Prior = 1 ,
                         Std.Prior = 0.5)
                  r.t <- res$R %>% stabilise.r.t(t.proj) %>%
                                    apply(1, function(r){
                                       shape <- r["Mean(R)"]^2 / r["Std(R)"]^2
                                       scale <- r["Std(R)"]^2 / r["Mean(R)"]
                                       return(rgamma(1, shape = shape,
                                                     scale = scale))})
                  return(r.t) }) %>%
           as.data.frame

colnames(r.j.t) <- grep("incid", names(by.location), value = TRUE) %>%
                    lapply(function(s){
                        s %>%
                        strsplit(split = "[.]") %>%
                        unlist  %>%
                       `[`(1)}) %>% unlist

r.j.t$Date      <- by.location[end, "Date"]

## unfortunate hack to get things going
r.j.t %<>%
    .[complete.cases(.), ]

r.j.t %>% reshape2::melt(id.vars = "Date") %>%
    dplyr::rename(Country = variable, R_t = value) %>%
        ggplot(aes(Date, R_t)) + geom_point() + facet_grid(. ~ Country)

## At this point, all the pieces are in place.
## by.location contains the incidence count
## r.j.t contains the estimates of reproduction numbers.
## p.movement conatins the probabilities.
## SI_Distr is the serial interval distribution.
## The model is: lambda.j.t = p.movement * (incidence * r_t) * serial_interval
## taking care of the dimensions of course.
n.locations     <- length(w.africa)
common.dates    <- which(by.location$Date %in% r.j.t$Date)
incidence.count <- by.location[common.dates, grep("incid", names(by.location))]
date.col        <-  names(r.j.t) %in% "Date"
incidence.count %<>% `*`(r.j.t[, !date.col])

n.dates.sim     <- 11
t.max           <- nrow(incidence.count) + n.dates.sim - 1
ws              <- c(SI_Distr, rep(0, t.max - length(SI_Distr) + 1)) %>% rev

incidence.proj           <- matrix(0, nrow = n.dates.sim, ncol = n.locations)
colnames(incidence.proj) <- grep("incid", names(by.location), value = TRUE)
incidence.proj %<>% rbind(incidence.count)


lambda.j        <-  t(p.movement) %*% t(incidence.proj) %*% matrix(ws, ncol = length(ws), nrow = length(ws))
incidence.proj[1 + nrow(incidence.count):t.max, ] <- lambda.j %>%
                                                      t %>%
                                                      `[`((1 + nrow(incidence.count):t.max), ) %>%
                                                         apply(c(1, 2), function(l) rpois(1, l))

incidence.proj$Date <- by.location$Date[common.dates] %>%
                               c(seq(max(.) + 1, length.out = n.dates.sim, by = 1))

projected   <- incidence.proj[1 + nrow(incidence.count):t.max, ]
projected$provenance <- "Projected"
validation %<>% `[`(colnames(projected))
validation$provenance <- "Validation"

validation %>%
    rbind(projected) %>%
    reshape2::melt(id.vars=c("Date", "provenance")) %>%
    ggplot(aes(Date, value, color = provenance)) + geom_point() + facet_grid(variable ~ .)
