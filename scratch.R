library(magrittr)
library(ggplot2)
library(EpiEstim)
#source("useful.R")
devtools::load_all()

## Parameters for incidence data stream processing
## Collect the incidence count for all locations
species   <- "Humans"
disease   <- "Ebola"
case.type <- "SCC"

## Parameters needed for projection.
## The time from which we will project forward.
t.proj      <- 133L
n.sim       <- 1000L    # Number of simulations to run
n.dates.sim <- 28L      # The time period over which projection will be made.


## Parameters for estimating reproduction number
## specify a serial interval distribution
# from http://www.nejm.org/doi/suppl/10.1056/NEJMc1414992/suppl_file/nejmc1414992_appendix.pdf
mean_SI     <- 14.2
CV_SI       <- 9.6 / 14.2
SItrunc     <- 40
SI_Distr    <- sapply(0:SItrunc, function(e) DiscrSI(e, mean_SI, mean_SI * CV_SI))
SI_Distr    <- SI_Distr / sum(SI_Distr)
time_window <- 7 * 7

## Parameters for processing the spatial data.
## Gravity model parameters
pow_N_from <- 1
pow_N_to   <- 1
pow_dist   <- 1
K          <- 1


healthmap <- "data/CaseCounts/raw/HealthMap_Ebola_GNE_WHO.csv" %>%
               read.csv(stringsAsFactors = FALSE)

## Before we do anything else, we will plot the raw data.
## healthmap.raw <- healthmap %>%
##                  dplyr::filter(Species == species, Disease == disease) %>%
##                  dplyr::select(Issue.Date, SC, SD, CC, CD, Country)
##
## healthmap.raw$Issue.Date %<>% as.Date(format = "%m/%d/%y")
## healthmap.raw %>%
##     split(.$Country) %>%
##     lapply(function(case.count){
##         out <- paste(case.count$Country[1], "raw.pdf", sep = "-")
##         p   <- case.count %>%
##                 reshape2::melt(id.vars = c("Issue.Date", "Country")) %>%
##                 dplyr::rename(Case.Type = variable, Count = value) %>%
##                 ggplot(aes(Issue.Date, Count, color = Case.Type)) + geom_point() + theme_minimal()
##         ggsave(out, p)})
## ## End of plotting raw data

w.africa    <- healthmap$Country %>% unique

by.location <- healthmap %>%
                split(.$Country) %>%
                lapply(function(case.count){
                    location <- case.count$Country[1]
                    case.count %<>%  incidence.from.DS1(species, disease,
                                                      case.type,
                                                      location,
                                                     merge_rule = "median")
                    colnames(case.count) <- c("Date", paste0(location, ".Cases"),
                                      paste0(location, ".incid"))
                   return(case.count)}) %>%
                Reduce(function(d1, d2) dplyr::left_join(d1, d2, by="Date"), .)

## Even at this point we have several NAs because for a given date,
## we don't have data for all locations.
## Fortunately the dates are regularly spaced even after the step below.

by.location %<>% `[`(complete.cases(.), )


## Subset the incidence counts. we are not going to use any other column
## except date which we will grab from by.location data frame
by.location.incidence <- by.location[, grep("incid", names(by.location))]
colnames(by.location.incidence) %<>%
                          lapply(function(s){
                                       strsplit(s, split = "[.]") %>%
                                       unlist  %>%
                                       `[`(1)}) %>% unlist %>%
                                       gsub(" ", "", ., fixed = TRUE)
# by.location.incidence %<>% cbind(Date = by.location$Date, .)

## Using incidence count to estimate reproduction number.
start     <- 1:(length(by.location$Date) - time_window)
end       <- start + time_window
end.dates <- by.location[end, "Date"]
r.estim   <- by.location.incidence  %>%
                   plyr::alply(2, .dims = TRUE, function(incid) {
                                                 res <- EstimateR(incid[, 1], T.Start = start , T.End = end,
                                                                  method = "NonParametricSI",
                                                                  SI.Distr = SI_Distr,
                                                                  plot = FALSE ,
                                                                  CV.Posterior = 1 ,
                                                                  Mean.Prior = 1 ,
                                                                  Std.Prior = 0.5)
                                                 res$R %<>% cbind(Date = end.dates)
                                                 return(res$R)})

## Plotting estimate of R for each country
r.estim %>%
    dplyr::bind_rows(.id = 'Location') %>%
    ggplot(aes(Date, `Mean(R)`, color = Location)) + geom_point() + theme_minimal()
## End of plotting R

## We assume that the reproduction number remains unchanged for the time
## period over which we wish to project. For each location, distribution of
## r_t at t.proj is r_t over the next n.days.sim.
r.j.t <- r.estim %>%
           lapply(function(R){
                     cutoff <- which(R$Date %in% by.location[t.proj, "Date"])
                     shape  <- R[cutoff, "Mean(R)"]^2 / R[cutoff, "Std(R)"]^2
                     scale  <- R[cutoff, "Std(R)"]^2 / R[cutoff, "Mean(R)"]
                     return(rgamma(n.sim, shape = shape,
                                          scale = scale))}) %>% data.frame

colnames(r.j.t) <- colnames(by.location.incidence)

r.j.t %>% cbind(Index = 1:n.sim, .) %>%
    reshape2::melt(id.vars = "Index") %>%
    dplyr::rename(Country = variable, R_t = value) %>%
        ggplot(aes(Index, R_t)) + geom_point() + facet_grid(. ~ Country)


## Determine the flow matrix for the countries of interest only.
adm0_centroids <- "data/Geography/GravityModel/raw/adm0_centroids.tsv" %>%
                   read.csv(stringsAsFactors = FALSE, sep = "\t", header = FALSE) %>%
                   dplyr::filter(V1 %in% w.africa)
names(adm0_centroids) <- c("country", "id", "lon", "lat", "pop")
flow.matrix           <- flow_matrix(longitude = adm0_centroids[, "lon"],
                                     latitude  = adm0_centroids[, "lat"],
                                     population = adm0_centroids[, "pop"],
                                     place.names = adm0_centroids[, "country"],
                                     model = "gravity",
                                     K = K, pow_N_from = pow_N_from,
                                     pow_N_to = pow_N_to, pow_dist = pow_dist)


## Relative risk
relative.risk <- flow.matrix / rowSums(flow.matrix, na.rm=TRUE)

## matrix characterising the population movement between geographical units
p.stay      <- 0.99 # this can be a vector
p.movement  <- probability_movement(relative.risk, p.stay)



## At this point, all the pieces are in place.
## by.location contains the incidence count
## r.j.t contains the estimates of reproduction numbers.
## p.movement conatins the probabilities.
## SI_Distr is the serial interval distribution.
## The model is: lambda.j.t = p.movement * (incidence * r_t) * serial_interval
## taking care of the dimensions of course.
## Now divide the dataset into training and validation sets.
validation      <- by.location %>%
                      utils::tail(n = nrow(.) - t.proj)
n.locations     <- length(w.africa)
incidence.count <- by.location.incidence[1:t.proj, ]
dates.all       <- by.location[1:t.proj, "Date"] %>%
                       c(seq(max(.) + 1, length.out = n.dates.sim, by = 1))
t.max           <- nrow(incidence.count) + n.dates.sim - 1
##ws            <- c(SI_Distr, rep(0, t.max - length(SI_Distr) + 1)) %>% rev

## Each row of r.j.t is a set of reproduction numbers at each
## location for one simulation.
daily.projections <- plyr::alply(r.j.t, 1, function(r.t){
                                       incid <- as.matrix(incidence.count)
                                       r.t   <- as.matrix(r.t)
                                       out   <- project(incid, r.t, SI_Distr, p.movement, n.dates.sim)
                                       colnames(incid) <- colnames(incidence.count)
                                       incidence.proj  <- rbind(incidence.count, out)
                                       incidence.proj %<>% cbind(Date = dates.all)
                                       return(incidence.proj[(nrow(incidence.count) + 1):t.max, ])})



daily.to.weekly    <- function(daily){
    weeks  <- cut(daily$Date, breaks="1 week")
    weekly <- split(daily, weeks) %>%
                 plyr::ldply(function(d) colSums(d[, names(d) != "Date"])) %>%
                dplyr::rename(Date = .id)
    return(weekly)

}

weekly.projections <- lapply(daily.projections, daily.to.weekly) %>% dplyr::bind_rows(.)


## For each country, we want to plot the training data, the validation data
## and a polygon spanned by the 2.5% and 97.5% quantiles.
##cols.to.keep     <- grep("incid", names(by.location), value = TRUE) %>% c("Date")
training         <- cbind(Date = by.location$Date[1:t.proj], by.location.incidence[1:t.proj, ])
validation       <- cbind(Date = by.location$Date[(t.proj + 1):nrow(by.location.incidence)],
                          by.location.incidence[(t.proj + 1):nrow(by.location.incidence), ])
weekly.available <- c(training    = list(training),
                       validation = list(validation)) %>%
                       lapply(daily.to.weekly) %>%
                       dplyr::bind_rows(.id = "Category")


plot.weekly <- function(available, projection){
    available$Date %<>% as.Date
    p     <- ggplot(available, aes_string("Date",
                                          colnames(available)[3],
                                          color = "Category")) + geom_point()
    ci.95 <- projection     %>%
              split(.$Date) %>%
              plyr::ldply(. %>% `[`(, 2)
                            %>% quantile(probs = c(0.5, 0.025, 0.975))) %>%
                                dplyr::rename(Date = .id)
    x <- ci.95$Date %>% c(rev(.)) %>% as.Date
    y <- c(ci.95[, 3], rev(ci.95[ , 4]))

    p   <- p + geom_polygon(data = data.frame(x = x, y = y), aes(x, y, alpha = 0.01, color = "red"))
    p   <- p + theme_minimal() + theme(legend.position="none")
    return(p)

}

## cols.to.keep <- grep("incid", names(by.location), value = TRUE) %>%
##                                  gsub(" ", "", ., fixed = TRUE)
## colnames(weekly.available)   %<>% gsub(" ", "", ., fixed = TRUE)
## colnames(weekly.projections) %<>% gsub(" ", "", ., fixed = TRUE)

plots.list   <- lapply(colnames(by.location.incidence), function(location){
                        available  <- weekly.available[, c("Date", "Category", location)]
                        projection <- weekly.projections[, c("Date", location)]
                        plot.weekly(available, projection)})

p <- cowplot::plot_grid(plots.list[[1]],
                        plots.list[[2]],
                        plots.list[[3]],
                        plots.list[[4]],
                        plots.list[[5]],
                        plots.list[[6]])

cowplot::save_plot("project-28-111.png", p)
