                                        # For each country in our dataset, we do the following analysis
                                        # 1. Convert the raw data into a clean incidence object
                                        # 2. Determine the relative risk for each location with the focal point as Sierra Leone. This step can be carried out independently as it does not depend on the incidence data but only the populations and the distance data.
                                        # 3. Use p.spread to project forward, plot and compare.

library(devtools)
library(magrittr)
library(ggplot2)
library(EpiEstim)
source("useful.R")
devtools::load_all()

species <- "Humans"
disease <- "Ebola"
focal.location <- "Sierra Leone"
### for now looking at suspected and confirmed cases ###
case.type <- "SCC"


## Gravity model parameters ##

pow_N_from <- 1
pow_N_to <- 1
pow_dist <- 2
K <- 1

flow.dir <- "data/Geography/GravityModel/processed/estimated_flow_from_to_gravity_model_powers_Nfrom_"
flow.file <- paste0(flow.dir, pow_N_from, "_Nto_", pow_N_to,
                                      "_dist_", pow_dist,
                                      ".csv")
flow.matrix <- read.csv(flow.file)
countries_names <- flow.matrix[,1]

# Other params
p.stay <- 0.05

# params for estimating reproduction number
# specify a serial interval distribution
mean_SI <- 14.2
# from http://www.nejm.org/doi/suppl/10.1056/NEJMc1414992/suppl_file/nejmc1414992_appendix.pdf
CV_SI <- 9.6 / 14.2
SItrunc <- 40
SI_Distr <- sapply(0:SItrunc, function(e) DiscrSI(e, mean_SI, mean_SI * CV_SI))
SI_Distr <- SI_Distr / sum(SI_Distr)

# specify time windows for R estimation
time_window <- 7 * 7


# projections
T_proj <- 7 * c(21, 43, 64)
T_sim  <- 7 * 7



relative.risk <- function(flow.to, tot.flow.from){
    flow.to/tot.flow.from
}

p.spread <- function(p.stay, rel.risk){
    (1 - p.stay) * rel.risk
}

healthmap <- "data/CaseCounts/raw/HealthMap_Ebola_GNE_WHO.csv" %>%
              read.csv(stringsAsFactors = FALSE)

by.location <- split(healthmap, healthmap$Country)



lapply(by.location, function(case.count){
    location <- case.count$Country[1]
    case.count %<>%  incidence.from.DS1(species, disease,
                                         case.type,
                                         location,
                                         merge_rule = "median")

    outfile <- paste(species, disease, location, "processed", sep="_") %>%
               paste(".pdf", sep="")

    p <- ggplot(case.count, aes(Date, Cases)) + geom_point()
    ggsave(outfile, p)

                                        # temporary workaround
    case.count %<>% .[complete.cases(.),]
                                        # Estimate reproduction number

    start <- 3:(length(case.count$Date) - time_window)
    end   <- start + time_window
    res   <- EstimateR( case.count$incid , T.Start=start , T.End=end ,
                     method="NonParametricSI", SI.Distr= SI_Distr ,
                     plot=FALSE , CV.Posterior=1 , Mean.Prior=1 , Std.Prior=0.5)
    res$R <- cbind(res$R, case.count$Date[res$R$T.End])
    names(res$R) <- c(names(res$R)[1:11],'dates')

    new_i <- as.incidence(matrix(case.count$incid, length(case.count$incid), 1), case.count$Date)
    new_i$dates <- case.count$Date
    new_i$counts <- matrix(case.count$incid, nrow(case.count),1)
    new_i$timespan <- as.numeric( diff(range( new_i$dates, na.rm = TRUE))) + 1
    new_i$n <- sum( new_i$counts)

                                        # + 1 because the first column consists of country names.
    from <- which(countries_names %in% focal.location) + 1
    tot.flow.from <- flow.matrix[, from] %>% sum(na.rm=TRUE)
    to  <-  which(countries_names %in% location)
    flow.to <- flow.matrix[to, from]
    rel.risk <- relative.risk( flow.to, tot.flow.from)
    if(is.na( rel.risk)) rel.risk <- 0
    p.spread <- p.spread(p.stay, rel.risk)
    projection <- get_projection(T_proj , T_sim  , new_i  , res, p.spread)

    outfile <- paste0("output/", location, "_projection.png")
    png(outfile, width=1600,height=1000,res=200)
     get_plot_weekly(new_i, T_proj, projection, res )
    dev.off()


})
