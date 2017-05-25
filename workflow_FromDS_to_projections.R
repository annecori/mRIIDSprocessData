rm(list=ls())

library(devtools)
install_github("annecori/mRIIDSprocessData")
library(mRIIDSprocessData)

###############################
### arguments that user may want to change
###############################

spec <- "Humans"
disease <- "Ebola"

### for now looking at suspected and confirmed cases ###
case_type <- "SCC"

### location of interest ###
location <- "Sierra Leone"

####################################
### read in Healthmap data ###
####################################

### file in which data is stored ###
# filename <- "~/Dropbox/mRIIDS/data/CaseCounts/raw/HealthMap_Ebola_GNE_WHO.csv"
filename<- '~/../Dropbox (SPH Imperial College)/mRIIDS/data/CaseCounts/raw/HealthMap_Ebola_GNE_WHO.csv'

dat <- read.csv(filename, stringsAsFactors = FALSE)

####################################
### test get_incid_DS1 function ###
####################################

incid <- get_incid_DS1(dat, 
                       spec, 
                       disease, 
                       case_type, 
                       location, 
                       merge_rule = "median")



### visualise incidence
par(mfrow=c(2,2))
plot(incid$dates, incid$cum_incid, type="l", xlab="Time", ylab="Cumulative Incidence", xlim= c(as.Date("2014-01-01"), as.Date("2016-12-12")))
plot(incid$dates, incid$incid, type="l", xlab="Time", ylab="Incidence", xlim= c(as.Date("2014-01-01"), as.Date("2016-12-12")))
### To do: add our estimate of daily incidence

##############################################################################################################################

####################################
### Estimate Reproudction Number  ###
####################################
library('EpiEstim')

# specify a serial interval distribution
mean_SI <- 14.2
CV_SI <- 9.6/14.2 # from http://www.nejm.org/doi/suppl/10.1056/NEJMc1414992/suppl_file/nejmc1414992_appendix.pdf
SItrunc <- 40
SI_Distr <- sapply(0:SItrunc, function(e) DiscrSI(e,mean_SI,mean_SI*CV_SI) )
SI_Distr <- SI_Distr / sum(SI_Distr)

# specify time windows for R estimation
time_window<-7*7
Start <- 3:(length(incid$dates)-time_window) 
End <- Start+time_window

#estimate the reproduction number over time
res <- EstimateR( incid$incid , T.Start=Start , T.End=End , 
                  method="NonParametricSI", SI.Distr= SI_Distr ,
                  plot=FALSE , CV.Posterior=1 , Mean.Prior=1 , Std.Prior=0.5)
res$R <- cbind(res$R,incid$dates[res$R$T.End])
names(res$R) <- c(names(res$R)[1:11],'dates')

####################################
###   Project case forward       ###
####################################
install_github("reconhub/projections")
install_github("reconhub/incidence")
library(projections)
library(incidence)

T_proj <- 100
T_sim <- 2*7

# samples for R
shape <- res$R$`Mean(R)`[T_proj]^2 / res$R$`Std(R)`[T_proj]^2
scale <- res$R$`Std(R)`[T_proj]^2 / res$R$`Mean(R)`[T_proj]
R_samples <- rgamma(1e3,shape=shape,scale=scale)

# transform incidence to incidence object
new_i <- as.incidence(incid$incid, incid$dates)

# prjoections
project_1 <- project(x = new_i[1:T_proj], R = R_samples , si = SI_Distr, n_sim = 1e3, n_days = T_sim,
                     R_fix_within = TRUE)

plot(new_i[1:(T_proj+T_sim)], proj = project_1)
