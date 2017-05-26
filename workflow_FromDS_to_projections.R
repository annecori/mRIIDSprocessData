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
source('useful.R')
# projections
T_proj <- 7*c(21,43,64)
T_sim <- 7*7

# transform incidence to incidence object
new_i <- as.incidence(matrix(incid$incid,length(incid$incid),1), incid$dates)
new_i$dates <- incid$dates
new_i$counts <- matrix(incid$incid,nrow(incid),1)
new_i$timespan <- as.numeric(diff(range(new_i$dates, na.rm = TRUE))) + 1
new_i$n <- sum(new_i$counts)


projection <- get_projection(T_proj , T_sim  , new_i  , res )

####################################
###  Plot data and projections   ###
####################################

png("Proj1.png",width=1600,height=1000,res=200)
get_plot_weekly(new_i, T_proj, projection, res )
dev.off()

####################################
###  Plot data and projections   ###
####################################

file <- 'estimated_flow_from_to_gravity_model_powers_Nfrom_1_Nto_1_dist_'
pth <- '../../../Dropbox (SPH Imperial College)/mRIIDS/data/Geography/GravityModel/processed/'

############# for 1 table
Scenario_proj <- 1:length(T_proj) # related to which projection period

get_table_summary <- function(Scenario_proj){
  scenario_grav <- c(1,2) # 2 scenario with power 1 or 2 for gravity
  p_travel <- c(1,5)*1e-2 # proba of traveling during SI
  
  summary_tab <- matrix(NA,10+2,4*length(p_travel))
  
  # total projected cases
  T_proj_case <- colSums(projection[[Scenario_proj]])
  
  for (sg in scenario_grav){
    flow <- read.csv(paste0(pth,file,sg,'pn.csv'), stringsAsFactors = FALSE)
    countries_names <- flow[,1]
    
    # relative flow
    rel_prob <- flow[,which(countries_names %in% location) + 1]/
      sum(flow[,which(countries_names %in% location) + 1],na.rm=TRUE)
    rel_prob[is.na(rel_prob)] <- 0
    risk <- data.frame(country=flow[,1],rel_prob=rel_prob)
    risk <- risk[order(risk$rel_prob,decreasing=TRUE),]
    
    summary_tab[(1:10)+2,(1:2)+(sg-1)*(2+length(p_travel))] <- as.matrix(risk[1:10,])
    
    export_n <- rep(NA,10)
    for (j in 1:length(p_travel)){ # different probas of travel
      N_travel <- rbinom(1e3,T_proj_case,p_travel[j])
      N_travel_country <- matrix(NA,nrow(risk),1e3)
      for (l in 1:1e3) N_travel_country[,l] <- rmultinom(1,N_travel[l],risk$rel_prob)
      Q <- rbind(rowSums(N_travel_country)/1e3,
                 apply(N_travel_country,1,quantile,c(.025,.975)))
      for (m in 1:10) export_n[m] <- paste0(round(Q[1,m]*10)/10,' (',
                                            round(Q[2,m]*10)/10,
                                            ';',round(Q[3,m]*10)/10,')')
      
      summary_tab[(1:10)+2,2+j+(sg-1)*(2+length(p_travel))] <- export_n
      
    }
    
  }
  summary_tab[1,c(1,5)] <- c('Power of gravity model: 1','Power of gravity model: 2')
  summary_tab[2,] <- c('Country','Relative risk','Exportation 1','Exportation 2',
                       'Country','Relative risk','Exportation 1','Exportation 2')
  return(summary_tab)
}

for (i in Scenario_proj){
  summary_tab <- get_table_summary(i)
  write.csv(file=paste0('summary_projection_period_',i,'.csv'),summary_tab)
}

