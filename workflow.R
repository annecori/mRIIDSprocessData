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
filename <- "~/Dropbox/mRIIDS/data/CaseCounts/raw/HealthMap_Ebola_GNE_WHO.csv"

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
### read in ProMED data ###
####################################

### file in which data is stored ###
filename2 <- "~/Dropbox/mRIIDS/data/CaseCounts/raw/Copy of ProMED_Ebola_2014-2016 Curation.csv"

dat2 <- read.csv(filename2, stringsAsFactors = FALSE)

process_names_promed_dat <- function(dat)
{
  names(dat)[names(dat) %in% "HM.Alert.ID"] <- "HealthMap.Alert.ID"
  names(dat)[names(dat) %in% "Cumulative.SC"] <- "SC"
  names(dat)[names(dat) %in% "Cumulative.SD"] <- "SD"
  names(dat)[names(dat) %in% "Cumulative.CC"] <- "CC"
  names(dat)[names(dat) %in% "Cumulative.CD"] <- "CD"
  
  return(dat)
}

process_issue_dates_promed_dat <- function(dat)
{
  tmp <- unlist(strsplit(dat$Issue.Date, " "))
  dates_m_d_y <- as.character(format(as.Date(tmp[seq(1, 2*nrow(dat), 2)], format="%d/%m/%y"), "%m/%d/%y"))
  times <- tmp[seq(2, 2*nrow(dat), 2)]
  dat$Issue.Date <- paste(dates_m_d_y, times)
  
  return(dat)
}

dat2 <- process_names_promed_dat(dat2)
dat2 <- process_issue_dates_promed_dat(dat2)

####################################
### test get_incid_DS1 function ###
####################################

incid2 <- get_incid_DS1(dat2, 
                        spec, 
                        disease, 
                        case_type, 
                        location, 
                        merge_rule = "median")



### visualise incidence
plot(incid2$dates, incid2$cum_incid, type="l", xlab="Time", ylab="Cumulative Incidence", xlim= c(as.Date("2014-01-01"), as.Date("2016-12-12")))
plot(incid2$dates, incid2$incid, type="l", xlab="Time", ylab="Incidence", xlim= c(as.Date("2014-01-01"), as.Date("2016-12-12")))

dat2 <- mRIIDSprocessData:::check_dat(dat2, 
                                      spec, 
                                      disease, 
                                      case_type, 
                                      location)
points(dat2$Date, dat2$New.SC + dat2$New.SD, col="red")
### To do: add our estimate of daily incidence
