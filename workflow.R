rm(list=ls())

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
par(mfrow=c(1,1))
plot(incid$dates, incid$incid, type="l", xlab="Time", ylab="Incidence")
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

dat2 <- process_names_promed_dat(dat2)

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
par(mfrow=c(1,1))
plot(incid2$dates, incid2$incid, type="l", xlab="Time", ylab="Incidence")
### To do: add our estimate of daily incidence
