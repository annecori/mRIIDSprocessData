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

### file in which data is stored ###
filename <- "~/Dropbox/mRIIDS/data/CaseCounts/raw/HealthMap_Ebola_GNE_WHO.csv"

####################################
### read in data ###
####################################

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
