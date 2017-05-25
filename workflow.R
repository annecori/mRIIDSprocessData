rm(list=ls())

library(devtools)
install_github("annecori/mRIIDSprocessData")
library(mRIIDSprocessData)



##############################################################################################################################
##############################################################################################################################
################################################## PROCESSING DATA STREAM 1 ##################################################
##############################################################################################################################
##############################################################################################################################

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



##############################################################################################################################
##############################################################################################################################
################################################## PROCESSING DATA STREAM 3 ##################################################
##############################################################################################################################
##############################################################################################################################

require(geosphere)

####################################
### read in country specific lat, lon and pop estimates (for whole of Africa) ###
####################################

### file in which data is stored ###
filename <- "~/Dropbox/mRIIDS/data/Geography/GravityModel/raw/adm0_centroids.tsv"

dat3 <- read.csv(filename, stringsAsFactors = FALSE, sep = "\t", header = FALSE)
names(dat3) <- c("country", "id", "lon", "lat", "pop")

####################################
### Choose parameters of the gravity model ###
####################################

pow_N_from <- 1
pow_N_to <- 1
pow_dist <- 1 # 2

####################################
### Compute flows using the gravity model ###
####################################

flow_from_to <- matrix(NA, nrow(dat3), nrow(dat3))
row.names(flow_from_to) <- dat3$country
colnames(flow_from_to) <- dat3$country

for(i in 2:nrow(dat3))
{
  print(i)
  for(j in 1:(i-1))
  {
    dist_i_j <- as.numeric(distm(dat3[i,c('lon','lat')], dat3[j,c('lon','lat')]))
    flow_from_to[i,j] <- get_gravity_model_flow_DS3(dat3[i,'pop'], dat3[j,'pop'], dist_i_j, pow_N_from = pow_N_from, pow_N_to = pow_N_to, pow_dist = pow_dist)
    flow_from_to[j,i] <- get_gravity_model_flow_DS3(dat3[j,'pop'], dat3[i,'pop'], dist_i_j, pow_N_from = pow_N_from, pow_N_to = pow_N_to, pow_dist = pow_dist)
  }
}

### write output so don't have to rerun every time
write.csv(flow_from_to, file = paste0("~/Dropbox/mRIIDS/data/Geography/GravityModel/processed/estimated_flow_from_to_gravity_model_powers_Nfrom_",pow_n_from,
                                      "_Nto_",pow_n_to,
                                      "_dist_",pow_dist,
                                      ".csv"), quote = FALSE)

### look for maximum flow
max_flow <- max(flow_from_to, na.rm = TRUE)
which_max_flow <- which(flow_from_to == max_flow, arr.ind = TRUE)
dat3$country[which_max_flow[1,]]
dat3$country[which_max_flow[2,]]
# --> with pow_dist = 1, largest estimated flow is between China and India (here flows are symetric as we have assumed the same power parameter for both population sizes)
# --> with pow_dist = 2, largest estimated flow is between Republic of Congo and Democratic Republic of the Congo (here flows are symetric as we have assumed the same power parameter for both population sizes)

### look for top destinations from Sierra Leone
top_destinations_from_SL <- sort(flow_from_to[dat3$country %in% "Sierra Leone",], decreasing = TRUE)
head(names(top_destinations_from_SL), 10)
# --> with pow_dist = 1, top 10 desinations are 
# "India"                  "China"                  "Nigeria"                            "Guinea"           "Brazil" 
# "United States"          "C\xf4te d'Ivoire"       "Democratic Republic of the Congo"   "Pakistan"         "Mali" 

# --> with pow_dist = 2, top 10 desinations are 
# "Guinea"           "Nigeria"          "C\xf4te d'Ivoire" "Liberia"          "Mali"             "Senegal"          
# "Ghana"            "India"           "Burkina Faso"     "Brazil"   

