rm(list=ls())
library(magrittr)
#library(devtools)
#install_github("annecori/mRIIDSprocessData")
#library(mRIIDSprocessData)
# For now we will source all files
source("R/compute_inc_with_corrections_DS1.R")
source("R/get_gravity_model_flow_DS3.R")
source("R/get_incid_DS1.R")
source("R/merge_dup_lines_DS1.R")
source("R/Utilities.R")
source("R/spatial_processing.R")

##############################################################################################################################
##############################################################################################################################
################################################## PROCESSING DATA STREAM 1 ##################################################
##############################################################################################################################
##############################################################################################################################

###############################
### arguments that user may want to change
###############################

species <- "Humans"
disease <- "Ebola"

### for now looking at suspected and confirmed cases ###
case.type <- "SCC"

### location of interest ###
location <- "Sierra Leone"

####################################
### read in Healthmap data ###
####################################

### file in which data is stored ###
case.count <- "data/CaseCounts/raw/HealthMap_Ebola_GNE_WHO.csv" %<>%
  read.csv(stringsAsFactors = FALSE)

####################################
### test get_incid_DS1 function ###
####################################

incid <- incidence.from.DS1(case.count = case.count,
                       species,
                       disease,
                       case.type,
                       location,
                       merge_rule = "median")

# visualise incidence. works like a charm
library(ggplot2)
p1 = ggplot(incid, aes(dates, incid)) + geom_line() + xlab("Time") + ylab("Incidence")
p2 = ggplot(incid, aes(dates, cum_incid)) + geom_line() + xlab("Time") + ylab("Cumulative Incidence")
multiplot(p1, p2, cols=2)

##par(mfrow=c(2,2))
##plot(incid$dates, incid$cum_incid, type="l", xlab="Time", ylab="Cumulative Incidence", xlim= c(as.Date("2014-01-01"), as.Date("2016-12-12")))
##plot(incid$dates, incid$incid, type="l", xlab="Time", ylab="Incidence", xlim= c(as.Date("2014-01-01"), as.Date("2016-12-12")))
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
adm0_centroids <- "data/Geography/GravityModel/raw/adm0_centroids.tsv" %>%
                   read.csv(stringsAsFactors = FALSE, sep = "\t", header = FALSE)
names(adm0_centroids) <- c("country", "id", "lon", "lat", "pop")




distances <- geosphere::distm(adm0_centroids[,c('lon', 'lat')])
distances <- distances[lower.tri(distances)] # Extract the distances vector
pairs <-  nrow(adm0_centroids) %>% combn(2)
n_from <- adm0_centroids[pairs[1,],'pop']
n_to <- adm0_centroids[pairs[2,],'pop']


####################################
### Choose parameters of the gravity model ###
####################################

pow_N_from <- 1
pow_N_to <- 1
pow_dist <- 1 # 2


                                        # another way of calculating the flow matrix
flow_from_to <- flow_vector(n_from, n_to, distances, pow_N_from, pow_N_to, pow_dist)
flow_to_from <- flow_vector(n_to, n_from, distances, pow_N_from, pow_N_to, pow_dist)

                                        # fill in the matrix from the vectors
flow_matrix <-  matrix(NA, nrow(adm0_centroids), nrow(adm0_centroids))
rownames(flow_matrix) <- adm0_centroids$country
colnames(flow_matrix) <- adm0_centroids$country
flow_matrix[lower.tri(flow_matrix)] <- flow_from_to
flow_matrix <- t(flow_matrix) # fill out the upper triangule
flow_matrix[lower.tri(flow_matrix)] <- flow_to_from # fill out the lower triangle

### write output so don't have to rerun every time
write.csv(flow_from_to, file = paste0("data/Geography/GravityModel/processed/sb_estimated_flow_from_to_gravity_model_powers_Nfrom_",pow_N_from,
                                      "_Nto_",pow_N_to,
                                      "_dist_",pow_dist,
                                      ".csv"), quote = FALSE)

### look for maximum flow
max_flow <- max(flow_matrix, na.rm = TRUE)
which_max_flow <- which(flow_matrix == max_flow, arr.ind = TRUE)
adm0_centroids$country[which_max_flow[1,]]
adm0_centroids$country[which_max_flow[2,]]
# --> with pow_dist = 1, largest estimated flow is between China and India (here flows are symetric as we have assumed the same power parameter for both population sizes)
# --> with pow_dist = 2, largest estimated flow is between Republic of Congo and Democratic Republic of the Congo (here flows are symetric as we have assumed the same power parameter for both population sizes)

### look for top destinations from Sierra Leone
top_destinations_from_SL <- sort(flow_matrix[adm0_centroids$country %in% "Sierra Leone",], decreasing = TRUE)
head(names(top_destinations_from_SL), 10)
# --> with pow_dist = 1, top 10 desinations are
# "India"                  "China"                  "Nigeria"                            "Guinea"           "Brazil"
# "United States"          "C\xf4te d'Ivoire"       "Democratic Republic of the Congo"   "Pakistan"         "Mali"

# --> with pow_dist = 2, top 10 desinations are
# "Guinea"           "Nigeria"          "C\xf4te d'Ivoire" "Liberia"          "Mali"             "Senegal"
# "Ghana"            "India"           "Burkina Faso"     "Brazil"

