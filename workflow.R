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
### does the merging ###
####################################

### order these by dates ###
new_dat <- processDS1(dat, 
                                  spec, 
                                  disease, 
                                  case_type, 
                                  location, 
                                  merge_rule = "median")

####################################
### deal with cumulative number of cases going down ###
####################################

cum_dat <- new_dat[new_dat$Location %in% location,] # cum_dat must have one line per date

### dates for which we want incidence
dates <- seq(min(cum_dat$Date, na.rm = TRUE)-1, max(cum_dat$Date, na.rm = TRUE), 1)

### create initially empty vector cum_incid
cum_incid <- c(0, rep(NA, length(dates)))
### record whether data on one day was missing or not
missing_data <- rep(1, length(dates))
### fill in relevant entries with data from cum_dat
for (i in 1:nrow(cum_dat))
{
  cum_incid[match(cum_dat$Date[i], dates)] <- cum_dat[i,case_type]
  if(!is.na(cum_dat[i,case_type])) missing_data[match(cum_dat$Date[i], dates)] <- 0
}

# recording only dates with non missing data
dates <- c(dates[1], dates[missing_data == 0])
cum_incid <- c(cum_incid[1], cum_incid[missing_data == 0])

### how do we correct cum incidence which goes down ###

# 1) identify pairs t, t+1 so that cumI(t) > cumI(t+1)
# 2) try to remove cumI(t), or cumI(t+1). 
#     If one of those two options, and ONLY one "works", 
#     i.e. makes cumI increasing for all steps from t-1 to t+2
#     then remove (i.e. set to NA and change missing_data to 1) 
#     the datapoint which makes it work when removed
# 3)  if the two options in 2) work, then remove (i.e. set to NA and change missing_data to 1) 
#     both data points (t and t+1), the incidence should be increasing from t-1 to t+2
# 4) if none of the two options in 2) work, then remove (i.e. set to NA and change missing_data to 1) 
#     both data points (t and t+1). Check if resulting incidence is increasing from t-1 to t+2. 
#     If it is increasing from t-1 to t+2, that's it. 
#     If it is not increasing from t-1 to t+2, apply the above rules (1-4)
#     as if t'=t-1 and t'+1=t+2.

# Question: how to deal with issues towards the end of the dataset (i.e. towards present days in real-time)
# If t+1 is the last datapoint with data, do the following:
# 2') try to remove cumI(t), 
#     if this "works", 
#     i.e. makes cumI increasing for from t-1 to t+1
#     then remove t (i.e. set to NA and change missing_data to 1) 
# 3') if the option in 2) did not work, then remove t (i.e. set to NA and change missing_data to 1) anyway
#     and apply the above rules (2'-3')
#     as if t'=t-1 and t'+1=t+1.

# Question: how to deal with issues towards the start of the dataset 
# In order to do this we add a 0 cumI at the start, which will NOT be used apart from for this correction

##############################################################
### 1) identify pairs t, t+1 so that cumI(t) > cumI(t+1)
##############################################################

deal_non_incr_cumI <- function(cum_incid, dates, t1)
{
  t2 <- t1 + 1
  
  # check if t2 is last data point
  if(t2==length(cum_incid)) # rules for last datapoint
  {
    # 2') try to remove cumI(t), 
    
    tmp_cum_incid_no_t1 <- cum_incid # make a copy
    tmp_cum_incid_no_t1 <- cum_incid[-t1]
    # is tmp_cum_incid_no_t1 increasing between t1-1 and t2
    still_pb <- diff(tmp_cum_incid_no_t1[c(t1-1, t2-1)])<0
    
    if(!still_pb)
    {
      #     if this "works", 
      #     i.e. makes cumI increasing for from t-1 to t+1
      #     then remove t (i.e. set to NA and change missing_data to 1) 
      to_remove <- t1
      cum_incid <- cum_incid[-to_remove]
      dates <- dates[-to_remove]
    }else 
    {
      #     then remove t (i.e. set to NA and change missing_data to 1) anyway
      to_remove <- t1
      cum_incid <- cum_incid[-to_remove]
      dates <- dates[-to_remove]
      
      #     and apply the above rules (2'-3')
      #     as if t'=t-1 and t'+1=t+1.
      tmp <- deal_non_incr_cumI(cum_incid, dates, t1-1)
      cum_incid <- tmp$cum_incid
      dates <- tmp$dates
    }
    
  }else # rules for non last datapoint
  {
    tmp_cum_incid_no_t1 <- cum_incid # make a copy
    tmp_cum_incid_no_t2 <- cum_incid # make a copy
    ### 2) try to remove cumI(t), or cumI(t+1). 
    tmp_cum_incid_no_t1 <- cum_incid[-t1]
    tmp_cum_incid_no_t2 <- cum_incid[-t2]
    
    still_pb <- rep(NA, 2)
    # is tmp_cum_incid_no_t1 increasing between t1-1 and t2
    still_pb[1] <- diff(tmp_cum_incid_no_t1[c(t1-1, t2-1)])<0
    # is tmp_cum_incid_no_t2 increasing between t1-1 and t2
    still_pb[2] <- diff(tmp_cum_incid_no_t2[c(t1, t2)])<0
    
    if(sum(still_pb) == 2) # none of the options above worked
    {
      # if none of the two options in 2) work, then remove (i.e. set to NA and change missing_data to 1) 
      #     both data points (t and t+1). Check if resulting incidence is increasing from t-1 to t+2. 
      #     If it is increasing from t-1 to t+2, that's it. 
      #     If it is not increasing from t-1 to t+2, apply the above rules (1-4)
      #     as if t'=t-1 and t'+1=t+2.
      to_remove <- c(t1, t2)
      cum_incid <- cum_incid[-to_remove]
      dates <- dates[-to_remove]
      
      if(diff(cum_incid[c(t1-1, t1)]) < 0)
      {
        tmp <- deal_non_incr_cumI(cum_incid, dates, t1-1)
        cum_incid <- tmp$cum_incid
        dates <- tmp$dates
      } # else do nothing more
      
    }else if (sum(still_pb) == 0) # both options worked
    {
      # if the two options in 2) work, then remove (i.e. set to NA and change missing_data to 1) 
      #     both data points (t and t+1), the incidence should be increasing from t-1 to t+2
      to_remove <- c(t1, t2)
      cum_incid <- cum_incid[-to_remove]
      dates <- dates[-to_remove]
      
    }else # only one of the options worked
    {
      # If one of those two options, and ONLY one "works", 
      #     i.e. makes cumI increasing for all steps from t-1 to t+2
      #     then remove (i.e. set to NA and change missing_data to 1) 
      #     the datapoint which makes it work when removed
      if(still_pb[1])
      {
        to_remove <- t2
        cum_incid <- cum_incid[-to_remove]
        dates <- dates[-to_remove]
      }else
      {
        to_remove <- t1
        cum_incid <- cum_incid[-to_remove]
        dates <- dates[-to_remove]
      }
    }
    
  }
  
  out <- list(cum_incid = cum_incid, dates = dates)
  return(out)
  
}

cum_incid_corrected <- cum_incid
dates_corrected <- dates

incid <- c(0, diff(cum_incid_corrected))
all_ts <- which(incid<0) - 1

while (length(all_ts)>0)
{
  ### TO DO here need to compute t1 and t2 at each iteration from current cum_incid_corrected, as indexes are changing
  ### always call first problem first
  
  t1 <- all_ts[1]
  tmp <- deal_non_incr_cumI(cum_incid_corrected, dates_corrected, t1)
  cum_incid_corrected <- tmp$cum_incid
  dates_corrected <- tmp$dates
  
  incid <- c(0, diff(cum_incid_corrected))
  all_ts <- which(incid<0) - 1
}

### add NAs for dates with no data

dates_final <- seq(min(dates_corrected), max(dates_corrected), 1)
cum_incid_final <- rep(NA, length(dates_final))
for(i in 1:length(dates_final))
{
  idx <- match(dates_final[i], dates_corrected)
  if(length(idx)>0)
    cum_incid_final[i] <- cum_incid_corrected[idx]
}

### check this is doing the right thing:
# plot(dates_corrected, cum_incid_corrected)
# points(dates_final, cum_incid_final, col="blue")

### TO DO: do linear regression to fill in the NAs

cum_incid_final_with_imputations <- cum_incid_final

nas <- which(is.na(cum_incid_final))
end_seq_sep <- nas[c(which(diff(nas)>1), length(nas))]
start_seq_sep <- nas[c(1, which(diff(nas)>1)+1)]

for(i in 1:length(start_seq_sep))
{
  y1 <- cum_incid_final[start_seq_sep[i]-1]
  y2 <- cum_incid_final[end_seq_sep[i]+1]
  n_missing <- end_seq_sep[i] - start_seq_sep[i] + 1
  slope <- (y2-y1)/(n_missing + 1)
  missing <- y1 + slope*(1:n_missing)
  cum_incid_final_with_imputations[start_seq_sep[i]:end_seq_sep[i]] <- missing
}

### check this is doing the right thing:
# plot(dates_final, cum_incid_final_with_imputations, type="l", col="red", xlab="Time", ylab="Cumulative incidence")
# lines(dates_final, cum_incid_final)

### Compute incidence from cumulative incidence

incid_final_with_imputations <- c(0, diff(cum_incid_final_with_imputations))

### visualise incidence
par(mfrow=c(1,1))
plot(dates_final, incid_final_with_imputations, type="l", xlab="Time", ylab="Incidence")
### To do: add our estimate of daily incidence

##############################################################################################################################

par(mfrow=c(2, 2))

pch <- 20
cex <- 0.4

dat_loc <- dat[dat$Location %in% location,]
plot(dat_loc$Date, dat_loc[,case_type], type="p", xlab="Time", ylab="Cumulative incidence", main="Raw data", pch=pch, cex=cex, col="green")

plot(dat_loc$Date, dat_loc[,case_type], type="p", xlab="Time", ylab="Cumulative incidence", main="Merged data", pch=pch, cex=cex, col="green")
points(dates, cum_incid, pch=pch, cex=cex, col="red")

plot(dates, cum_incid, type="p", xlab="Time", ylab="Cumulative incidence", main="Removed points\ninconsistent with increasing cum. incid.", pch=pch, cex=cex, col="red")
points(dates_corrected, cum_incid_corrected, pch=pch, cex=cex,col="blue")

plot(dates_corrected, cum_incid_corrected, type="p", xlab="Time", ylab="Cumulative incidence", main="Linear interpolation\non dates with no data", pch=pch, cex=cex, col="blue")
points(dates_final, cum_incid_final_with_imputations, pch=pch, cex=cex)


### first step: merging ###

par(mfrow=c(1, 2))

pch <- 20
cex <- 1

plot(dat_loc$Date, dat_loc[,case_type], type="p", xlab="Time", ylab="Cumulative incidence", main="Raw data", pch=pch, cex=cex, col="green")

plot(dat_loc$Date, dat_loc[,case_type], type="p", xlab="Time", ylab="Cumulative incidence", main="Merged data", pch=pch, cex=cex, col="green")
points(dates, cum_incid, pch=pch, cex=cex, col="red")

### first step: zoom ###

xlim <- c(as.Date("2014-03-01"), as.Date("2014-10-01"))
ylim=c(0, 3000)

plot(dat_loc$Date, dat_loc[,case_type], type="p", xlab="Time", ylab="Cumulative incidence", main="Raw data", pch=pch, cex=cex, col="green", xlim=xlim, ylim=ylim)

plot(dat_loc$Date, dat_loc[,case_type], type="p", xlab="Time", ylab="Cumulative incidence", main="Merged data", pch=pch, cex=cex, col="green", xlim=xlim, ylim=ylim)
points(dates, cum_incid, pch=pch, cex=cex, col="red")



### second step: remove inconsistent points ###

par(mfrow=c(1, 2))

pch <- 20
cex <- 1

plot(dates, cum_incid, type="p", xlab="Time", ylab="Cumulative incidence", main="Merged data", pch=pch, cex=cex, col="red")

plot(dates, cum_incid, type="p", xlab="Time", ylab="Cumulative incidence", main="Removed points\ninconsistent with increasing cum. incid.", pch=pch, cex=cex, col="red")
points(dates_corrected, cum_incid_corrected, pch=pch, cex=cex, col="blue")

### second step: zoom ###


plot(dates, cum_incid, type="p", xlab="Time", ylab="Cumulative incidence", main="Merged data", pch=pch, cex=cex, col="red", xlim=xlim, ylim=ylim)

plot(dates, cum_incid, type="p", xlab="Time", ylab="Cumulative incidence", main="Removed points\ninconsistent with increasing cum. incid.", pch=pch, cex=cex, col="red", xlim=xlim, ylim=ylim)
points(dates_corrected, cum_incid_corrected, pch=pch, cex=cex, col="blue")


### third step: interpolate ###

par(mfrow=c(1, 2))

pch <- 20
cex <- 1

plot(dates_corrected, cum_incid_corrected, type="p", xlab="Time", ylab="Cumulative incidence", main="Removed points\ninconsistent with increasing cum. incid.", pch=pch, cex=cex, col="blue")

plot(dates_corrected, cum_incid_corrected, type="p", xlab="Time", ylab="Cumulative incidence", main="Interpolated", pch=pch, cex=cex, col="blue")
points(dates_final, cum_incid_final_with_imputations, pch=pch, cex=cex, col="black")

### third step: zoom ###


plot(dates_corrected, cum_incid_corrected, type="p", xlab="Time", ylab="Cumulative incidence", main="Removed points\ninconsistent with increasing cum. incid.", pch=pch, cex=cex, col="blue", xlim=xlim, ylim=ylim)

plot(dates_corrected, cum_incid_corrected, type="p", xlab="Time", ylab="Cumulative incidence", main="Interpolated", pch=pch, cex=cex, col="blue", xlim=xlim, ylim=ylim)
points(dates_final, cum_incid_final_with_imputations, pch=pch, cex=cex, col="black")




##############################################################################################################################

### create list of all locations and all countries ###
locations <- unique(new_dat$Location)
countries <- unique(new_dat$Country)
dates <- seq(range(new_dat$Date)[1], range(new_dat$Date)[2], 1)


### create a plot of cumulative case counts by country ###
i <- 1
plot(dat_by_country[[i]]$Date, dat_by_country[[i]]$Cases, xlab="Date", ylab="Number of cases", col=i, ylim=c(0, max(unlist(new_dat$Cases), na.rm=TRUE)))

for(i in 2:length(countries))
{
  points(dat_by_country[[i]]$Date, dat_by_country[[i]]$Cases, xlab="Date", ylab="Number of cases", col=i)
}

### cases ###
e <- 1
ord <- order(dat_by_country[[e]]$Date)
plot(dat_by_country[[e]]$Date[ord], dat_by_country[[e]]$Cases[ord], type="l", xlab="Date", ylab="Number of cases", main=countries[e])
#ord <- order(dat_by_location[[e]]$Date)
#plot(dat_by_location[[e]]$Date[ord], dat_by_location[[e]]$Cases[ord], type="l", xlab="Date", ylab="Number of cases", main=locations[e])

### potential issues: 
# 1) conflicting data for the same day
#       dat_by_location[[e]][as.character(dat_by_location[[e]]$Date) %in% "2014-05-26",]
# 2) data conflicting with previous data (cumulative numbers should be increasing but are not always)
