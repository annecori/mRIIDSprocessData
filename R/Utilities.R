# Sum with na.rm = TRUE but if all nas, return NA instead of 0
#

sum_only_na_stays_na <- function(x)
{
  if(all(is.na(x))) out <- NA else out <- sum(x, na.rm = TRUE)
  return(out)
}


# Gets appropriate case counts off dataframe
#

get_cases <- function(dat, 
                      case_type = c("SCC", "SC", "CC", "SCD", "SD", "CD"))
{
  
  case_type <- match.arg(case_type)
  
  if(case_type %in% "SCC")
  {
    col <- apply(dat[, c("SC", "CC")], 1, sum_only_na_stays_na)
  }else if(case_type %in% "SC")
  {
    col <- dat$SC
  }else if(case_type %in% "CC")
  {
    col <- dat$CC
  }else if(case_type %in% "SCD")
  {
    col <- apply(dat[, c("SD", "CD")], 1, sum_only_na_stays_na)
  }else if(case_type %in% "SD")
  {
    col <- dat$SD
  }else if(case_type %in% "CD")
  {
    col <- dat$CD
  }
  
  return(col)
}


# selects appropriate species and disease, and adds a column "Cases' with appropriate case definition. 
#

check_dat <- function(dat, 
                      spec, 
                      disease, 
                      case_type = c("SCC", "SC", "CC", "SCD", "SD", "CD"), 
                      location)
{
  
  ####################################
  ### add checks on input arguments ###
  ####################################
  
  case_type <- match.arg(case_type)
  
  if(!("Disease" %in% names(dat)))
  {
    stop("dat should have a column 'Disease'.")
  }
  
  if(!("Species" %in% names(dat)))
  {
    stop("dat should have a column 'Species'.")
  }
  
  if(!("Issue.Date" %in% names(dat)))
  {
    stop("dat should have a column 'Issue.Date'.")
  }
  
  if(!("Location" %in% names(dat)))
  {
    stop("dat should have a column 'Location'.")
  }
  
  if(!("Country" %in% names(dat)))
  {
    stop("dat should have a column 'Country'.")
  }
  
  ####################################
  ### select disease / species / location ###
  ####################################
  
  ### select only disease of interest ###
  dat <- dat[dat$Disease %in% disease,]
  
  ### select only species of interest ###
  dat <- dat[dat$Species %in% spec,]
  
  
  ### select only species of interest ###
  dat <- dat[dat$Location %in% location,]
  
  ####################################
  ### deal with dates ###
  ####################################
  
  ### create dates without time from Issue.Date
  dat$Date <- as.Date(unlist(strsplit(dat$Issue.Date, " "))[seq(1, 2*nrow(dat), 2)], format="%m/%d/%y")
  
  ####################################
  ### Create column called Cases which comprises all relevant cases to be counted in incidence ###
  ####################################
  
  dat$Cases <- get_cases(dat, case_type)
  
  return(dat)
}

# function to create a merged entry, separated by a slash
# 
paste_single_col <- function(col_dup)
{
  out <- paste(col_dup, collapse=" / ")
  return(out)
}


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