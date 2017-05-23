
#' Compute incidence from data stream 1 (after duplicate entries have been merged). 
#' This function corrects for decreasing cumulative case numbers and missing data. 
#'
#' @export
#'
#' @author Pierre Nouvellet and Anne Cori
#'
#' @param merged_dat dataframe containing data with no duplicates i.e. one entry per date
#'
#' @return a dataframe containing incidence and cumulative incidence obtained from merged_dat. 
#'
#' @details ### how do we correct cum incidence which goes down ###
#' 1) identify pairs t, t+1 so that cumI(t) > cumI(t+1)
#' 2) try to remove cumI(t), or cumI(t+1). 
#'     If one of those two options, and ONLY one "works", 
#'     i.e. makes cumI increasing for all steps from t-1 to t+2
#'     then remove (i.e. set to NA and change missing_data to 1) 
#'     the datapoint which makes it work when removed
#' 3)  if the two options in 2) work, then remove (i.e. set to NA and change missing_data to 1) 
#'     both data points (t and t+1), the incidence should be increasing from t-1 to t+2
#' 4) if none of the two options in 2) work, then remove (i.e. set to NA and change missing_data to 1) 
#'     both data points (t and t+1). Check if resulting incidence is increasing from t-1 to t+2. 
#'     If it is increasing from t-1 to t+2, that's it. 
#'     If it is not increasing from t-1 to t+2, apply the above rules (1-4)
#'     as if t'=t-1 and t'+1=t+2.

#' Question: how to deal with issues towards the end of the dataset (i.e. towards present days in real-time)
#' If t+1 is the last datapoint with data, do the following:
#' 2') try to remove cumI(t), 
#'     if this "works", 
#'     i.e. makes cumI increasing for from t-1 to t+1
#'     then remove t (i.e. set to NA and change missing_data to 1) 
#' 3') if the option in 2) did not work, then remove t (i.e. set to NA and change missing_data to 1) anyway
#'     and apply the above rules (2'-3')
#'     as if t'=t-1 and t'+1=t+1.

#' Question: how to deal with issues towards the start of the dataset 
#' In order to do this we add a 0 cumI at the start, which will NOT be used apart from for this correction

#'
#' @examples
#'
#' #
#'

compute_inc_with_corrections_DS1 <- function(merged_dat) 
{
  
  #######################################
  ### create a vector of cumulative incidence
  #######################################
  
  ### dates for which we want incidence
  dates <- seq(min(merged_dat$Date, na.rm = TRUE)-1, max(merged_dat$Date, na.rm = TRUE), 1) # starting one day before the first entry so that cumulative incidence on that day is zero
  
  ### create initially empty vector cum_incid
  cum_incid <- c(0, rep(NA, length(dates)))
  ### record whether data on one day was missing or not
  missing_data <- rep(1, length(dates))
  ### fill in relevant entries with data from merged_dat
  for (i in 1:nrow(merged_dat))
  {
    cum_incid[match(merged_dat$Date[i], dates)] <- merged_dat[i,"Cases"]
    if(!is.na(merged_dat[i,"Cases"])) missing_data[match(merged_dat$Date[i], dates)] <- 0
  }
  
  # recording only dates with non missing data
  dates <- c(dates[1], dates[missing_data == 0]) # even though no cases recorded on dates[1] we want this to have the initial cumulative incidence zero. 
  cum_incid <- c(cum_incid[1], cum_incid[missing_data == 0])
  
  
  #######################################
  ### identify where this cumulative incidence is decreasing and removing data points accordingly
  #######################################
  
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
  
  #######################################
  ### do a linear regression on cumulative incidence to fill in the NAs
  #######################################
  
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
  
  ### Compute incidence from cumulative incidence
  
  incid_final_with_imputations <- c(0, diff(cum_incid_final_with_imputations))
  
  out <- data.frame(dates = dates_final, incid = incid_final_with_imputations, cum_incid = cum_incid_final_with_imputations)
  
  return(out)
  
}


