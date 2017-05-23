
#' Merge duplicated lines from data stream 1
#'
#' This function BLABLA
#'
#' @export
#'
#' @author Pierre Nouvellet and Anne Cori
#'
#' @param dat_dup dataframe containing duplicated lines to be merged (could contain a single row)
#' @param cols_to_keep vector of column names to appear in merged dataset
#' @param rule string to specify how merging should be made. At the moment only "median" is supported. 
#'
#' @details XXX
#'
#'@importFrom stats median
#'
#' @examples
#'
#' #
#'

merge_dup_lines_DS1 <- function(dat_dup, cols_to_keep, rule = c("median")) 
{
  
  rule <- match.arg(rule)
  
  if(!all(cols_to_keep %in% names(dat_dup)))
  {
    stop("dat_dup should have names containing all elements of cols_to_keep.")
  }
  if(!("HealthMap.Alert.ID" %in% names(dat_dup)))
  {
    stop("dat_dup should have a column 'HealthMap.Alert.ID'.")
  }
  if(!("Cases" %in% names(dat_dup)))
  {
    stop("dat_dup should have a column 'Cases'.")
  }
  
  ### to start with, create an output based on first entry
  out <- dat_dup[1, cols_to_keep] 
  
  if(nrow(dat_dup)>1) # only necessary to merge duplicates if there are >1 entry
  {
    ### apply corrections to get correct answer
    # by definition location and country are the same for all entries
    # keep all HealthMap alert IDs
    if("HealthMap.Alert.ID" %in% cols_to_keep)
      out$HealthMap.Alert.ID <- paste_single_col(dat_dup$HealthMap.Alert.ID)
    # same for Headline
    if("Headline" %in% cols_to_keep)
      out$Headline <- paste_single_col(dat_dup$Headline)
    # same for URL
    if("URL" %in% cols_to_keep)
      out$URL <- paste_single_col(dat_dup$URL)
    # Alert.Tag, same as HealthMap.Alert.ID
    if("Alert.Tag" %in% cols_to_keep)
      out$Alert.Tag <- paste_single_col(dat_dup$Alert.Tag)
    # Feed.Name, same as HealthMap.Alert.ID
    if("Feed.Name" %in% cols_to_keep)
      out$Feed.Name <- paste_single_col(dat_dup$Feed.Name)
    # For Longitude and Latitude, just checking these are the same across duplicates and issuing a warning if not
    # in any case, keeping the first of all duplicated entries
    if("Lon" %in% names(dat_dup))
    {
      if(length(unique(dat_dup$Lon))>1)
      {
        msg <- paste0("All duplicated entries do not have same longitude. ", 
                      "Issue is with duplicates including HealthMap alert ID ", dat_dup$HealthMap.Alert.ID[1])
        warning(msg)
      }
    }
    if("Lat" %in% names(dat_dup))
    {
      if(length(unique(dat_dup$Lat))>1)
    {
      msg <- paste0("All duplicated entries do not have same latitude ", 
                    "Issue is with duplicates including HealthMap alert ID ", dat_dup$HealthMap.Alert.ID[1])
      warning(msg)
      }
    }
    
    
    # by definition Date are the same for all entries
    # Cases: applying rule
    if(rule == "median")
      out$Cases <- median(dat_dup$Cases, na.rm = TRUE)
  }
  
  return(out)
}


