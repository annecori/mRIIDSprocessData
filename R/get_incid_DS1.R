#' Compute incidence from data stream 1
#'
#' This function BLABLA
#'
#' @export
#'
#' @author Pierre Nouvellet and Anne Cori
#'
#' @param dat dataframe containing data stream 1 to be processed
#' @param spec string giving species in which disease incidence will be extracted
#' @param disease string specifying disease of interest
#' @param case_type string specifying column name from which incidence is extracted; one of "SCC" (suspected and confirmed cases), "SC" (suspected cases), "CC" (confirmed cases), "SCD" (suspected and confirmed deaths), "SD" (suspected deaths), or "CD" (confirmed deaths). 
#' @param location string specifying location for which incidence will be computed
#' @param merge_rule string to specify how merging should be made when duplicated entries exist. At the moment only "median" is supported. 
#'
#' @return a dataframe containing incidence and cumulative incidence obtained from merged_dat. 
#'
#' @details XXX
#' 
#' @importFrom stats median
#'
#' @examples
#'
#' # to be written
#'

get_incid_DS1 <- function(dat, 
                       spec, 
                       disease, 
                       case_type = c("SCC", "SC", "CC", "SCD", "SD", "CD"), 
                       location, 
                       merge_rule = c("median"))
{
  
  case_type <- match.arg(case_type)
  merge_rule <- match.arg(merge_rule)
  
  ####################################
  ### cols_to_keep contains a list of column names to be merged in case of duplicated entries
  ### if a column name does not exist and is not needed apart from merging, it is ignored. 
  ####################################
  
  cols_to_keep = c("Location", "Country", "Disease", "Species", 
                   "HealthMap.Alert.ID", "Headline", "URL", 
                   "Alert.Tag", "Feed.Name", "Lon", "Lat")
  cols_to_keep <- c(cols_to_keep, "Date", "Cases") # these are columns we generate ourselves later on
  
  ####################################
  ### select appropriate species and disease, 
  ### and adds a column "Cases' with appropriate case definition. 
  ####################################
  
  dat <- check_dat(dat, 
                   spec, 
                   disease, 
                   case_type, 
                   location)
  
  ####################################
  ### identify unique entries based on date and country columns
  ####################################
  
  dat$DateCountry <- paste0(as.character(dat$Date), dat$Country)
  uniq_dat <- unique(dat$DateCountry)
  
  ####################################
  ### merge duplicated entries where necessary
  ####################################
  
  ### create a processed dataset ###
  new_dat <- as.data.frame(matrix(NA, length(uniq_dat), length(cols_to_keep)))
  names(new_dat) <- cols_to_keep
  ### making sure classes are consistent with original dataset (useful for dates in particular)
  for(j in 1:length(cols_to_keep))
  {
    class(new_dat[,j]) <- class(dat[, cols_to_keep[j]])
  }
  ### merging entries ###
  for(i in 1:length(uniq_dat))
  {
    new_dat[i,] <- merge_dup_lines_DS1(dat[which(dat$DateCountry %in% uniq_dat[i]),], cols_to_keep, rule = merge_rule)
  }
  
  ### order these by dates ###
  new_dat <- new_dat[order(as.numeric(new_dat$Date)), ]
  
  out <- compute_inc_with_corrections_DS1(new_dat)
  
  return(out)
  
}
