#' Compute incidence from data stream 1
#'
#' This function serves as a wrapper around the cleaning and
#' processing of incidence data. The functions for these are
#' available in ...
#'
#' @export
#'
#' @author Pierre Nouvellet, Anne Cori and Sangeeta Bhatia
#'
#' @param case_count dataframe containing data stream 1 to be
#' processed
#' @param species string giving species in which disease
#' incidence will be extracted
#' @param disease string specifying disease of interest
#' @param case_type string specifying column name from which
#' incidence is extracted; one of "SCC" (suspected and
#' confirmed cases), "SC" (suspected cases), "CC" (confirmed
#' cases), "SCD" (suspected and confirmed deaths), "SD"
#' (suspected deaths), or "CD" (confirmed deaths).
#' @param location string specifying location for which
#' incidence will be computed
#' @param merge_rule string to specify how merging should be
#' made when duplicated entries exist. At the moment only
#' "median" is supported.
#'
#' @return a dataframe containing incidence and cumulative
#' incidence obtained from merged_dat.
#'
#' @details XXX
#'
#' @importFrom stats median
#'
#' @examples
#'
#' # to be written
#'

incidence.from.DS1 <- function(case_count,
                               species,
                               disease,
                               case_type = c("SCC", "SC", "CC",
                                             "SCD", "SD", "CD",
                                             "ALL"),
                               location,
                               merge_rule = c("median")){

  case_type <- match.arg(case_type)
  merge_rule <- match.arg(merge_rule)

  ####################################
  ### cols_to_keep contains a list of column names to be
  ### merged in case of duplicated entries
  ### if a column name does not exist and is not needed apart
  ### from merging, it is ignored.
  ####################################

  cols_to_keep <- c("Location", "Country", "Disease", "Species",
                   "HealthMap.Alert.ID", "Headline", "URL",
                   "Alert.Tag", "Feed.Name", "Lon", "Lat")
  ## these are columns we generate ourselves later on
  cols_to_keep <- c(cols_to_keep, "Date", "Cases")


  ## If column names are not as expected, stop right here.
  good_colnames <- c("Disease", "Species", "Issue.Date",
                     "Location", "Country")
  if (!check.columns(case_count, good_colnames))
    stop("Input data should have columns named:
          Disease, Species, Issue.Date, Location, Country")

  ####################################
  ### select appropriate species and disease
  ### add a column "Cases' with appropriate case definition.
  ####################################
  case_count %<>%
    filter_case_count(species, disease, location) %<>%
    update_cases_column(case_type) %<>%
    merge_duplicates(cols_to_keep)


  ### order these by dates ###
  case_count <- case_count[order(as.numeric(case_count$Date)), ]
  out        <- compute.cumulative.incidence(case_count)
  out$incid  <- c(0, diff(out$Cases))

  return(out)

}
