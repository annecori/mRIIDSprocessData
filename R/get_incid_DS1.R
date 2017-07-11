#' Compute incidence from data stream 1
#'
#' This function serves as a wrapper around the cleaning and processing of
#' incidence data. The functions for these are available in ...
#'
#' @export
#'
#' @author Pierre Nouvellet, Anne Cori and Sangeeta Bhatia
#'
#' @param case.count dataframe containing data stream 1 to be processed
#' @param species string giving species in which disease incidence will be
#'   extracted
#' @param disease string specifying disease of interest
#' @param case.type string specifying column name from which incidence is
#'   extracted; one of "SCC" (suspected and confirmed cases), "SC" (suspected
#'   cases), "CC" (confirmed cases), "SCD" (suspected and confirmed deaths),
#'   "SD" (suspected deaths), or "CD" (confirmed deaths).
#' @param location string specifying location for which incidence will be
#'   computed
#' @param merge_rule string to specify how merging should be made when
#'   duplicated entries exist. At the moment only "median" is supported.
#'
#' @return a dataframe containing incidence and cumulative incidence obtained
#'   from merged_dat.
#'
#' @details XXX
#'
#' @importFrom stats median
#'
#' @examples
#'
#' # to be written
#'

incidence.from.DS1 <- function(case.count,
                       species,
                       disease,
                       case.type = c("SCC", "SC", "CC", "SCD", "SD", "CD"),
                       location,
                       merge_rule = c("median"))
{

  case.type <- match.arg(case.type)
  merge_rule <- match.arg(merge_rule)

  ####################################
  ### cols.to.keep contains a list of column names to be merged in case of duplicated entries
  ### if a column name does not exist and is not needed apart from merging, it is ignored.
  ####################################

  cols.to.keep = c("Location", "Country", "Disease", "Species",
                   "HealthMap.Alert.ID", "Headline", "URL",
                   "Alert.Tag", "Feed.Name", "Lon", "Lat")
  cols.to.keep <- c(cols.to.keep, "Date", "Cases") # these are columns we generate ourselves later on

  # If column names are not as expected, stop right here.
  good.colnames <- c("Disease","Species","Issue.Date","Location","Country")
  if(!check.columns(case.count, good.colnames))
    stop("Input data should have columns named:Disease, Species, Issue.Date, Location, Country")

  ####################################
  ### select appropriate species and disease
  ### add a column "Cases' with appropriate case definition.
  ####################################
  case.count %<>%
    filter.case.count(species, disease, location) %<>%
    update.cases.column(case.type) %<>%
    merge.duplicates(cols.to.keep)


  ### order these by dates ###
  case.count <- case.count[order(as.numeric(case.count$Date)), ]

  out <- compute.cumulative.incidence(case.count)

  return(out)

}
