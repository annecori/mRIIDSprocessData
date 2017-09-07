##' Duplicate records in the case count are merged according to the following rules.
##' Details
##' @title Merge dulplicate records in the incidence count.
##' @param case.count
##' @param cols.to.keep
##' @param merge_rule
##' @return
##' @author Sangeeta Bhatia
merge.duplicates <- function(case.count, cols.to.keep, merge_rule=c("median"))
{
  ####################################
  ### identify unique entries based on date and country columns
  ####################################

  case.count$DateCountry <- paste0(as.character(case.count$Date), case.count$Country)
  uniq_dat <- unique(case.count$DateCountry)

  ####################################
  ### merge duplicated entries where necessary
  ####################################

  cols.to.keep <- cols.to.keep[cols.to.keep %in% names(case.count)]

  ### create a processed dataset ###
  duplicates.free <- as.data.frame(matrix(NA, length(uniq_dat), length(cols.to.keep)))
  names(duplicates.free) <- cols.to.keep
  ### making sure classes are consistent with original dataset (useful for dates in particular)
  for(j in 1:length(cols.to.keep))
  {
    class(duplicates.free[,j]) <- class(case.count[, cols.to.keep[j]])
  }
  ### merging entries ###
  for(i in 1:length(uniq_dat))
  {
    duplicates.free[i,] <- merge_dup_lines_DS1(case.count[which(case.count$DateCountry %in% uniq_dat[i]),], cols.to.keep, rule = merge_rule)
  }
  return(duplicates.free)
}


##' Filters the input data on species, disease and location
##'
##' .. content for \details{} ..
##' @title Filter the case count for parameters of interest.
##' @param case.count
##' @param species
##' @param disease
##' @param location
##' @return
##' @author Sangeeta Bhatia
filter.case.count <- function(case.count, species, disease, location)
{

  case.count %<>%
   subset(Disease %in% disease) %<>%
   subset(Species %in% species) %<>%
   subset(Location %in% location)

   return(case.count)
}



##' adds a column "Cases" with appropriate case definition and
##' a column "Date" with date extracted from timestamp
##' .. content for \details{} ..
##' @title Update the case count with a column for dates and a column for the
##' appropriate case definition.
##' @param case.count
##' @param case_type
##' @return
##' @author Sangeeta Bhatia
update.cases.column <- function(case.count,case_type = c("SCC", "SC", "CC", "SCD", "SD", "CD"))
{

  ####################################
  ### add checks on input arguments ###
  ####################################

  case_type <- match.arg(case_type)

  ### create dates without time from Issue.Date
  #dat$Date <- as.Date(unlist(strsplit(dat$Issue.Date, " "))[seq(1, 2*nrow(dat), 2)], format="%m/%d/%y")
  case.count$Date <- lubridate::mdy_hm(case.count$Issue.Date) %>% lubridate::date(.)
  ####################################
  ### Create column called Cases which comprises all relevant cases to be counted in incidence ###
  ####################################

  case.count$Cases <- get_cases(case.count, case_type)

  return(case.count)
}

# function to create a merged entry, separated by a slash
#
paste_single_col <- function(col_dup)
{
  out <- paste(col_dup, collapse=" / ")
  return(out)
}

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

# Check column names
check.columns <- function(case.count, good.colnames)
{
  actual.colnames <- colnames(case.count)
  check.colnames <- lapply(good.colnames, FUN = function(x) x %in% actual.colnames) %>% unlist
  return(all(check.colnames))
}


##' .. Computes the prection interval for the (n + 1)th observation given n observations ..
##' This interval is [mu.hat - lambda.sample.var, mu.hat + lambda.sample.var] where
##' lambda = k * sqrt((n + 1)/n)
##' Unfortunate notation due to Saw et al ..
##' @references \url{https://www.jstor.org/stable/pdf/2683249.pdf}
##' @seealso \url{https://arxiv.org/pdf/1509.08398.pdf}
##' @title
##' @return
##' @author Sangeeta Bhatia
prediction.interval <- function(x, k){
    n <- length(x)
    mu.hat <- mean(x)
    sd.hat <- sd(x)
    lambda <- ((n + 1)/n) %>% sqrt %>% `*`(k)
    c(mu.hat - (lambda * sd.hat), mu.hat + (lambda * sd.hat)) %>% return
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. Reports the maximum percentage beyond k * sqrt((n + 1)/n) standard deviations from mean ..
##' @title Chebyshev Inequality with sample mean.
##' Utilizes Chebyshev inequality with sample mean.
##' @param n Sample size
##' @param k Number of standard deviations.
##' @return
##' @author Sangeeta Bhatia
chebyshev.ineq.sample <- function(n, k){

    lambda <- k * sqrt((n + 1)/n)
    (((n + 1)*(n^2 - 1 + n*(lambda^2)))/(n^2 * lambda^2)) %>%
    floor %>%
    `/`(n + 1)
}

##' .. For sample size n, what should k be so that the p% of the data is outside
##' k standard deviations of the sample mean ..
##'
##' @details if k=interval.width.for.p(n, p), then chebyshev.ineq.sample(n, k) should be p.
##' @title Interval width for Chebysev Inequality with sample mean.
##' @param n
##' @param p
##' @return
##' @author Sangeeta Bhatia
interval.width.for.p <- function(n, p){

    lower.lim <- (n^2 - 1)/((p * n * (n + 1)) - 1)
    upper.lim <- (n^2 - 1)/((p * n *(n + 1)) - 1 - n)
    return(c(lower.lim, upper.lim))
}
