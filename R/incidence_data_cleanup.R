##' Duplicate records in the case count are merged according to the
##' following rules.
##' Details
##' @title Merge dulplicate records in the incidence count.
##' @param case_count
##' @param cols_to_keep
##' @param merge_rule
##' @return duplicates free data frame
##' @author Sangeeta Bhatia
merge_duplicates <- function(case_count,
                             cols_to_keep,
                             merge_rule=c("median")){
  ####################################
  ### identify unique entries based on date and country columns
  ####################################

  case_count$DateCountry <- paste0(as.character(case_count$Date),
                                   case_count$Country)
  uniq_dat <- unique(case_count$DateCountry)

  ####################################
  ### merge duplicated entries where necessary
  ####################################

  cols_to_keep <- cols_to_keep[cols_to_keep %in% names(case_count)]

  ### create a processed dataset ###
  duplicates_free <- as.data.frame(matrix(NA, length(uniq_dat),
                                          length(cols_to_keep)))
  names(duplicates_free) <- cols_to_keep
  ### making sure classes are consistent with
  ### original dataset (useful for dates in particular)
  for (j in 1:length(cols_to_keep)){
    class(duplicates_free[, j]) <- class(case_count[, cols_to_keep[j]])
  }
  ### merging entries ###
  for (i in 1:length(uniq_dat)){
      duplicates_free[i, ] <- merge_dup_lines_DS1(case_count[which(case_count$DateCountry
                                                                  %in% uniq_dat[i]), ],
                                                 cols_to_keep,
                                                 rule = merge_rule)
  }
  return(duplicates_free)
}


##' Filters the input data on species, disease and location
##'
##' @title Filter the case count for parameters of interest.
##' @param case_count
##' @param species
##' @param disease
##' @param location
##' @return
##' @author Sangeeta Bhatia
filter_case_count <- function(case_count, species, disease, location){

  case_count %<>%
   subset(Disease  %in% disease) %<>%
   subset(Species  %in% species) %<>%
   subset(Location %in% location)

   return(case_count)
}



##' adds a column "Cases" with appropriate case definition and
##' a column "Date" with date extracted from timestamp
##' .. content for \details{} ..
##' @title Update the case count with a column for dates and a column
##' for the appropriate case definition.
##' @param case_count
##' @param case_type
##' @return
##' @author Sangeeta Bhatia
update_cases_column <- function(case_count, case_type = c("SCC", "SC",
                                                          "CC", "SCD",
                                                          "SD", "CD",
                                                          "ALL")){

  ####################################
  ### add checks on input arguments ###
  ####################################

  case_type <- match.arg(case_type)

  ### create dates without time from Issue.Date

  case_count$Date <- lubridate::mdy_hm(case_count$Issue.Date) %>%
                       lubridate::date(.)
  ##################################################################
  ### Create column called Cases which comprises all relevant cases
  ### to be counted in incidence
  ##################################################################

  case_count$Cases <- get_cases(case_count, case_type)

  return(case_count)
}

# function to create a merged entry, separated by a slash
#
paste_single_col <- function(col_dup) {
  out <- paste(col_dup, collapse = " / ")
  return(out)
}

# Sum with na.rm = TRUE but if all nas, return NA instead of 0
#

sum_only_na_stays_na <- function(x){
    if (all(is.na(x))) out <- NA
    else out <- sum(x, na.rm = TRUE)
    return(out)
}


# Gets appropriate case counts off dataframe
#

get_cases <- function(dat,
                      case_type = c("SCC", "SC", "CC", "SCD",
                                    "SD", "CD", "ALL")){

  case_type <- match.arg(case_type)

  if (case_type %in% "SCC") {
    col <- apply(dat[, c("SC", "CC")], 1, sum_only_na_stays_na)
  } else if (case_type %in% "SC"){
    col <- dat$SC
  } else if (case_type %in% "CC"){
    col <- dat$CC
  } else if (case_type %in% "SCD"){
    col <- apply(dat[, c("SD", "CD")], 1, sum_only_na_stays_na)
  } else if (case_type %in% "SD"){
    col <- dat$SD
  } else if (case_type %in% "CD"){
    col <- dat$CD
  } else if (case_type %in% "ALL"){
      col <- apply(dat[, c("SC", "CC", "SD", "CD")], 1,
                   sum_only_na_stays_na)
  }

  return(col)
}

# Check column names
check.columns <- function(case_count, good_colnames){
  actual_colnames <- colnames(case_count)
  check_colnames  <- lapply(good_colnames,
                            FUN = function(x) x %in% actual_colnames) %>%
                      unlist
  return(all(check_colnames))
}


##' Computes the prection interval for the (n + 1)th observation given
##' n observations. This interval is
##' [mu.hat - lambda.sample.var, mu.hat + lambda.sample.var] where
##' lambda = k * sqrt((n + 1)/n)
##' Unfortunate notation due to Saw et al ..
##' @references \url{https://www.jstor.org/stable/pdf/2683249.pdf}
##' @seealso \url{https://arxiv.org/pdf/1509.08398.pdf}
##' @title
##' @return
##' @author Sangeeta Bhatia
prediction_interval <- function(x, k){
    n <- length(x)
    mu.hat <- mean(x)
    sd.hat <- sd(x)
    lambda <- ( (n + 1) / n) %>% sqrt %>% `*`(k)

    c(mu.hat - (lambda * sd.hat), mu.hat + (lambda * sd.hat))
}


##' Reports the maximum percentage beyond k * sqrt((n + 1)/n)
##' standard deviations from mean
##' @title Chebyshev Inequality with sample mean.
##' Utilizes Chebyshev inequality with sample mean.
##' @param n Sample size
##' @param k Number of standard deviations.
##' @return
##' @author Sangeeta Bhatia
chebyshev_ineq_sample <- function(n, k){

    lambda <- k * sqrt( (n + 1) / n)
    ( ( (n + 1) * (n^2 - 1 + n * (lambda^2))) /
      (n^2 * lambda^2)) %>%
      floor %>%
      `/`(n + 1)
}

##' For sample size n, what should k be so that the p% of the data
##' is outside k standard deviations of the sample mean.
##' @details if k=interval_width_for_p(n, p), then
##' chebyshev_ineq_sample(n, k) should be p.
##' @title Interval width for Chebysev Inequality with sample mean.
##' @param n
##' @param p
##' @return
##' @author Sangeeta Bhatia
interval_width_for_p <- function(n, p){

    lower_lim <- (n^2 - 1) / ( (p * n * (n + 1)) - 1)
    upper_lim <- (n^2 - 1) / ( (p * n * (n + 1)) - 1 - n)
    return(c(lower_lim, upper_lim))
}
