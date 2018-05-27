##' Check if the input vector is monotonically not decreasing
##' @details This means that seq[i] should be greater than or equal
##' to seq[i-1] for all i
##'
##' @title
##' @return a list of indices at which the function is not
##' (not decreasing). That is, return i if seq[i] <= seq[i+1]].
##' If there is no such index, return a list of length 0.
##' @author Sangeeta Bhatia
is_monotonically_increasing <- function(vec){
    differences <- diff(vec)
    which(differences < 0)
}

##' Test if the last data point is within the prediction interval
##' returned by prediction_interval
##'
##' @details k_sd controls the proportion of the data that you want to
##' grab. If this value is large, greater variance
##' will be tolerated.
##' @seealso chebyshev_ineq_sample for the proportion *outside* a
##' certain interval around the mean
##' @title Tests for outlier.
##' @param Cases numeric vector
##' @param use_last number of points preceeding the last one to be
##' used for estimation. If this is more than
##' the number of available data points, all points are used.
##' @param k_sd
##' @return TRUE is the last point is outside the prediction_interval
##' @author Sangeeta Bhatia
is_outlier <- function(Cases, use_last, k_sd){
    if (length(Cases) < use_last){
        warning("use_last is smaller than the length of the input.
                 Using full vector.")
        use_last <- length(Cases)
    }
    p_interval <- Cases              %>%
                  tail(use_last + 1) %>%
                  `[`(-1)            %>%
                  prediction_interval(k_sd)

    last_point <- Cases[length(Cases)]
    if (last_point < p_interval[1] ||
        last_point > p_interval[2]) return(TRUE)
    else return(FALSE)
}

##' @title Outlier Removal
##' @details If the last datapoint is an outlier, remove it
##' Do this until there is no outlier at the end.
##' @seealso
##'
##' @return
##' @author Sangeeta Bhatia
remove_last_outliers <- function(cum_incidence,
                                 use_last = 20, k_sd = 6){
    outlier <- is_outlier(cum_incidence$Cases, use_last, k_sd)
    while (outlier){
        remove <- nrow(cum_incidence)
        cum_incidence <- cum_incidence[-remove, ]
        outlier <- is_outlier(cum_incidence$Cases, use_last, k_sd)
    }
    cum_incidence
}

make_increasing_at_end <- function(cum_incidence, t1){
    cum_incidence[-t1, ]
}

make_increasing_at_k <- function(cum_incidence, t1){

    t2 <- t1 + 1
    if (nrow(cum_incidence) == t2 ) cum_incidence %<>%
                                     make_increasing_at_end(t1)
    else {
        cum_incidence_copy  <- cum_incidence
        cum_incidence_no_t1 <- cum_incidence_copy
        cum_incidence_no_t1 %<>% .[-t1, ]
        is_increasing_no_t1 <- cum_incidence_no_t1$Cases[c(t1 - 1,
                                                           t2 - 1)] %>%
                               is_monotonically_increasing %>% length

        cum_incidence_no_t2 <- cum_incidence_copy
        cum_incidence_no_t2 %<>% .[-t2, ]
        is_increasing_no_t2 <- cum_incidence_no_t2$Cases[c(t1 - 1,
                                                           t2 - 1)] %>%
                               is_monotonically_increasing %>% length

        both_worked <- is_increasing_no_t1 == 0 &
                       is_increasing_no_t2 == 0

        none_worked <- is_increasing_no_t1 > 0 &
                       is_increasing_no_t2 > 0
        if (both_worked || none_worked){
            remove <- c(t1, t2)
        } else if (is_increasing_no_t1 == 0){
            remove <- t1
        } else {
            remove <- t2
        }

        cum_incidence <- cum_incidence[-remove, ]
    }
     ## May still be non-increasing. We will fix it in the while loop.
     cum_incidence
 }



##' .. fixing monotonicity in a vector that returns a non-empty set of
##' indices from is_monotonically_increasing ..
##' @details If vec %>% is_monotonically_increasing is non-empty, it
##' means that there are indices at which vec is
##' decreasing. This is not desirable for instance for a vector of
##' cumulative case counts. We fix this issue using
##' the following rules.
##' @title
##' @param cum_incidence a data frame containing a numeric column
##' called 'Cases'.
##' @return
##' @author Sangeeta Bhatia
make_monotonically_increasing <- function(cum_incidence){
    not_increasing <- cum_incidence$Cases %>%
                      is_monotonically_increasing
    while (length(not_increasing) > 0){
        cum_incidence %<>% make_increasing_at_k(t1 = not_increasing[1])
        not_increasing <- cum_incidence$Cases %>%
                           is_monotonically_increasing
    }
    cum_incidence
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param no_duplicates a data frame ordered by dates with no
##' duplicated rows.
##' @return data frame ordered by date with no missing cases count
##' and which has strictly non-decreasing case count.
##' @author Sangeeta Bhatia
compute.cumulative.incidence <- function(no_duplicates){

    ## starting one day before the first entry so that cumulative
    ## incidence on that day is zero

    not_na        <- which(!is.na(no_duplicates$Cases))
    cum_incidence <- no_duplicates[not_na, c("Date", "Cases")]

    first_row       <- no_duplicates[1, c("Date", "Cases")]
    first_row$Date  <- first_row$Date - 1
    first_row$Cases <- 0

    cum_incidence %<>% rbind(first_row, .)

    ## params for outlier removal.
    use_last   <- 20
    p_within_k <- 0.50
    k_sd       <- interval_width_for_p(use_last, 1 - p_within_k) %>%
                  sqrt %>% `[`(2)


    cum_incidence %<>%
        remove_last_outliers(use_last = use_last, k_sd = k_sd) %<>%
        make_monotonically_increasing %<>%
        interpolate_missing_data

    cum_incidence
}

##' Pads the input data frame with interpolated count for dates for
##' which no data are available.
##'
##' @title
##' @param cum_incidence a data frame containing the columns Date and
##' Cases
##' @return a data frame with all the dates from the first and the
##' last and interpolated case counts for the dates for
##' which this was missing.
##' @author Sangeeta Bhatia
interpolate_missing_data  <- function(cum_incidence,
                                      method = c("linear",
                                                 "loglinear")) {
    match.arg(method)
    if (nrow(cum_incidence) < 2){
        warning("need at least two non-NA values to interpolate.
                 Returning input unchanged.")
        return(cum_incidence)
    }
    dates_all <- seq(from = min(cum_incidence$date) - 1,
                     to = max(cum_incidence$date), by = 1)
    cum_incidence <- merge(cum_incidence,
                           data.frame(date = dates_all),
                           all.y = TRUE)
    if (method == "linear") {
        out <- approx(cum_incidence$date, cum_incidence$cases,
                      xout = cum_incidence$date,
                      method = "linear", rule = 2)
    } else {
        stop("Method not yet implemented.")
    }
    cum_incidence$interpolated_date  <- out$x
    cum_incidence$interpolated_cases <- out$y
    cum_incidence
}
