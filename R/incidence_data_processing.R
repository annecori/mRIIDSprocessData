##' Check if the input vector is monotonically not decreasing
##' @details This means that seq[i] should be greater than or equal
##' to seq[i-1] for all i
##'
##' @title
##' @return a list of indices at which the function is not
##' (not decreasing). That is, return i if seq[i] <= seq[i+1]].
##' If there is no such index, return a list of length 0.
##' @author Sangeeta Bhatia
##' @export
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
##' @param cases numeric vector
##' @param use_last number of points preceeding the last one to be
##' used for estimation. If this is more than
##' the number of available data points, all points are used.
##' @param k_sd
##' @return TRUE is the last point is outside the prediction_interval
##' @author Sangeeta Bhatia
##' @export
is_outlier <- function(cases, use_last, k_sd){
    if (length(cases) < use_last){
        warning("use_last is smaller than the length of the input.
                 Using full vector.")
        use_last <- length(cases)
    }
    p_interval <- tail(cases, use_last + 1)
    p_interval <- prediction_interval(p_interval[-1],
                                      k_sd)

    last_point <- cases[length(cases)]
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
remove_last_outliers <- function(df,
                                 use_last = 20, k_sd = 6){
    outlier <- is_outlier(df$cases, use_last, k_sd)
    while (outlier){
        remove <- nrow(df)
        df <- df[-remove, ]
        outlier <- is_outlier(df$cases, use_last, k_sd)
    }
    df
}

make_increasing_at_end <- function(df, t1){
    df[-t1, ]
}

make_increasing_at_k <- function(df, t1){

    t2 <- t1 + 1
    if (nrow(df) == t2 ) df <- make_increasing_at_end(df, t1)

    else {
        df_copy  <- df
        df_no_t1 <- df_copy
        df_no_t1 <- df_no_t1[-t1, ]
        is_increasing_no_t1 <- is_monotonically_increasing(df_no_t1$cases[c(t1 - 1,
                                                                            t2 - 1)])
        is_increasing_no_t1 <- length(is_increasing_no_t1)

        df_no_t2 <- df_copy
        df_no_t2 <- df_no_t2[-t2, ]
        is_increasing_no_t2 <- is_monotonically_increasing(df_no_t2$cases[c(t1 - 1,
                                                                            t2 - 1)])
        is_increasing_no_t2 <- length(is_increasing_no_t2)

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

        df <- df[-remove, ]
    }
     ## May still be non-increasing. We will fix it in the while loop.
     df
 }



##' .. fixing monotonicity in a vector that returns a non-empty set of
##' indices from is_monotonically_increasing ..
##' @details If vec %>% is_monotonically_increasing is non-empty, it
##' means that there are indices at which vec is
##' decreasing. This is not desirable for instance for a vector of
##' cumulative case counts. We fix this issue using
##' the following rules.
##' @title
##' @param df a data frame containing a numeric column
##' called 'cases'.
##' @return
##' @author Sangeeta Bhatia
make_monotonically_increasing <- function(df){
    not_increasing <- is_monotonically_increasing(df$cases)

    while (length(not_increasing) > 0){
        df <- make_increasing_at_k(df,
                                   t1 = not_increasing[1])
        not_increasing <- is_monotonically_increasing(df$cases)

    }
    df
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

    not_na        <- which(!is.na(no_duplicates$cases))
    df <- no_duplicates[not_na, c("date", "cases")]

    first_row       <- no_duplicates[1, c("date", "cases")]
    first_row$date  <- first_row$date - 1
    first_row$cases <- 0

    df <- rbind(first_row, df)

    ## params for outlier removal.
    use_last   <- 20
    p_within_k <- 0.50
    k_sd       <- interval_width_for_p(use_last, 1 - p_within_k) %>%
                  sqrt %>% `[`(2)


    df <- remove_last_outliers(df, use_last = use_last, k_sd = k_sd)
    df <- make_monotonically_increasing(df)
    df <- interpolate_missing_data(df)
    df
}

##' Pads the input data frame with interpolated count for dates for
##' which no data are available.
##'
##' @title
##' @param df a data frame containing the columns date and
##' cases
##' @return a data frame with all the dates from the first and the
##' last and interpolated case counts for the dates for
##' which this was missing.
##' @author Sangeeta Bhatia
interpolate_missing_data  <- function(df,
                                      method = c("linear",
                                                 "loglinear")) {
    match.arg(method)
    if (nrow(df) < 2){
        warning("need at least two non-NA values to interpolate.
                 Returning input unchanged.")
        return(df)
    }
    dates_all <- seq(from = min(df$date) - 1,
                     to = max(df$date), by = 1)
    df <- merge(df,
                data.frame(date = dates_all),
                all.y = TRUE)
    ## merge will set this to NA and linear interpolation
    ## will make this equal to the number of cases on the first day
    ## for which we have data whereas we want it to start at 0.
    df$cases[1] <- 0
    if (method == "linear") {
        out <- approx(df$date, df$cases,
                      xout = df$date,
                      method = "linear", rule = 2)
    } else {
        stop("Method not yet implemented.")
    }
    df$interpolated_date  <- out$x
    df$interpolated_cases <- out$y
    df
}
