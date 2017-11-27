##' .. check if the input vector is monotonically not decreasing ..
##' This means that seq[i] should be greater than or equal to seq[i-1] for all i
##' .. content for \details{} ..
##' @title
##' @return a list of indices at which the function is not (not drecreasing). That is, return i if seq[i] <= seq[i+1]].
##' If there is no such index, return a list of length 0.
##' @author Sangeeta Bhatia
is_monotonically_increasing <- function(vec){
    differences <- diff(vec)
    which(differences < 0) %>% return
}

##' .. Test if the last data point is within the prediction interval returned by prediction.interval ..
##'
##' @details k.sd controls the proportion of the data that you want to grab. If this value is large, greater variance
##' will be tolerated.
##' @seealso chebyshev.ineq.sample for the proportion *outside* a certain interval around the mean
##' @title
##' @param Cases numeric vector
##' @param use.last number of points preceeding the last one to be used for estimation. If this is more than
##' the number of available data points, all points are used.
##' @param k.sd
##' @return TRUE is the last point is outside the prediction interval, FALSE otherwise.
##' @author Sangeeta Bhatia
is.outlier <- function(Cases, use.last, k.sd){
    if( length(Cases) < use.last){
        warning("use.last is smaller than the length of the input. Using full vector.")
        use.last <- length(Cases)
    }
    p.interval <- Cases              %>%
                  tail(use.last + 1) %>%
                  `[`(-1)            %>%
                  prediction.interval(k.sd)

    last.point <- Cases[length(Cases)]
    if( last.point < p.interval[1] || last.point > p.interval[2]) return(TRUE)
    else return(FALSE)
}

##' .. If the last datapoint is an outlier, remove it ..
##' .. Do this until there is no outlier at the end.
##' @seealso
##' @title
##' @return
##' @author Sangeeta Bhatia
remove.last.outliers <- function(cum.incidence, use.last=20, k.sd=6){
    outlier <- is.outlier(cum.incidence$Cases, use.last, k.sd)
    while(outlier){
        remove <- nrow(cum.incidence)
        cum.incidence <- cum.incidence[-remove, ]
        outlier <- is.outlier(cum.incidence$Cases, use.last, k.sd)
    }
    return(cum.incidence)
}

make_increasing_at_end <- function(cum.incidence, t1){
    cum.incidence[-t1, ]
}

make_increasing_at_k <- function(cum.incidence, t1){


    t2 <- t1 + 1
    if( nrow(cum.incidence) == t2 ) cum.incidence %<>% make_increasing_at_end(t1)
    else {
        cum.incidence.copy <- cum.incidence
        cum.incidence.no.t1 <- cum.incidence.copy
        cum.incidence.no.t1 %<>% .[-t1, ]
        is.increasing.no.t1 <- cum.incidence.no.t1$Cases[c(t1-1, t2-1)] %>% is_monotonically_increasing %>% length

        cum.incidence.no.t2 <- cum.incidence.copy
        cum.incidence.no.t2 %<>% .[-t2, ]
        is.increasing.no.t2 <- cum.incidence.no.t2$Cases[c(t1-1, t2-1)] %>% is_monotonically_increasing %>% length

        both.worked <- is.increasing.no.t1 == 0 & is.increasing.no.t2 == 0
        none.worked <- is.increasing.no.t1 > 0 & is.increasing.no.t2 > 0
        if(both.worked || none.worked){
            remove <- c(t1, t2)
        } else if(is.increasing.no.t1 == 0){
            remove <- t1
        } else {
            remove <- t2
        }

        cum.incidence <- cum.incidence[-remove,]
     }
     cum.incidence # May still be non-increasing but we will fix that in the while loop.
 }



##' .. fixing monotonicity in a vector that returns a non-empty set of indices from is_monotonically_increasing ..
##' @details If vec %>% is_monotonically_increasing is non-empty, it means that there are indices at which vec is
##' decreasing. This is not desirable for instance for a vector of cumulative case counts. We fix this issue using
##' the following rules.
##' @title
##' @param cum.incidence a data frame containing a numeric column called 'Cases'.
##' @return
##' @author Sangeeta Bhatia
make_monotonically_increasing <- function(cum.incidence){
    not.increasing <- cum.incidence$Cases %>% is_monotonically_increasing
    while(length(not.increasing) > 0){
        cum.incidence %<>% make_increasing_at_k( t1=not.increasing[1] )
        not.increasing <- cum.incidence$Cases %>% is_monotonically_increasing
    }
    cum.incidence
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param no.duplicates a data frame ordered by dates with no duplicated rows.
##' @return data frame ordered by date with no missing cases count and which has strictly non-decreasing
##' case count.
##' @author Sangeeta Bhatia
compute.cumulative.incidence <- function(no.duplicates){

    # starting one day before the first entry so that cumulative incidence on that day is zero
    # dates.all <- seq(min(merged_dat$Date, na.rm = TRUE)-1, max(merged_dat$Date, na.rm = TRUE), 1)
    not.na        <- which(!is.na(no.duplicates$Cases))
    cum.incidence <- no.duplicates[not.na, c('Date', 'Cases')]

    first.row       <- no.duplicates[1, c('Date', 'Cases')]
    first.row$Date  <- first.row$Date - 1
    first.row$Cases <- 0

    cum.incidence %<>% rbind(first.row, .)

    # params for outlier removal.
    use.last   <- 20
    p.within.k <- 0.50
    k.sd       <- interval.width.for.p(use.last, 1 - p.within.k) %>% sqrt %>% `[`(2)


    cum.incidence %<>%
        remove.last.outliers(use.last=use.last, k.sd=k.sd) %<>%
        make_monotonically_increasing %<>%
        interpolate.missing.data

    cum.incidence
}

##' Pads the input data frame with interpolated count for dates for which no data are available.
##'
##' .. content for \details{} ..
##' @title
##' @param cum.incidence a data frame containing the columns Date and Cases
##' @return a data frame with all the dates from the first and the last and interpolated case counts for the dates for
##' which this was missing.
##' @author Sangeeta Bhatia
interpolate.missing.data <- function(cum.incidence) {
    if(nrow(cum.incidence) < 2){
        warning("need at least two non-NA values to interpolate.
                 Returning input unchanged.")
        return(cum.incidence)
    }
    dates.all <- seq(min(cum.incidence$Date) - 1, max(cum.incidence$Date), by = 1)
    cum.incidence %<>%  merge(data.frame(Date=dates.all), all.y = TRUE)
    out <- approx(cum.incidence$Date, cum.incidence$Cases,
                    xout = cum.incidence$Date, method = "linear", rule = 2)
    cum.incidence$Date  <- out$x
    cum.incidence$Cases <- out$y
    cum.incidence
}
