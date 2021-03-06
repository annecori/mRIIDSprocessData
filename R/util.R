##' Generate indices for lower triangle of a matrix
##'
##' https://stackoverflow.com/questions/20898684
##' @title
##' @param nrow
##' @return
##' @author Sangeeta Bhatia
lower_tri_idx <- function(nrow) {
    s <- seq.int(nrow - 1)
    x <- rev(abs(sequence(s) - nrow) + 1)
    y <- rep.int(s, rev(s))
    idx <- cbind(x, y)
    idx
}
##' Convert a week of year string to the last date of the week
##' @details For our purposes, Week 1 of the year starts on 1st Jan
##' and ends on 7th Jan. I am aware that there is more to the logic than
##' this but for now, this simple rule will suffice. For week i, function
##' returns 7 * (i - 1) days have passed from the start of the year. Use
##' lubridate to add 7 * i to 1-1-year
##' @title Week of year to date conversion
##' @param year
##' @param week a number between 1 and 53
##' @return date
##' @author Sangeeta Bhatia
week_to_maxdate <- function(year, week){
    day1 <- paste(year, 1, 1, sep = "-")
    day1 <- lubridate::ymd(day1)
    dayn <- day1 + (week * 7 - 1)
    lubridate::ymd(dayn)
}

## Copied verbatim from
## github.com/jennybc/row-oriented-workflows/blob/master/ex07_group-by-summarise.R
enquantile <- function(x, ...) {
  qtile <- tibble::enframe(quantile(x, ...), name = "quantile")
  qtile$quantile <- factor(qtile$quantile)
  list(qtile)
}


daily.to.weekly    <- function(daily) {
  extra <- nrow(daily) %% 7
  if (extra != 0) {
    warning("Number of rows is not a multiple of 7.")
    warning(paste("Ignoring last", extra, "days."))
    daily  <- utils::head(daily,-extra)
  }
  weeks  <- cut(daily$date, breaks = "7 days")
  weekly <- split(daily, weeks) 
  weekly <- plyr::ldply(weekly, function(d)
    colSums(d[ , !names(d) %in% "date"])) 
  weekly <- dplyr::rename(weekly, date = .id)
  weekly  
}


add_0incid <- function(df) {
  df    <- arrange(df, DateOnsetInferred)
  start <- min(df$DateOnsetInferred)
  end   <- max(df$DateOnsetInferred)

  dates.all <- seq(from = start, to = end, by = "1 day")
  ndays     <- length(dates.all)
  country   <- rep(df$country[1], ndays)
  district  <- rep(df$CL_DistrictRes[1], ndays)
  dummy     <- data.frame(
    DateOnsetInferred = dates.all,
    country = country,
    CL_DistrictRes = district
  )
  df <- right_join(df, dummy)
  df$incid <- ifelse(is.na(df$incid), 0, df$incid)
  df
}

rms <- function(error) {
  sqrt(mean((error) ^ 2, na.rm = TRUE))
}

##' Test if two samples were drawn from a Poisson distribution with
##' the same rate parameter
##'
##' http://www.sciencedirect.com/science/article/pii/S0378375802004081
##' Homogeneity of Results in Testing Samples from Poisson Series:
##' With an Application to Testing Clover Seed for Dodder
##' J. Przyborowski and H. Wilenski Biometrika Vol. 31, No. 3/4
##' (Mar., 1940), pp. 313-323
##' @details The smaller the difference between the two rate parameters,
##' the harder it will be to detect, especially with small
##' sample sizes. x and y should have same length otherwise the return
##' value will be nonsensical
##' @param x numeric vector
##' @param y numeric vector.
##' @return pvalue for H_0: lambda_1 = lamnda_2
##' @author Sangeeta Bhatia
ctest <- function(x, y) {
  k1 <- sum(x)
  k2 <- sum(y)
  k  <- k1 + k2

  p_x1_lt_k1 <-
    pbinom(k1,
           size = k,
           prob = 0.5,
           lower.tail = T) # P(X1 <= k1)
  p_x1_gt_k1 <-
    pbinom(k1,
           size = k,
           prob = 0.5,
           lower.tail = F) # P(X1 > k1)
  p_x1_eq_k1 <- abs(p_x1_lt_k1 -  p_x1_gt_k1)   # P(X1 = k1)
  p_x1_gt_k1 <- p_x1_gt_k1 + p_x1_eq_k1 # P(X1 >= k1)

  2 * min(p_x1_gt_k1, p_x1_lt_k1)

}

##' Log likelihood of an observed vector
##'
##' .. content for \details{} ..
##' @title
##' @param obs vector of observed values
##' @param freq.table data frame recording the probability distribution
##' such as that returned by as.data.frame(prop.table()).
##' Column names are x and prob.
##' @return log likelihood
##' @author Sangeeta Bhatia
log_likelihood <- function(obs, freq.table) {
  prob0 <- setdiff(obs, freq.table$x)
  if (length(prob0 > 0))
    lh <- 0
  else{
    lh <- filter(freq.table, x %in% obs) %>%
      pull(prob) %>% prod
  }


  log(lh)
}

##' .. Log likelihood of the observed incidence data
##'
##' Log likelihood of the observed incidence data under the probability
##' distribution
##' determined by the predicted data.
##' @title
##' @param observed  data.frame with t rows and 2 columns one of which
##' should contain the time (1, 2, 3 etc or dates) and should be
##' called date.
##' The second column should be called incid and contains the observed
##' incidences.
##' @param predicted data.frame with t * num_simulations rows and 2
##' columns one of which contains the time (same as observed) and is
##' called Date. The second column is called incid and contains the
##' predicted incidences.
##' @return log likelihood of the observed data.
##' @author Sangeeta Bhatia
log_likelihoods_atj <- function(observed, predicted) {
  dates <- unique(observed$Date)
  lh <- c()
  for (d in dates) {
    obs  <- filter(observed,  Date == d) %>% pull(incid)
    pred <- filter(predicted, Date == d) %>% pull(incid)

    lambda <- mean(pred)
    lh_obs <- dpois(obs, lambda = lambda, log = TRUE)
    lh <- c(lh, lh_obs)
  }
  names(lh) <- dates
  lh
}




projection_quantiles <- function(projections) {
  grouped <- dplyr::group_by(projections, date) 
  projections_50 <- dplyr::summarise_if(grouped, 
                                        is.numeric, 
                                        quantile, 
                                        probs = 0.5, 
                                        na.rm = TRUE)
  
  projections_50 <- tidyr::gather(projections_50, country, y,-date)

  projections_025 <- dplyr::summarise_if(grouped, 
                                         is.numeric, 
                                         quantile, 
                                         probs = 0.025, 
                                         na.rm = TRUE)
  projections_025 <- tidyr::gather(projections_025, country, ymin,-date)

  projections_975 <- dplyr::summarise_if(grouped, 
                                         is.numeric, 
                                         quantile, 
                                         probs = 0.975, 
                                         na.rm = TRUE)
  projections_975 <- tidyr::gather(projections_975, country, ymax,-date)

  projections_distr <- left_join(projections_50, 
                                 projections_025) 
  
  projections_distr <- left_join(projections_distr, 
                                 projections_975)

  projections_distr$date <- as.Date(projections_distr$date)

  projections_distr

}


## Given a vector R return a nrow * ncol matrix where
## R[1:ncol] will be inserted in rows 1 to change_at[1].
## Similarly R[ncol + 1: (2 * ncol)] will be inserted from
## change_at[1] + 1 to row_indices[2] and so on.
## for example makeRmatrix(c(1, 2, 3), 3, 4, c(4)) will return
## [ 1 2 3
##   1 2 3
##   1 2 3
##   1 2 3]  
## makeRmatrix(c(1, 2, 3), 1, 9, c(5, 7)) returns
## [1 
## 1 
## 1 
## 1 
## 2 
## 2 
## 3 
## 3 
## 3]

makeRmatrix <- function(R, ncol, nrow, change_at) {
  split_at <- seq(from = 1, to = length(R), by = ncol)
  split_R  <- unname(split(R, cumsum(seq(R) %in%  split_at)))
  num_rows <- diff(c(1, change_at, nrow + 1))

  out <- mapply(rep, x = split_R, times = num_rows)
  matrix(unlist(out), byrow = T, ncol = ncol)
}


clean_names <- function(x) {
    x <- tolower(x)
    x <- stringr::str_replace_all(x, "\ ", "")
    x <- stringr::str_replace_all(x, "-", "")
    x <- stringr::str_replace_all(x, "'", ".")
    x
}


weekly_average<- function(daily) {
    weekly <- daily.to.weekly(daily)
    weekly_avg <- dplyr::mutate_if(weekly, is.numeric, funs(. / 7))
    weekly_avg
}
