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
  weekly <- split(daily, weeks) %>%
            plyr::ldply(function(d)
            select(d,-date) %>% colSums) %>%
             dplyr::rename(date = .id)

  weekly
}

## For each country, we want to plot the training data, the validation
## data and a polygon spanned by the 2.5% and 97.5% quantiles.
plot.weekly <- function(available, projection) {
  available$Date %<>% as.Date
  p     <- ggplot(available, aes_string("Date",
                                        colnames(available)[3],
                                        color = "Category")) + geom_point()
  ci.95 <- projection     %>%
    split(.$Date) %>%
    plyr::ldply(. %>% `[`(, 2)
                %>% quantile(probs = c(0.5, 0.025, 0.975))) %>%
    dplyr::rename(Date = .id)
  x <- ci.95$Date %>% c(rev(.)) %>% as.Date
  y <- c(ci.95[, 3], rev(ci.95[, 4]))

  p   <-
    p + geom_polygon(
      data = data.frame(x = x, y = y),
      aes(x, y, alpha = 0.01),
      color = "red",
      size = 0.3,
      linetype = "blank"
    )
  p   <-
    p + geom_line(
      data = data.frame(x = as.Date(ci.95$Date), y = ci.95[, 2]),
      aes(x, y),
      size = 0.3,
      color = "red",
      inherit.aes = FALSE
    )
  p   <- p + theme_minimal() + theme(legend.position = "none")
  p   <-
    p + xlab("") + theme(axis.text.x = element_text(angle = 45, vjust = 0.2))
  return(p)

}

add_0incid <- function(df) {
  df    <- arrange(df, DateOnsetInferred)
  start <- min(df$DateOnsetInferred)
  end   <- max(df$DateOnsetInferred)

  dates.all <- seq(from = start, to = end, by = "1 day")
  ndays     <- length(dates.all)
  country   <- rep(df$Country[1], ndays)
  district  <- rep(df$CL_DistrictRes[1], ndays)
  dummy     <- data.frame(
    DateOnsetInferred = dates.all,
    Country = country,
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
  by_date <- split(projections, projections$Date)

  projections_50 <- lapply(by_date, function(df)
    summarise_if(df, is.numeric, quantile, probs = 0.5, na.rm = TRUE)) %>%
    bind_rows(.id = "Date") %>%
    tidyr::gather(Country, y,-Date)

  projections_025 <- lapply(by_date, function(df)
    summarise_if(df, is.numeric, quantile, probs = 0.025, na.rm = TRUE)) %>%
    bind_rows(.id = "Date") %>%
    tidyr::gather(Country, ymin,-Date)

  projections_975 <- lapply(by_date, function(df)
    summarise_if(df, is.numeric, quantile, probs = 0.975, na.rm = TRUE)) %>%
    bind_rows(.id = "Date") %>%
    tidyr::gather(Country, ymax,-Date)

  projections_distr <-
    left_join(projections_50, projections_025) %>%
    left_join(projections_975)

  projections_distr$Date %<>% as.Date

  projections_distr

}

plot.style <- function(p) {
  p <- p + theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      strip.background = element_blank()
    )
  p <-
    p + theme(axis.text.x = element_text(angle = 80, hjust = 1))
  p <- p + theme(legend.title = element_blank())
  p <- p + xlab("")
  p <- p + scale_x_date(date_labels =  "%b %Y")
  p

}


plot.weekly2 <- function(available, projections_distr) {
  tmp <-
    tidyr::gather(available, Country, Incidence,-c(Category, Date))
  tmp$Date %<>% as.Date
  p   <- ggplot(tmp, aes(Date, Incidence, color = Category)) +
    geom_point(size = 1,
               stroke = 0,
               shape = 16) +
    facet_wrap( ~ Country, scales = "free_y")
  p <-
    p + scale_color_discrete(labels = c("Training", "Validation"))
  p <-
    p + geom_line(
      data = projections_distr,
      aes(x = Date, y = y, group = 1),
      color = 'black',
      size = 0.9
    )
  p <- p + geom_ribbon(
    data = projections_distr,
    aes(
      x = Date,
      ymin = ymin,
      ymax = ymax,
      group = 1
    ),
    inherit.aes = FALSE,
    alpha = 0.5
  )
  p <- plot.style(p)
  p

}

plot.weekly3 <-
  function(available,
           projections_distr,
           trng.start,
           valdtn.end) {
    tmp <-
      tidyr::gather(available, Country, Incidence,-c(Category, Date))
    tmp$Date %<>% as.Date
    p   <- ggplot(tmp, aes(Date, Incidence)) +
      geom_point(
        size = 2,
        stroke = 0,
        shape = 16,
        colour = "gray"
      ) +
      facet_wrap( ~ Country, scales = "free_y", nrow = 3)

    tmp2 <- filter(tmp, Date >= trng.start & Date <= valdtn.end)
    p <-
      p + geom_point(
        data = tmp2,
        aes(Date, Incidence, color = Category),
        size = 2,
        stroke = 0,
        shape = 16
      )
    p <-
      p + scale_color_discrete(labels = c("Training", "Validation"))
    p <-
      p + geom_line(
        data = projections_distr,
        aes(x = Date, y = y, group = 1),
        color = 'black',
        size = 1
      )
    p <- p + geom_ribbon(
      data = projections_distr,
      aes(
        x = Date,
        ymin = ymin,
        ymax = ymax,
        group = 1
      ),
      inherit.aes = FALSE,
      alpha = 0.5
    )
    p <- plot.style(p)
    p

  }


## Given a vector R return a nrow * ncol matrix where
## R[1:ncol] will be inserted in rows 1 to change_at[1].
## Similarly R[ncol + 1: (2 * ncol)] will be inserted from
## change_at[1] + 1 to row_indices[2] and so on.
## for example makeRmatrix(c(1, 2, 3), 3, 4, c(4)) will return
## [ 1 2 3
##   1 2 3
##   1 2 3
##   1 2 3] and makeRmatrix(c(1, 2, 3), 1, 9, c(5, 7)) returns
## [1 1 1 1 2 2 3 3 3]

makeRmatrix <- function(R, ncol, nrow, change_at) {
  split_at <- seq(from = 1, to = nrow, by = ncol)
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
