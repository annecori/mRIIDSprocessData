simulate.outliers <- function(n.outliers, max.val) {
                                        #sample(0:max.val, n.outliers, replace=TRUE) %>% return
    rpois(n.outliers, lambda=max.val) %>% return
}

perturb.cumincid <-  function(Cases, n.perturb=10, mu.perturb){

    if( length(Cases) < n.perturb){
        stop("cannot perturb more positions than items in the input.", do.call=FALSE)
    }
                                        # choose random places in the input to add poisson noise
    perturb.at <- sample(1:length(Cases), n.perturb, replace=TRUE)
    perturb.by <- rpois(n.perturb, lambda=mu.perturb)
    Cases[perturb.at] %<>% `+`(perturb.by)
    return(Cases)
}

simulate.cumincid <- function(n=100, lambda, n.outliers=1){

    if(n.outliers > n){
        stop("cannot replace more items than there are in the input.", do.call=FALSE)
    }
    cum.incidence <- rpois(n, lambda) %>% cumsum %>% rev
    last.few <- simulate.outliers(n.outliers, min(cum.incidence))
    cum.incidence[1:n.outliers] <- last.few
    cum.incidence %<>% rev
    return(cum.incidence)
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
##' @title
##' @param n
##' @param k
##' @return
##' @author Sangeeta Bhatia
chebyshev.ineq.sample <- function(n, k){

    lambda <- k * sqrt((n + 1)/n)
    tmp <- ((n + 1)*(n^2 - 1 + n*(lambda^2)))/(n^2 * lambda^2) %>% floor
    tmp/(n + 1) %>% return
}

##' .. For sample size n, what should k be so that the p% of the data is outside
##' k standard deviations of the sample mean ..
##'
##' @details if k=interval.width.for.p(n, p), then chebyshev.ineq.sample(n, k) should be p.
##' @title
##' @param n
##' @param p
##' @return
##' @author Sangeeta Bhatia
interval.width.for.p <- function(n, p){

    lower.lim <- (n^2 - 1)/((p * n * (n + 1)) - 1)
    upper.lim <- (n^2 - 1)/((p * n *(n + 1)) - 1 - n)
    return(c(lower.lim, upper.lim))
}



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
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

# Filters the input data on species, disease and location
filter.case.count <- function(case.count, species, disease, location)
{

  case.count %<>%
   subset(Disease %in% disease) %<>%
   subset(Species %in% species) %<>%
   subset(Location %in% location)

   return(case.count)
}

# adds a column "Cases" with appropriate case definition.
# adds a column "Date" with date extracted from timestamp
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


##############################################################
### 1) identify pairs t, t+1 so that cumI(t) > cumI(t+1)
##############################################################

deal_non_incr_cumI <- function(cum_incid, dates, t1)
{
  t2 <- t1 + 1

  # check if t2 is last data point
  if(t2==length(cum_incid)) # rules for last datapoint
  {
    # 2') try to remove cumI(t),

    tmp_cum_incid_no_t1 <- cum_incid # make a copy
    tmp_cum_incid_no_t1 <- cum_incid[-t1]
    # is tmp_cum_incid_no_t1 increasing between t1-1 and t2
    still_pb <- diff(tmp_cum_incid_no_t1[c(t1-1, t2-1)])<0

    if(!still_pb)
    {
      #     if this "works",
      #     i.e. makes cumI increasing for from t-1 to t+1
      #     then remove t (i.e. set to NA and change missing_data to 1)
      to_remove <- t1
      cum_incid <- cum_incid[-to_remove]
      dates <- dates[-to_remove]
    }else
    {
      #     then remove t (i.e. set to NA and change missing_data to 1) anyway
      to_remove <- t1
      cum_incid <- cum_incid[-to_remove]
      dates <- dates[-to_remove]

      #     and apply the above rules (2'-3')
      #     as if t'=t-1 and t'+1=t+1.
      tmp <- deal_non_incr_cumI(cum_incid, dates, t1-1)
      cum_incid <- tmp$cum_incid
      dates <- tmp$dates
    }

  }else # rules for non last datapoint
  {
    tmp_cum_incid_no_t1 <- cum_incid # make a copy
    tmp_cum_incid_no_t2 <- cum_incid # make a copy
    ### 2) try to remove cumI(t), or cumI(t+1).
    tmp_cum_incid_no_t1 <- cum_incid[-t1]
    tmp_cum_incid_no_t2 <- cum_incid[-t2]

    still_pb <- rep(NA, 2)
    # is tmp_cum_incid_no_t1 increasing between t1-1 and t2
    still_pb[1] <- diff(tmp_cum_incid_no_t1[c(t1-1, t2-1)])<0
    # is tmp_cum_incid_no_t2 increasing between t1-1 and t2
    still_pb[2] <- diff(tmp_cum_incid_no_t2[c(t1, t2)])<0

    if(sum(still_pb) == 2) # none of the options above worked
    {
      # if none of the two options in 2) work, then remove (i.e. set to NA and change missing_data to 1)
      #     both data points (t and t+1). Check if resulting incidence is increasing from t-1 to t+2.
      #     If it is increasing from t-1 to t+2, that's it.
      #     If it is not increasing from t-1 to t+2, apply the above rules (1-4)
      #     as if t'=t-1 and t'+1=t+2.
      to_remove <- c(t1, t2)
      cum_incid <- cum_incid[-to_remove]
      dates <- dates[-to_remove]

      if(diff(cum_incid[c(t1-1, t1)]) < 0)
      {
        tmp <- deal_non_incr_cumI(cum_incid, dates, t1-1)
        cum_incid <- tmp$cum_incid
        dates <- tmp$dates
      } # else do nothing more

    }else if (sum(still_pb) == 0) # both options worked
    {
      # if the two options in 2) work, then remove (i.e. set to NA and change missing_data to 1)
      #     both data points (t and t+1), the incidence should be increasing from t-1 to t+2
      to_remove <- c(t1, t2)
      cum_incid <- cum_incid[-to_remove]
      dates <- dates[-to_remove]

    }else # only one of the options worked
    {
      # If one of those two options, and ONLY one "works",
      #     i.e. makes cumI increasing for all steps from t-1 to t+2
      #     then remove (i.e. set to NA and change missing_data to 1)
      #     the datapoint which makes it work when removed
      if(still_pb[1])
      {
        to_remove <- t2
        cum_incid <- cum_incid[-to_remove]
        dates <- dates[-to_remove]
      }else
      {
        to_remove <- t1
        cum_incid <- cum_incid[-to_remove]
        dates <- dates[-to_remove]
      }
    }

  }

  out <- list(cum_incid = cum_incid, dates = dates)
  return(out)

}




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
