plot_fixed_cumincid <- function(cum.incidence, no.outliers, title, outfile){
    p = ggplot() + geom_point(data=cum.incidence, aes(Date, Cases)) +
    geom_point(data=no.outliers, aes(Date, Cases), color='red') + ggtitle(title)
    ggsave(outfile, p)
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




