##' Shuffle the columns of a data frame
##'
##' Each row of the data frame is shuffled *separately*
##' @title Shuffle columns of data frame
##' @param df data frame
##' @return shuffled data frame
##' @author Sangeeta Bhatia
shuffle_cols <- function(df){

    for (i in seq_len(nrow(df))){
       df[i, ] <- df[i, sample(ncol(df))]
    }
    df
}

##' Draw random sample from a list of data frames
##' @param projections_list list of data-frames. Each data frame is a
##' single simulation. The rows are dates and columns are places.
##' @param place name of column to be selected from each simulation
##' @param k number of samples to draw. If k > length(projections_list), it is
##' set to length(projections_list)
##' @author Sangeeta Bhatia
sample_projections <- function(projections_list, place, k){

    n_sim <- length(projections_list)
    k <- min(k, n_sim)
    idx <- sample(seq_len(n_sim), k)
    draw <- projections_list[idx]
    d_sims <- purrr::map_dfc(draw, function(x)
        dplyr::select(x, sim = place))
    d_sims$place <- place
    d_sims
}

