##' Distribute the case count for a spatial unit into subunits
##'
##' @title Return disaggregated counts
##' @param total case count for large spatial unit
##' @param pmatrix matrix of probabilities. Each row corresponsd to a
##' single run and each column is a spatial sub-unit.
##' @param sim Number of simulations (i.e., draws) for the multinomial
##' distribution. Note that this need not be the same as the number of
##' rows in pmatrix.
##' @return matrix with nrow(pmatrix) * sim rows and ncol(pmatrix)
##' columns
##' @author Sangeeta Bhatia
disaggregate <- function(total, pmatrix, sim = 1000){

    psim <- nrow(pmatrix)
    out <- matrix(NA, nrow = psim * sim, ncol = ncol(pmatrix))
    for (i in seq_len(psim)){
        start <- (i - 1) * sim + 1
        end <- i * sim
        prob <- pmatrix[i, ]
        out[start:end, ] <- t(rmultinom(sim, total, prob))
    }
    out
}

##' Quantiles for disaggregated count distribution
##' @param sample_mat matrix of samples. Each row is a simulation and
##' each column is a spatial sub-unit. Make sure that the matrix has
##' meaningful column names so that the output makes sense.
##' @return desired quantiles for each spatial sub-unit.
disaggregate_distr <- function(sample_mat, probs =  c(0.025, 0.5, 0.975)){

    out <- map_dfr(data.frame(sample_mat), quantile, probs)
    out$quantiles <- probs
    out <- tidyr::gather(out, district, val, -quantiles)
    out <- tidyr::spread(out, quantiles, val)

}
