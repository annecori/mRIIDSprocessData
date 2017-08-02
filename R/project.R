qd##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param p_ij : row vector of length n where n is the number of locations. Each entry is the probability of flow from i to j.
##' @param r_t : n X t matrix. Entry ij is the reproduction number at location i and time j.
##' @param i_t : n X t matrix. Entry ij is the incidence count at location j and time j.
##' @param w_t : column vector of length t. Samples from the serial distribution.
##' @title
##' @return the mean of the Poisson distribution determining the distribution of incidences at location j at time t.
##' @author Sangeeta Bhatia
lambda.j.t <- function(p_ij, r_t, i_t, w_t){
    pij %*% (rt * incidence) %*% omega
}
##' Project future incidence at a location
##' Simulates future incidence based on past incidence data at a
##' given set of locations, the Reproduction numbers at each location and the distribution of the serial interval.
##' @export
##' @param incid A n x t matrix of incidence counts where n is the
##' number of locations and t is the time.
##' @param R A positive matrix of reproduction numbers
##' @param si A vector
##' @param pij a n x n matrix of probabilities
##' @param n.sim number of simulations
##' @param n.days number of days for which each simulation should be run.
##' @return
##' @author Sangeeta Bhatia
project.for.location <-  function(incid, R, si, pij, n.sim = 100, n.days = 7){

    out  <- matrix(0, n.days, n.sim) %>% rbind(incid, .)
    start <- nrow(incid) + 1
    end <- nrow(out) + 1
    for(i in start:end){
        i_t      <- incid[1:i, ]
        w_t      <- utils::tail(si, i)
        r_t      <- R[1:i, ]
        p_ij     <- pij[1:i, ]
        lamda    <- lambda.j.t(p_ij, r_t, i_t, w_t)
        out[i, ] <- stats::rpois(n.sim, lamda)
    }

    out

}
