##' .. content for \description{} (no empty lines) ..
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
