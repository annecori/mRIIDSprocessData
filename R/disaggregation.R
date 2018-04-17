##' Distribute the case count for a spatial unit into subunits
##'
##'
##' @title
##' @param total case count for large spatial unit
##' @param prob probability vector
##' @param sim number of simulations
##' @return matrix where each column is a simulation or draw from the
##' multinomial distribution. Number of rows is the length of the
##' probability vector.
##' @author Sangeeta Bhatia
disaggregate <- function(total, prob, sim = 1000) {
  rmultinom(sim, total, prob)
}
