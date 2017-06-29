##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param K
##' @param alpha
##' @param beta
##' @param gamma
##' @param model
##' @importFrom geosphere
##' @return
##' @author Sangeeta Bhatia

flow_matrix <-  function(K=1, alpha=1, beta=1, gamma=1, model=c("gravity")) {
}

##' .. Computes the flow from A to B under the gravity model ..
##'
##' .. Given the populations of A and B and the distance between them, the function returns the estimated population flow between
##' them modeled as \phi(A,B) = K N_A^{\alpha}N_B^{\beta}/r_{AB}^{\gamma}..
##' @title
##' @param N_from population of the source
##' @param N_to population of the destination
##' @param dist distance between the two places
##' @param pow_N_from power on the population of the source
##' @param pow_N_to power on the population of the destination
##' @param pow_dist power on the distance between the source and the destination
##' @return estimated flow between source and destination
##' @author Pierre Nouvellet, Anne Cori Sangeeta Bhatia
##' @export
gravity_model_flow <- function(N_from, N_to, dist, pow_N_from = 1, pow_N_to = 1, pow_dist=1){

   out <- N_from ^ pow_N_from * N_to ^ pow_N_to / ( dist ^ pow_dist )
   return(out)
}
}
