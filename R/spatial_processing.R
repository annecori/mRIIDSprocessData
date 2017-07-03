##' .. Given the populations at two places and the distances between them, returns the flow matrix under the specified model ..
##' .. The models are : gravity and radiation ..
##'
##' @title
##' @param K
##' @param alpha
##' @param beta
##' @param gamma
##' @param model
##' @importFrom geosphere
##' @return
##' @author Sangeeta Bhatia

flow_vector <-  function(N_from, N_to, distance, K=1, pow_N_from=1, pow_N_to=1, pow_dist=1, model=c("gravity")) {
    if(model=="gravity") gravity_model_flow(N_from, N_to, distance, K, pow_N_from, pow_N_to, pow_dist) %>% return
    else if(model=="radiation") stop("Model not yet implemented")
    else stop("Model not yet implemented")
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
gravity_model_flow <- function(N_from, N_to, distance, K, pow_N_from, pow_N_to, pow_dist){

   K * (N_from^pow_N_from) * (N_to^pow_N_to) / (distance^pow_dist) %>% return

}

