##' .. Given the populations at two places and the distances between them, returns the flow
##' vector under the specified model ..
##' .. The models are : gravity and radiation ..
##'
##' @title
##' @param K
##' @param alpha
##' @param beta
##' @param gamma
##' @param model
##' @return
##' @author Sangeeta Bhatia

flow_vector <-  function(N_from, N_to, distance,model=c("gravity"), ...) {
    params      <- list(...)
    if(model == "gravity"){
        K          <- params$K
        pow_N_from <- params$pow_N_from
        pow_N_to   <- params$pow_N_to
        pow_dist   <- params$pow_dist
        gravity_model_flow(N_from, N_to, distance, K, pow_N_from, pow_N_to, pow_dist)
    } else if(model=="radiation") stop("Model not yet implemented")
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

   K * (N_from^pow_N_from) * (N_to^pow_N_to) / (distance^pow_dist)

}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param
##' @return
##' @author Sangeeta Bhatia
flow_matrix <-  function(longitude, latitude, population, place.names,
                           model=c("gravity"), ...){

    distances <- geosphere::distm(cbind(longitude, latitude))
    distances <- distances[lower.tri(distances)] # Extract the distances vector
    pairs     <- length(latitude) %>% combn(2)
    n_from    <- population[pairs[1, ]]
    n_to      <- population[pairs[2, ]]


    flow.matrix           <- matrix(NA, length(latitude), length(latitude))
    rownames(flow.matrix) <- place.names
    colnames(flow.matrix) <- place.names
    ## fill in the matrix from the vectors
    flow_from_to <- flow_vector(n_from, n_to, distances, model, ...)
    flow.matrix[lower.tri(flow.matrix)] <- flow_from_to
    flow.matrix <- t(flow.matrix) # fill out the upper triangle

    flow_to_from <- flow_vector(n_to, n_from, distances, model, ...)
    flow.matrix[lower.tri(flow.matrix)] <- flow_to_from # fill out the lower triangle
    flow.matrix
}
##' Probability of moving from location i to j
##'
##' the probability of moving from location i to location j is given by
##' (1 - p_stay_at_i) * (flow_from_i_to_j/(total outward flow from i))
##' @title
##' @param relative.risk n * n matrix where n = n.locations
##' @param p.stay a vector of length n where the ith entry specifies the probability of
##' staying at location i. If length of p.stay is less than n, elements will be recycled.
##' @return a n * n matrix specifying the population flow between n locations
##' @author Sangeeta Bhatia
probability_movement <- function(relative.risk, p.stay){
    if(nrow(relative.risk) != ncol(relative.risk)){
        stop("relative.risk should be a square matrix.")
    }
    n.countries      <- nrow(relative.risk)
    p.mat            <- matrix(rep(p.stay, each = n.countries,
                                   length.out = n.countries^2),
                               nrow = n.countries, byrow = TRUE)
    p.mat            <- 1 - p.mat
    p.movement       <- relative.risk * p.mat
    diag(p.movement) <- rep(p.stay, each = 1, length.out = n.countries)
    p.movement
}
