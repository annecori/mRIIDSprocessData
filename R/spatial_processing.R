##' .. Given the populations at two places and the distances between
##' them, returns the flow vector under the specified model ..
##' The models are : gravity and radiation.
##'
##' @title
##' @param K
##' @param alpha
##' @param beta
##' @param gamma
##' @param model
##' @return
##' @author Sangeeta Bhatia
flow_vector <- function(N_from,
                        N_to,
                        distance,
                        model = c("gravity"),
                        ...) {
    params      <- list(...)
    if (model == "gravity") {
      K          <- params$K
      pow_N_from <- params$pow_N_from
      pow_N_to   <- params$pow_N_to
      pow_dist   <- params$pow_dist
      gravity_model_flow(N_from, N_to, distance, K,
                         pow_N_from, pow_N_to, pow_dist)
    } else if (model == "poisson_gravity") {
      K          <- params$K
      pow_N_from <- params$pow_N_from
      pow_N_to   <- params$pow_N_to
      pow_dist   <- params$pow_dist
      poisson_gravity(N_from, N_to, distance, K,
                      pow_N_from, pow_N_to, pow_dist)
    } else if (model == "gravity_alt") {
        tau <- params$tau
        rho <- params$rho
        alpha <- params$alpha
        gravity_alt(N_to, distance, tau, rho, alpha)
    } else
      stop("Model not yet implemented")
 }


gravity_alt <- function(N_to, distance, tau, rho, alpha) {
    (N_to ^ tau) * ((1 + distance/rho)^(-alpha))
}

##' Flow using gravity model based on Poisson process
##'
##' @details In this model the flow between locations is
##' distributed accordin to a poisson process with mean
##' lamda_ij = exp(b0 + b1*ln(P1) + b2*ln(P2) + b3*ln(dij))
##' @title
##' @param N_from
##' @param N_to
##' @param distance
##' @param K
##' @param pow_N_from
##' @param pow_N_to
##' @param pow_dist note that this must be entered as a -ve number.
##' @return
##' @author Sangeeta Bhatia
poisson_gravity <- function(N_from,
                            N_to,
                            distance,
                            K,
                            pow_N_from,
                            pow_N_to,
                            pow_dist) {

    exp(K +
        pow_N_from * log(N_from) +
        pow_N_to * log(N_to) +
        pow_dist * log(distance))
}


##' Given the populations of A and B and the distance between them,
##' return the estimated population flow between
##' them modeled as
##' \phi(A,B) = K N_A^{\alpha}N_B^{\beta}/r_{AB}^{\gamma}..
##' @title Computes the flow from A to B under the gravity model
##' @param N_from population of the source
##' @param N_to population of the destination
##' @param dist distance between the two places
##' @param pow_N_from power on the population of the source
##' @param pow_N_to power on the population of the destination
##' @param pow_dist power on the distance between the source and the
##' destination
##' @return estimated flow between source and destination
##' @author Pierre Nouvellet, Anne Cori Sangeeta Bhatia
##' @export
gravity_model_flow <- function(N_from, N_to, distance, K,
                               pow_N_from, pow_N_to, pow_dist) {
    K * (N_from ^ pow_N_from) * (N_to ^ pow_N_to) /
        (distance ^ pow_dist)

}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param
##' @return
##' @author Sangeeta Bhatia
flow_matrix <- function(longitude,
                        latitude,
                        population,
                        place_names,
                        model = c("gravity", "gravity_alt"),
                        ...) {
    distances <- geosphere::distm(cbind(longitude, latitude))
    distances <-
      distances[lower.tri(distances)] # Extract the distances vector
    pairs     <- combn(length(latitude), 2)
    n_from    <- population[pairs[1, ]]
    n_to      <- population[pairs[2, ]]


    flow_mat  <-
      matrix(NA, length(latitude), length(latitude))
    rownames(flow_mat) <- place_names
    colnames(flow_mat) <- place_names
    ## fill in the matrix from the vectors
    flow_from_to <- flow_vector(n_from, n_to, distances, model, ...)
    flow_mat[lower.tri(flow_mat)] <- flow_from_to
    flow_mat <- t(flow_mat) # fill out the upper triangle

    flow_to_from <- flow_vector(n_to, n_from, distances, model, ...)
    flow_mat[lower.tri(flow_mat)] <-
      flow_to_from # fill out the lower triangle

    flow_mat
}

##' Probability of moving from location i to j
##'
##' the probability of moving from location i to location j is given by
##' (1 - p_stay_at_i) * (flow_from_i_to_j/(total outward flow from i))
##' @title
##' @param relative_risk n * n matrix where n = n.locations
##' @param p_stay a vector of length n where the ith entry specifies
##' the probability of staying at location i. If length of p_stay is
##' less than n, elements will be recycled.
##' @return a n * n matrix specifying the population flow between n
##' locations
##' @author Sangeeta Bhatia
probability_movement <- function(relative_risk, p_stay) {

  if (nrow(relative_risk) != ncol(relative_risk)) {
    stop("relative_risk should be a square matrix.")
  }
  n_countries      <- nrow(relative_risk)
  p_mat            <- matrix(
    rep(p_stay, each = n_countries,
        length.out = n_countries ^ 2),
    nrow = n_countries,
    byrow = TRUE
  )
  p_mat            <- 1 - p_mat
  p_movement       <- relative_risk * p_mat
  diag(p_movement) <-
    rep(p_stay, each = 1, length.out = n_countries)
  p_movement
}
