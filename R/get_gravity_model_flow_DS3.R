#' Uses a gravity model to estimate population flow from a location to another
#'
#' This function BLABLA
#'
#' @export
#'
#' @author Pierre Nouvellet and Anne Cori
#'
#' @param N_from population size of the origin location
#' @param N_to population size of the destination location
#' @param dist distance between the two locations
#' @param pow_N_from power parameter to be used in gravity model for the population size of the origin location. Defaults to 1.
#' @param pow_N_to power parameter to be used in gravity model for the population size of the destination location. Defaults to 1.
#' @param pow_dist power parameter to be used in gravity model for the distance between the two locations. Defaults to 1.
#'
#' @return a numeric value giving, to a multiplicative constant, the estimated population flow 
#' from the origin to the destination location, according to a gravity model (see details)
#'
#' @details the population flow is calculated as: 
#' N_from ^ pow_N_from * N_to ^ pow_N_to / ( dist ^ pow_dist )
#' 
#' @references Viboud et al. 2006. “Synchrony, Waves, and Spatial Hierarchies in the Spread of Influenza.” Science
#' 
#' @examples
#'
#' require(geosphere)
#' 
#' # approximate population and location of Paris and London
#' paris <- data.frame(pop = 2.2*10^6, lat = 48.8566, lon = 2.3522)
#' london <- data.frame(pop = 8.7*10^6, lat = 51.5074, lon = - 0.1278)
#' berlin <- data.frame(pop = 3.5*10^6, lat = 52.5200, lon = 13.4050)
#' 
#' # compute distances
#' dist_paris_london <- as.numeric(distm(paris[,c('lon','lat')], london[,c('lon','lat')]))
#' dist_paris_berlin <- as.numeric(distm(paris[,c('lon','lat')], berlin[,c('lon','lat')]))
#' 
#' # compute population flows from Paris to London and Berlin (to a multiplicative constant)
#' flow_paris2london <- get_gravity_model_flow_DS3(paris$pop, london$pop, dist_paris_london)
#' flow_paris2berlin <- get_gravity_model_flow_DS3(paris$pop, berlin$pop, dist_paris_berlin)

get_gravity_model_flow_DS3 <- function(N_from, N_to, dist, pow_N_from = 1, pow_N_to = 1, pow_dist = 1)
{
  out <- N_from ^ pow_N_from * N_to ^ pow_N_to / ( dist ^ pow_dist )
  
  return(out)
}
