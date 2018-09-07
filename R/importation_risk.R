##' Overall Infectivty at a given source
##'
##' @details Infectivity at a location i is
##' \deqn{ R * \sum_{s = 1}^t{I_{t - s}\omega_s}}
##' where I is the incidence and $\omega$ is the
##' serial interval.
##' @title Overll Infectivty at a source
##' @param incid incidence vector
##' @param si serial interval distribution
##' @param R reproduction number at time t at
##' the source.
##' @return infectivity at source
##' @author Sangeeta Bhatia
##'
infectivity_at_source <- function(incid, si, R) {
    R * EpiEstim::overall_infectivity(incid,
                                      si)
}
##' Weighted relative risk of importation
##'
##' @details the relative risk at a location
##' j is the sum of the risks from all possible
##' sources weighted by the infectivity at each source.
##' @title Relative risk of importation
##' @param wts vector of weights for each source.
##' @param a vector of the same length as wts.
##' rel_risk relative risk at j
##' is proportional to the relative flow into j
##' from i. Depending on the question, this can
##' be the proportion of flow out
##' of i into j i.e.,
##' \deqn{\frac{\phi_{i \rightarrow j}}{\sum_x{\phi_{i \rightarrow x}}},}
##' or the proportion of flow into j that is coming
##' from i
##' \deqn{\frac{\phi_{i \rightarrow j}}{\sum_x{\phi_{x \rightarrow j}}}.}
##' @return estimated weighted risk
##' @author Sangeeta Bhatia
weighted_risk <- function(wts, rel_risk) {
    (wtd %*% rel_risk)[1, 1]
}
