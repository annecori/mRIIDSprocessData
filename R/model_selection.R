##' Log-likelihood of observing the given incidence under the model.
##'
##' @details The model is
##' \deqn{I_{j, t} = \sum_{i = 1}^N{p_{ij} R_{i, t} \sum_{s = 1}^t{I_{i, t - s}si_s}}}
##' @title Log likelihood under given model.
##' @param I0  T X N matrix of incidence
##' @param pij N X N.  Element i, j is the
##' probability of moving from i to j.
##' @param R either 1 X N matrix of reproduction numbers in each
##' location at t or T X N matrix of reproduction numbers in each
##' location at each time step from 1 to t.
##' @param si A vector drawn from serial interval
##' distribution. If the length of the vector is less than
##' T, it is padded with 0s.
##' @param x N X 1 vector of observed incidence at step t.
##' @return log likelihood of observing x at step t.
##' @author Sangeeta Bhatia
##' @export
log_likelihood_t <- function(I0, pij, R, si, x) {
    if (length(si) < nrow(I0))
         ws <- rev(c(si, rep(0, nrow(I0) - length(si))))
    else ws <- rev(si)
    lambda <- lambda_j_t(pij, R, I0, ws)
    ## probability of observing x
    ## given lambda
    loglh <- sum(dpois(x,
                       lambda = lambda,
                       log = TRUE))
    loglh
}

##' Deviance at a given set of parameters.
##' Deviance is a measure of the model discrepancy and is defined as -2
##' times the log likelihood.
##' @details \deqn{D(y, \theta) = -2 logp(y|\theta)}
##' @title Deviance
##' @inheritParams log_likelihood_t
##' @return deviance
##' @author Sangeeta Bhatia
##' @references Gelman, Andrew, et al. Bayesian data analysis.
##' Chapman and Hall/CRC, 1995.
##' @export
deviance <- function(I0, pij, R, si, x) {
    -2 * log_likelihood_t(I0, pij, R, si, x)
}


##' Deviance averaged over the posterior distribution.
##'
##' @details \deqn{\hat{D}_{avg}(y) =
##' \frac{1}{L}\sum_{l = 1}^{L}{D(y, \theta^{l})}}
##' @title Estimated average deviance
##' @param I0 T X N incidence matrix
##' @param pij list of matrices. Each matrix is the probability of
##' movement for a single simulation. If a list is shorter than other
##' lists, it will be recycled.
##' @param R list of reproduction vectors. Same as for the other
##' parameters.
##' @param si serial interval vector.
##' @param x vector for which deviance is sought.
##' @return Deviance averaged over the simulations.
##' @author Sangeeta Bhatia
##' @references Gelman, Andrew, et al. Bayesian data analysis. Chapman
##' and Hall/CRC, 1995.
deviance_avg <- function(I0, pijs, Rs, si, x) {

    devs <- purrr::map(pijs, function(p) {
        purrr::map(Rs, function(r) {
            mRIIDS::deviance(I0, p, r, si, x)
        })})

    devs <- unlist(devs)
    mean(devs)
}

##' Effective number of unconstrained parameters in the model
##'
##' @details \deqn{p_D^1 = \hat{D}_{avg}(y) - D_{\hat{\theta}}(y)}
##' @title Effective number of unconstrained parameters in the model
##' @return
##' @author Sangeeta Bhatia
##' @references Gelman, Andrew, et al. Bayesian data analysis. Chapman
##' and Hall/CRC, 1995.
params_eff <- function(I0, pijs, Rs, pij_point, R_point, si, x) {
    dev <- deviance(I0, pij_point, R_point, si, x)
    dev_avg <- deviance_avg(I0, pijs, Rs, si, x)
    neff <- dev_avg - dev
    neff
}

##' Deviance Information Criterion
##'
##' @details \deqn{DIC = 2\hat{D}_{avg}(y) - D_{\hat{\theta}}(y)}
##' @title Deviance Information Criterion
##' @inheritParams deviance_avg
##' @return dic
##' @author Sangeeta Bhatia
##' @references Gelman, Andrew, et al. Bayesian data analysis. Chapman
##' and Hall/CRC, 1995.
##' @export
dic <- function(I0, pijs, Rs, pij_point, R_point, si, x) {

    ## Estimate deviance at point estimates
    dev <- deviance(I0, pij_point, R_point, si, x)
    ## Estimate deviance averaged over simulations
    dev_avg <- deviance_avg(I0, pijs, Rs, si, x)
    dic <- (2 * dev_avg) - dev
    dic
}
