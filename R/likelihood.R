##' One step ahead likelihood
##'
##' The parameters are the same as in lambda.j.t.
##' @title
##' @param incid
##' @param window how many previous days are to be considered in determining lambda
##' @param start integer. the index at which to start computing log likelihood
##' @param end integer.
##' @return a vector of length end - start + 1. Element i is the sum of the log likelihoods
##' across all locations at time i.
##' @author Sangeeta Bhatia
log_likelihood_1step <- function(incid, window = 2, start, end, pij, R, si){
    n.loc <- ncol(incid)
    loglh <- c()
    if(length(si) < end)
       ws    <- c(si, rep(0, end - length(si))) %>% rev
    else ws  <- rev(si)
    for(i in start:end){
        i_t    <- incid[(i - window + 1):i, ]
        w_t    <- utils::tail(ws, nrow(i_t))
        lambda <- lambda.j.t(pij, R, i_t, w_t)
        loglh  %<>% c(sum(dpois(incid[i, ], lambda = lambda, log = TRUE)))
    }
    loglh
}
