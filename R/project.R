##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param p_ij : row vector of length n where n is the number of locations. Each entry is the probability of flow from i to j.
##' @param r_t : t X n matrix. Entry ij is the reproduction number at location j and time i.
##' @param i_t : t X n matrix. Entry ij is the incidence count at location j and time i.
##' @param w_t : column vector of length t. Samples from the serial distribution.
##' @title
##' @return the mean of the Poisson distribution determining the distribution of incidences at location j at time t.
##' @author Sangeeta Bhatia
lambda.j.t <- function(pij, r.t, incidence, ws){
    out <- (ws  %*% incidence) * r.t
    out <-  out %*% pij
    out
}

##' Project future incidence at a location
##' Simulates future incidence based on past incidence data at a
##' given set of locations, the Reproduction numbers at each location and the distribution of the serial interval.
##' Runs a single simulation.
##' @export
##' @param incid A data frame of incidence counts for n locations and t days.
##' Dates run down the rows and locations are across columns. Must contain a column called Date.
##' @param R A positive vector of reproduction numbers for each location.
##' @param si A vector
##' @param pij a n x n matrix of probabilities where n is the number of locations.
##' @param n.days number of days for which each simulation should be run.
##' @return data frame containing projected incidence count with n.days rows and n columns.
##' @author Sangeeta Bhatia
project <-  function(incid, R, si, pij, n.days = 7){

    n.loc <- ncol(incid)
    out   <- matrix(0, nrow = n.days, ncol = n.loc)
    out   <- rbind(incid, out)
    start <- nrow(incid) + 1
    end   <- nrow(out)
    if(length(si) < end)
         ws <- c(si, rep(0, end - length(si)))
    else ws <- si

    ws <- rev(ws)
    for(i in start:end){
        i_t      <- out[seq_len(i), ]
        w_t      <- utils::tail(ws, i)
        mu       <- lambda.j.t(pij, R, i_t, w_t)
        out[i, ] <- rpois(n.loc, mu)

    }
    return(out[start:nrow(out), ])
}


## same as project except that R is a matrix of t * n allowing
## reproduction number to vary temporally as well as spatially.
## n is the number of locations and t = nrow(incid) + n.days
project2 <-  function(incid, R, si, pij, n.days = 7){

    n.loc <- ncol(incid)
    out   <- matrix(0, nrow = n.days, ncol = n.loc) %>% rbind(incid, .)
    start <- nrow(incid) + 1
    end   <- nrow(out)
    if(length(si) < end)
         ws <- c(si, rep(0, end - length(si))) %>% rev
    else ws <- rev(si)
    for(i in start:end){
        i_t      <- out[1:i, ]
        w_t      <- utils::tail(ws, i)
        r_t      <- R[i, ]
        mu       <- lambda.j.t(pij, r_t, i_t, w_t)
        out[i, ] <- rpois(n.loc, mu)
    }
    return(out[start:nrow(out), ])
}


