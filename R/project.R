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
##' given set of locations, the Reproduction numbers and
##' the distribution of the serial interval.
##' Runs a single simulation.
##' @export
##' @param incid A data frame of incidence counts for n locations and t days.
##' Dates run down the rows and locations are across columns.
##' @param R either a 1 X n matrix of reproduction numbers
##' for each location or a matrix of n.days X n with R for
##' each time instant and each location for which projection
##' is to be made.
##' @param si A vector drawn from serial interval
##' distribution. If the length of the vector is less than
##' the nrow(incid) + n.days, it is padded with 0s.
##' @param pij a n x n matrix of probabilities where n is
##' the number of locations.
##' @param n.days number of days for which a simulation
##' should be run.
##' @return data frame containing projected incidence count
##' with n.days rows and n columns.
##' @author Sangeeta Bhatia
project <-  function(incid, R, si, pij, n.days = 7){

    if(ncol(R) != ncol(incid)){
        stop(" R should be a either a 1 X N or T X N matrix.")
    }
    if(nrow(R) != nrow(incid)){
        if(nrow(R) != 1){
            stop("R should be a either a 1 X N or T X N matrix.")
        }
    }

    n.loc <- ncol(incid)
    out   <- matrix(0, nrow = n.days, ncol = n.loc) %>% rbind(incid, .)
    start <- nrow(incid) + 1
    end   <- nrow(out)

    if(nrow(R) == 1){
        R <- matrix(R,
                    nrow = end - start + 1,
                    ncol = ncol(incid), byrow = TRUE)
    }
    if(length(si) < end)
         ws <- c(si, rep(0, end - length(si))) %>% rev
    else ws <- rev(si)
    for(i in start:end){
        i_t      <- out[1:i, ]
        w_t      <- utils::tail(ws, i)
        r_t      <- R[i - start + 1, ]
        mu       <- lambda.j.t(pij, r_t, i_t, w_t)
        out[i, ] <- rpois(n.loc, mu)
    }
    return(out[start:nrow(out), ])
}


