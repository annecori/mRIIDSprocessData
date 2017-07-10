simulate.outliers <- function(n.outliers, max.val) {
                                        #sample(0:max.val, n.outliers, replace=TRUE) %>% return
    rpois(n.outliers, lambda=max.val)
}

perturb.cumincid <-  function(Cases, n.perturb=10, mu.perturb){

    if( length(Cases) < n.perturb){
        stop("cannot perturb more positions than items in the input.", do.call=FALSE)
    }
                                        # choose random places in the input to add poisson noise
    perturb.at <- sample(1:length(Cases), n.perturb, replace=TRUE)
    perturb.by <- rpois(n.perturb, lambda=mu.perturb)
    Cases[perturb.at] %<>% `+`(perturb.by)
}

simulate.cumincid <- function(n=100, lambda, n.outliers=1){

    if(n.outliers > n){
        stop("cannot replace more items than there are in the input.", do.call=FALSE)
    }
    cum.incidence <- rpois(n, lambda) %>% cumsum %>% rev
    last.few <- simulate.outliers(n.outliers, min(cum.incidence))
    cum.incidence[1:n.outliers] <- last.few
    cum.incidence %<>% rev
}


test_outlier_removal <- function(lambda, length.out, p.within.k, use.last, n.outliers, perturb=TRUE, ...){

  dates <- seq(from=as.Date("01/01/2017"), length.out=length.out, by=1)
  cases <-  simulate.cumincid(length.out, lambda, n.outliers=n.outliers)
  if(perturb) cases <- perturb.cumincid(cases, ...)
  cum.incidence <- data.frame(Date=dates, Cases=cases)


  k.sd <- interval.width.for.p(use.last, 1 - p.within.k) %>% sqrt %>% `[`(2)
  no.outliers <- remove.last.outliers(cum.incidence, use.last=use.last, k.sd=k.sd)

  list(cum.incidence, no.outliers)
}


# Params
length.out <- 20
p.within.k <- 0.45
use.last <- 15
n.outliers <- 3
num.sims <- 4
lambda <- 5
perturb <- TRUE
n.perturb <- 10
mu.perturb <- 3
params <- paste("n =",length.out, ", p.within.k =",p.within.k, ",
                   use.last =",use.last, ", n.outliers =",n.outliers)

lapply(3:num.sims, function(i) {

    ops <- test_outlier_removal(lambda, length.out, p.within.k, use.last, n.outliers, perturb, n.perturb, mu.perturb)
    cum.incidence <- ops[[1]]
    no.outliers <- ops[[2]]

    rows.removed <- nrow(cum.incidence) - nrow(no.outliers)
    params <- paste(params, ", rows.removed =",rows.removed)
    outfile <- paste("output/", i, ".pdf", sep="")
    plot_fixed_cumincid(cum.incidence, no.outliers, params, outfile)
})
