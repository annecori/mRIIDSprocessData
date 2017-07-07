test_that("is_monotonically_increasing works as expected", {
    mono_inc <-  c(1, 2, 3, 4)
    nowhere <- mono_inc %>% is_monotonically_increasing %>% length
    expect_equal(nowhere, 0)

    mono_noninc <- c(1, 2, 3, 3)
    nowhere <- mono_noninc %>% is_monotonically_increasing %>% length
    expect_equal(nowhere, 0)

    decr <- c(1, 2, 3, 4, 2, 1)
    somewhere <- decr %>% is_monotonically_increasing
    right_ans <- c(4, 5)
    expect_true(all(somewhere==right_ans))
})


test_that("missing data and non increasing cumulative counts are handled correctly", {


    dates <- seq(from=as.Date("01/01/2017"), length.out=7, by=1)
    cases <- c(1:6, 3)
    cum.incidence <- data.frame(Date=dates, Cases=cases)

                                        # Outliers at the end should be removed
    remove.last.outliers(cum.incidence, use.last=2, k.sd=2)
    # when the non-increasing count is at the end


    cum.incidence.fixed <- cum.incidence[-c(4, 5, 6),]
    cum.incidence.test <- make_monotonically_increasing(cum.incidence)

    expect_identical(cum.incidence.fixed, cum.incidence.test)


    cases <- c(1:4, 7, 9, 8)
    cum.incidence <- data.frame(Date=dates, Cases=cases)
    cum.incidence.fixed <- cum.incidence[-6,]


    cum.incidence.test <- make_monotonically_increasing(cum.incidence)
    expect_identical(cum.incidence.fixed, cum.incidence.test)

})

plot_fixed_cumincid <- function(cum.incidence, no.outliers, title, outfile){
    p = ggplot() + geom_point(data=cum.incidence, aes(Date, Cases)) +
    geom_point(data=no.outliers, aes(Date, Cases), color='red') + ggtitle(title)

    ggsave(outfile, p)
}

test_outlier_removal <- function(lambda, length.out, p.within.k, use.last, n.outliers, perturb=TRUE, ...){

  dates <- seq(from=as.Date("01/01/2017"), length.out=length.out, by=1)
  cases <-  simulate.cumincid(length.out, lambda, n.outliers=n.outliers)
  if(perturb) cases <- perturb.cumincid(cases, ...)
  cum.incidence <- data.frame(Date=dates, Cases=cases)

  k.sd <- interval.width.for.p(use.last, 1 - p.within.k) %>% sqrt %>% `[`(2)
  no.outliers <- remove.last.outliers(cum.incidence, use.last=use.last, k.sd=k.sd)

  return(list(cum.incidence, no.outliers))
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
