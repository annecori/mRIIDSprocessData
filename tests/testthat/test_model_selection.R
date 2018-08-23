context("Model Selection")
testthat::test_that("Log likelihood gives expected result", {
    p.movement <- matrix(c(0.5, 0.2, 0.3,
                           0.2, 0.5, 0.2,
                           0.3, 0.3, 0.5), nrow = 3, byrow = FALSE)

    incid <- matrix(1:12, nrow = 4, byrow = FALSE)
    r.t   <- c(7, 8, 4)
    si    <- 1:4
    x <- c(267, 346, 415)
    lh.correct <- -13.2
    lh.test <- log_likelihood_t(incid,
                                p.movement,
                                r.t,
                                si,
                                x)
    lh.test <- round(lh.test, 2)
    expect_true(lh.correct == lh.test)
})

