## Only a test. The numbers do not make sense epidemiologically.
testthat::test_that("mean for projection model is correct", {
    p.movement <- matrix(c(0.5, 0.2, 0.3,
                           0.2, 0.5, 0.2,
                           0.3, 0.3, 0.5), nrow = 3, byrow = FALSE)

    incidence  <- matrix(1:12, nrow = 4, byrow = FALSE)
    r.t        <- c(7, 8, 4)
    ws         <- 4:1
    mu.correct <- c(286, 348, 386)
    mu.test    <- lambda_j_t(p.movement, r.t, incidence, ws)
    expect_true(all(mu.correct == mu.test))
})


