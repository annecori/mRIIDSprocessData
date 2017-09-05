## Only a test. The numbers do not make sense epidemiologically.
testthat::test_that("mean for projection model is correct", {
    p.movement <- matrix(c(0.5, 0.2, 0.3,
                           0.2, 0.5, 0.2,
                           0.3, 0.2, 0.5), nrow = 3, byrow = FALSE)

    incidence  <- c(1:12, nrow = 4, byrow = FALSE)
    r.t        <- matrix(c(1, 3, 5, 7,
                          2, 4, 6, 8,
                          1, 1, 3, 4),
                        nrow = 4, byrow = FALSE)
    w.s        <- 1:4
    mu.correct <- c(286, 388, 338)
    mu.test    <- lamda.j.t(p.movement, incidence, w.s, r.t)
}
