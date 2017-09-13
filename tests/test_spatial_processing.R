testthat::test_that("flow__matrix is correct for various models", {

  pops <- c(a = 1000, b = 2000)
  lat  <- c(12.5, 34.5)
  long <- c(-70, 69)

  K          <- 1
  pow_N_from <- 1
  pow_N_to   <- 1
  pow_dist   <- 1
  flow.mat   <- flow_matrix(longitude = long,
                            latitude  = lat,
                            population = pops,
                            place.names = letters[1:2],
                            model = "gravity",
                            K = K, pow_N_from = pow_N_from,
                            pow_N_to = pow_N_to, pow_dist = pow_dist)
  flow.mat   %<>% round(2)
  ## Calculated by hand
  right.flow <- matrix(c(NA, 0.15, 0.15, NA), nrow = 2, byrow = FALSE)
  expect_true(all(is.na(flow.mat[1, 1]) &
                        flow.mat[2, 1] == right.flow[2, 1] &
                        is.na(flow.mat[2, 2]) &
                        flow.mat[1, 2] == right.flow[1, 2]))
  ## Asymmetric powers
  K          <- 1
  pow_N_from <- 1
  pow_N_to   <- 2
  pow_dist   <- 1
  flow.mat   <- flow_matrix(longitude = long,
                            latitude  = lat,
                            population = pops,
                            place.names = letters[1:2],
                            model = "gravity",
                            K = K, pow_N_from = pow_N_from,
                            pow_N_to = pow_N_to, pow_dist = pow_dist)
  flow.mat   %<>% round(2)
  ## Calculated by hand
  right.flow <- matrix(c(NA, 150.99, 301.98, NA), nrow = 2, byrow = FALSE)
  expect_true(all(is.na(flow.mat[1, 1]) &
                        flow.mat[2, 1] == right.flow[2, 1] &
                        is.na(flow.mat[2, 2]) &
                        flow.mat[1, 2] == right.flow[1, 2]))

})
## For the purposes of this test, we will ignore the fact that rowSums of p.movement should be 1.
testthat::test_that("p.movement is correct for different lengths of p.stay", {
    r.risk     <- matrix(1, nrow = 3, ncol = 3)
    p.stay     <- 0.3
    p.movement <- probability_movement(relative.risk = r.risk,
                                      p.stay = p.stay)
    correct    <- matrix(c(0.3, 0.7, 0.7,
                           0.7, 0.3, 0.7,
                           0.7, 0.7, 0.3), nrow = 3, byrow = FALSE)
    error      <- round(p.movement - correct, 2)
    expect_true(all(error == 0))

    p.stay     <- c(0.3, 0.7)
    p.movement <- probability_movement(relative.risk = r.risk,
                                              p.stay = p.stay)
    correct    <- matrix(c(0.3, 0.3, 0.7,
                           0.7, 0.7, 0.7,
                           0.7, 0.3, 0.3), nrow = 3, byrow = FALSE)
    error      <- round(p.movement - correct, 2)
    expect_true(all(error == 0))

    p.stay     <- c(0.3, 0.7, 0.2)
    p.movement <- probability_movement(relative.risk = r.risk,
                                              p.stay = p.stay)
    correct    <- matrix(c(0.3, 0.3, 0.8,
                           0.7, 0.7, 0.8,
                           0.7, 0.3, 0.2), nrow = 3, byrow = FALSE)
    error      <- round(p.movement - correct, 2)
    expect_true(all(error == 0))


    p.stay     <- c(0.3, 0.7, 0.2, 0.9)
    p.movement <- probability_movement(relative.risk = r.risk,
                                       p.stay = p.stay)
    ## correct is same as before. Expect the last number to be ignored.
    error      <- round(p.movement - correct, 2)
    expect_true(all(error == 0))
})
