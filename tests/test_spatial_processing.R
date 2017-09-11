testthat::test_that("flow__matrix is correct for various models", {
  flow.mat  <- matrix(1, 3, 3)
  pairs     <- letters[1:3] %>% combn(2)
  pops      <- c(a = 1000, b = 2000, c = 3500)
  n_from    <- pops[pairs[1,]]
  n_to      <- pops[pairs[2, ]]
  distances <- c(1000, 1500, 4500)

  K          <- 1
  pow_N_from <- 1
  pow_N_to   <- 1
  pow_dist   <- 1
  flow_from_to <- flow_vector(n_from, n_to, distances, K=K,
                               pow_N_from = pow_N_from,
                               pow_N_to = pow_N_to,
                               pow_dist = pow_dist)

  flow.mat[lower.tri(flow.mat)] <- flow_from_to

  flow.mat <- t(flow.mat) # fill out the upper triangle

  flow_to_from <- flow_vector(n_to, n_from, distances, K=K,
                               pow_N_from = pow_N_from,
                               pow_N_to = pow_N_to,
                               pow_dist = pow_dist)

  flow.mat[lower.tri(flow.mat)] <- flow_to_from # fill out the lower triangle
  flow.mat %<>% floor
  ## Calculated by hand
  right.flow <- matrix(c(1, 2000, 2333,
                         2000, 1, 1555,
                         2333, 1555, 1), nrow = 3, byrow = FALSE)

    # check if all is good over vectors
    expect_true(all(right.flow == flow.mat))
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
