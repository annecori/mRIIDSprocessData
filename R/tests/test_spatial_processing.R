testthat::test_that("flow__matrix is correct for various models", {
    n_from <- c(1000, 2000, 3500)
    n_to <- c(1234, 900, 450)
    distance <- c(1000, 1500, 4500)
    correct_flow <- c(1234, 1200, 350)
    computed_flow <- flow_vector(n_from, n_to, distance)

    # check if all is good over vectors
    expect_true(all(correct_flow==computed_flow))
})
