context("aggregate_projections")

testthat::test_that("Rows of data frame are shuffled",{
    df <- data.frame(A = 1:3, B = 4:6, C = 7:9)
    correct <- data.frame(A = 7:9, B = c(4, 5, 3), C = c(1, 2, 6))
    set.seed(42)
    shuffled <- shuffle_cols(df)
    expect_true(all(shuffled == correct))
})


testthat::test_that("Random sample for a given place is returned", {
    df1 <- data.frame(date = 1:10, p1 = 1:10, p2 = 11:20)
    df2 <- data.frame(date = 1:10, p1 = 11:20, p2 = 1:10)
    df3 <- data.frame(date = 1:10,
                      p1 = letters[1:10],
                      p2 = letters[11:20])

    projections_list <- list(df1, df2, df3)
    correct <- data.frame(sim = letters[1:10],
                          sim2 = 11:20,
                          place = "p1")

    set.seed(42)
    sampled <- sample_projections(projections_list, place = "p1", k = 2)
    expect_true(all(correct == sampled))

})
