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
