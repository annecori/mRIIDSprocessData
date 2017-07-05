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

    # when the non-increasing count is at the end
    dates <- seq(from=as.Date("01/01/2017"), length.out=7, by=1)
    cases <- c(1:6, 3)
    cum.incidence <- data.frame(Date=dates, Cases=cases)

    cum.incidence.fixed <- cum.incidence[-c(4, 5, 6),]
    cum.incidence.test <- make_monotonically_increasing(cum.incidence)

    expect_identical(cum.incidence.fixed, cum.incidence.test)


    cases <- c(1:4, 7, 9, 8)
    cum.incidence <- data.frame(Date=dates, Cases=cases)
    cum.incidence.fixed <- cum.incidence[-6,]


    cum.incidence.test <- make_monotonically_increasing(cum.incidence)
    expect_identical(cum.incidence.fixed, cum.incidence.test)

})
