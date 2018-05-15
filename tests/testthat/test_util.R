testthat::test_that("Log likelihood is correct", {

    freq.table <- data.frame(x = 1:5, prob = c(0.1, 0.1, 0.2, 0.4, 0.2))
    obs <- c(1, 4, 5)
    ll  <-  log_likelihood(obs, freq.table) %>% round(2)
    correct_ll  <- -4.83

    expect_equal(ll, correct_ll)

    obs <- c(1, 6)
    expect_true(is.infinite(log_likelihood(obs, freq.table)))
})

testthat::test_that("Log likelihood for a place for a bunch of dates", {
    observed  <- data.frame(Date = 1:4, incid = c(5, 2, 1, 0))
    predicted <- data.frame(Date = rep(1:4, each = 5),
                            incid = c(2, 1, 3, 5, 4,
                                      3, 0, 2, 2, 0,
                                      3, 0, 2, 1, 3,
                                      2, 2, 0, 0, 1))
    correct_ll <- -5.05
    ll         <- log_likelihood_atj(observed, predicted) %>% round(2)
    expect_equal(ll, correct_ll)

})

testthat::test_that("Last date of the week is returned", {
    year <- 2018
    week <- 1
    correct <- lubridate::ymd("2018-01-07")
    expect_equal(week_to_maxdate(year, week), correct)

    week <- 13
    correct <- lubridate::ymd("2018-04-01")
    expect_equal(week_to_maxdate(year, week), correct)

    week <- 52
    correct <- lubridate::ymd("2018-12-30")
    expect_equal(week_to_maxdate(year, week), correct)

    ## leap year
    year <- 2016
    week <- 52
    correct <- lubridate::ymd("2016-12-30")
    expect_equal(week_to_maxdate(year, week), correct)

})

