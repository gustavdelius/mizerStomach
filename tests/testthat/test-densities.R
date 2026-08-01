# Tests for densities.R

test_that("dtexp returns non-negative values and integrates to ~1", {
    x <- seq(0, 20, length.out = 200)
    d <- dtexp(x, alpha = 0.5, ll = 2, ul = 20, lr = 15, ur = 20)
    expect_true(all(d >= 0))
    # Numerical integration should be close to 1
    integral <- integrate(dtexp, 0, 30,
                          alpha = 0.5, ll = 2, ul = 20, lr = 15, ur = 20)
    expect_equal(integral$value, 1, tolerance = 0.01)
})

test_that("dtexp returns NA for degenerate parameters", {
    x <- seq(0, 10, length.out = 50)
    # Very extreme parameters that break the density
    result <- dtexp(x, alpha = 100, ll = -100, ul = 0.001, lr = -200, ur = 0.001)
    expect_true(all(is.na(result)))
})

test_that("dtexp evaluates outside its normalization interval", {
    x <- c(-1, 31)
    d <- dtexp(x, alpha = 0.5, ll = 2, ul = 20, lr = 15, ur = 20)

    expect_length(d, length(x))
    expect_true(all(is.finite(d)))
    expect_true(all(d > 0))
})

test_that("dtexp rejects a non-finite requested density as a unit", {
    result <- dtexp(
        c(1, 1e6), alpha = 0.5, ll = 2, ul = 20, lr = 15, ur = 20
    )

    expect_true(all(is.na(result)))
})

test_that("get_density returns correct values for normal fit", {
    fit <- data.frame(species = "A", distribution = "normal",
                      mean = 5, sd = 2, stringsAsFactors = FALSE)
    x <- seq(0, 10, length.out = 50)
    d <- get_density(x, fit)
    expected <- dnorm(x, mean = 5, sd = 2)
    expect_equal(d, expected)
})

test_that("get_density returns non-negative values for trunc_exp fit", {
    fit <- data.frame(species = "B", distribution = "trunc_exp",
                      alpha = 0.5, ll = 2, ul = 20, lr = 15, ur = 20,
                      stringsAsFactors = FALSE)
    x <- seq(0, 20, length.out = 50)
    d <- get_density(x, fit)
    expect_true(all(d >= 0))
})

test_that("get_density returns correct values for gauss_mix fit", {
    fit <- data.frame(species = "C", distribution = "gauss_mix",
                      stringsAsFactors = FALSE)
    fit$mean <- I(list(c(3, 7)))
    fit$sd <- I(list(c(1, 1.5)))
    fit$p <- I(list(c(0.4, 0.6)))
    x <- seq(0, 12, length.out = 50)
    d <- get_density(x, fit)
    expected <- 0.4 * dnorm(x, 3, 1) + 0.6 * dnorm(x, 7, 1.5)
    expect_equal(d, expected)
})

test_that("get_density returns a vector for single-row fit", {
    fit <- data.frame(species = "A", distribution = "normal",
                      mean = 5, sd = 2, stringsAsFactors = FALSE)
    x <- seq(0, 10, length.out = 20)
    d <- get_density(x, fit)
    expect_true(is.numeric(d))
    expect_equal(length(d), 20)
})

test_that("get_density returns a matrix for multi-row fit", {
    fit <- data.frame(species = c("A", "B"), distribution = "normal",
                      mean = c(5, 7), sd = c(1, 2),
                      stringsAsFactors = FALSE)
    x <- seq(0, 12, length.out = 20)
    d <- get_density(x, fit)
    expect_true(is.matrix(d))
    expect_equal(ncol(d), 2)
    expect_equal(nrow(d), 20)
    expect_equal(colnames(d), c("A", "B"))
})

test_that("get_density dispatches each row of a mixed-family fit", {
    fit <- data.frame(
        species = c("normal", "truncated"),
        distribution = c("normal", "trunc_exp"),
        mean = c(5, NA), sd = c(2, NA),
        alpha = c(NA, 0.5), ll = c(NA, 2), ul = c(NA, 20),
        lr = c(NA, 15), ur = c(NA, 20),
        stringsAsFactors = FALSE
    )
    x <- seq(1, 10, length.out = 20)
    d <- get_density(x, fit)

    expect_equal(d[, "normal"], dnorm(x, 5, 2))
    expect_equal(
        d[, "truncated"],
        dtexp(x, alpha = 0.5, ll = 2, ul = 20, lr = 15, ur = 20)
    )
})

test_that("get_density treats power as metadata", {
    fit <- data.frame(
        species = "A", distribution = "normal", mean = 5, sd = 2,
        power = 1
    )

    expect_equal(get_density(5, fit), dnorm(5, 5, 2))
})

test_that("get_density_single accepts mixture vectors in one-row lists", {
    fit <- list(
        distribution = "gauss_mix",
        p = list(c(0.25, 0.75)),
        mean = list(c(2, 6)),
        sd = list(c(1, 2))
    )
    x <- c(2, 6)

    expect_equal(
        mizerStomach:::get_density_single(x, fit),
        0.25 * dnorm(x, 2, 1) + 0.75 * dnorm(x, 6, 2)
    )
})
