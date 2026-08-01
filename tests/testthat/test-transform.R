# Tests for transform.R

test_that("transform_normal shifts mean correctly", {
    fit <- list(mean = 5, sd = 2)
    result <- transform_normal(fit, power = 1)
    expect_equal(result$mean, 5 - 1 * 2^2)
    expect_equal(result$sd, 2)
})

test_that("transform_truncated_exp shifts alpha correctly", {
    fit <- list(alpha = 0.5, ll = 2, ul = 20, lr = 15, ur = 20)
    result <- transform_truncated_exp(fit, power = 1)
    expect_equal(result$alpha, 0.5 - 1)
    # Other parameters unchanged
    expect_equal(result$ll, 2)
    expect_equal(result$lr, 15)
})

test_that("transform_gaussian_mixture updates means and reweights p", {
    fit <- list(mean = c(3, 7), sd = c(1, 1.5), p = c(0.4, 0.6))
    result <- transform_gaussian_mixture(fit, power = 1)
    expect_equal(result$mean, c(3 - 1 * 1^2, 7 - 1 * 1.5^2))
    expect_equal(sum(result$p), 1, tolerance = 1e-10)
    expect_true(all(result$p > 0))
})

test_that("transform_gaussian_mixture equals numerical exponential tilt", {
    fit <- list(mean = c(3, 7), sd = c(1, 1.5), p = c(0.4, 0.6))
    power <- 0.7
    transformed <- transform_gaussian_mixture(fit, power = power)
    grid <- seq(-5, 18, length.out = 20001)
    original_density <- fit$p[1] * dnorm(grid, fit$mean[1], fit$sd[1]) +
        fit$p[2] * dnorm(grid, fit$mean[2], fit$sd[2])
    tilted_density <- exp(-power * grid) * original_density
    dx <- grid[2] - grid[1]
    tilted_density <- tilted_density / sum(tilted_density * dx)
    transformed_density <-
        transformed$p[1] * dnorm(grid, transformed$mean[1], transformed$sd[1]) +
        transformed$p[2] * dnorm(grid, transformed$mean[2], transformed$sd[2])

    expect_equal(transformed_density, tilted_density, tolerance = 1e-7)
})

test_that("transform_fit dispatches correctly for normal", {
    fit_df <- data.frame(species = "A", distribution = "normal",
                         mean = 5, sd = 2, power = 0,
                         stringsAsFactors = FALSE)
    result <- transform_fit(fit_df, power = 1)
    expect_equal(result$mean, 5 - 1 * 2^2)
    expect_equal(result$power, 1)
})

test_that("transform_fit dispatches correctly for trunc_exp", {
    fit_df <- data.frame(species = "B", distribution = "trunc_exp",
                         alpha = 0.5, ll = 2, ul = 20, lr = 15, ur = 20,
                         power = 0, stringsAsFactors = FALSE)
    result <- transform_fit(fit_df, power = 1)
    expect_equal(result$alpha, 0.5 - 1)
    expect_equal(result$power, 1)
})

test_that("transform_fit dispatches correctly for gauss_mix", {
    fit_df <- data.frame(species = "C", distribution = "gauss_mix",
                         power = 0, stringsAsFactors = FALSE)
    fit_df$mean <- I(list(c(3, 7)))
    fit_df$sd <- I(list(c(1, 1.5)))
    fit_df$p <- I(list(c(0.4, 0.6)))
    result <- transform_fit(fit_df, power = 1)
    expect_equal(result$power, 1)
    p <- if (is.list(result$p)) result$p[[1]] else result$p
    expect_equal(sum(p), 1, tolerance = 1e-10)
})

test_that("transform_fit errors on non-numeric power", {
    fit_df <- data.frame(species = "A", distribution = "normal",
                         mean = 5, sd = 2, stringsAsFactors = FALSE)
    expect_error(transform_fit(fit_df, power = "a"),
                 "power must be a single numeric value")
})

test_that("transform_fit with zero dp is a no-op for normal", {
    fit_df <- data.frame(species = "A", distribution = "normal",
                         mean = 5, sd = 2, power = 0,
                         stringsAsFactors = FALSE)
    result <- transform_fit(fit_df, power = 0)
    expect_equal(result$mean, 5)
    expect_equal(result$sd, 2)
})

test_that("transform_fit works on real fit from barnes_data", {
    species <- unique(barnes_data$species)[1]
    fit <- fit_log_ppmr(barnes_data, species, distribution = "normal")
    result <- transform_fit(fit, power = 1)
    expect_equal(result$power, 1)
    # Mean should shift
    expect_true(result$mean != fit$mean)
})

test_that("transform_fit uses each row's existing power and leaves input unchanged", {
    fit <- data.frame(
        species = c("A", "B"), distribution = "normal",
        mean = c(5, 7), sd = c(1, 2), power = c(0, 0.5)
    )
    original <- fit

    result <- transform_fit(fit, power = 1)

    expect_equal(result$mean, c(5 - 1, 7 - 0.5 * 4))
    expect_equal(result$power, c(1, 1))
    expect_identical(fit, original)
})

test_that("low-level transformations preserve documented unchanged parameters", {
    normal <- list(mean = 5, sd = 2, note = "kept")
    trunc <- list(alpha = 0.5, ll = 2, ul = 20, lr = 15, ur = 20)
    mixture <- list(p = c(0.4, 0.6), mean = c(3, 7), sd = c(1, 1.5))

    expect_identical(transform_normal(normal, 0.5)$note, "kept")
    expect_equal(
        transform_truncated_exp(trunc, 0.5)[c("ll", "ul", "lr", "ur")],
        trunc[c("ll", "ul", "lr", "ur")]
    )
    expect_equal(transform_gaussian_mixture(mixture, 0.5)$sd, mixture$sd)
})
