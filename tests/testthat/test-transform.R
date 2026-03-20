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
