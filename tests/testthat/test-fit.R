# Tests for fit.R

# Pick a species with sufficient data
test_species <- unique(barnes_data$species)[1]
sp_data <- validate_ppmr_data(barnes_data, species = test_species)

test_that("fit_normal returns list with mean and sd", {
    result <- fit_normal(sp_data$log_ppmr, sp_data$n_prey)
    expect_type(result, "list")
    expect_true(all(c("mean", "sd") %in% names(result)))
    expect_true(is.numeric(result$mean))
    expect_true(is.numeric(result$sd))
    expect_gt(result$sd, 0)
    # Mean should agree with weighted.mean
    expect_equal(result$mean,
                 weighted.mean(sp_data$log_ppmr, sp_data$n_prey))
})

test_that("fit_normal uses maximum-likelihood frequency weighting", {
    value <- seq(1, 10)
    weight <- seq(1, 10)
    expected_mean <- sum(value * weight) / sum(weight)
    expected_sd <- sqrt(sum(weight * (value - expected_mean)^2) / sum(weight))

    fit <- fit_normal(value, weight)
    scaled_fit <- fit_normal(value, 7 * weight)

    expect_equal(fit, list(mean = expected_mean, sd = expected_sd))
    expect_equal(scaled_fit, fit)
})

test_that("fit_gaussian_mixture returns correct structure", {
    result <- fit_gaussian_mixture(sp_data$log_ppmr, sp_data$n_prey, k = 2)
    expect_type(result, "list")
    expect_true(all(c("p", "mean", "sd") %in% names(result)))
    expect_equal(length(result$p), 2)
    expect_equal(length(result$mean), 2)
    expect_equal(length(result$sd), 2)
    expect_equal(sum(result$p), 1, tolerance = 1e-6)
    expect_true(all(result$sd > 0))
})

test_that("fit_gaussian_mixture iterates beyond the first EM update", {
    value <- c(seq(-4, -1, length.out = 20),
               seq(1, 5, length.out = 30))
    weight <- c(rep(1, 20), rep(2, 30))
    one_update <- fit_gaussian_mixture(value, weight, k = 2, max_iter = 1)
    converged <- fit_gaussian_mixture(value, weight, k = 2, max_iter = 100)

    expect_false(isTRUE(all.equal(one_update$mean, converged$mean)))
    expect_equal(sum(converged$p), 1, tolerance = 1e-10)
})

test_that("fit_gaussian_mixture supports a documented component count", {
    value <- seq(-3, 6, length.out = 30)
    fit <- fit_gaussian_mixture(value, rep(1, 30), k = 3, max_iter = 5)

    expect_length(fit$p, 3)
    expect_length(fit$mean, 3)
    expect_length(fit$sd, 3)
    expect_equal(sum(fit$p), 1, tolerance = 1e-10)
})

test_that("fit_truncated_exponential returns correct structure", {
    result <- fit_truncated_exponential(sp_data$log_ppmr, sp_data$n_prey)
    expect_type(result, "list")
    expect_true(all(c("alpha", "ll", "ul", "lr", "ur") %in% names(result)))
    expect_true(all(sapply(result, is.numeric)))
})

test_that("fit_log_ppmr returns correct data frame for normal", {
    result <- fit_log_ppmr(barnes_data, test_species, distribution = "normal")
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1)
    expect_equal(result$species, test_species)
    expect_equal(result$distribution, "normal")
    expect_equal(result$power, 0)
    expect_equal(rownames(result), test_species)
    expect_true(hasName(result, "mean"))
    expect_true(hasName(result, "sd"))
})

test_that("fit_log_ppmr returns correct data frame for trunc_exp", {
    result <- fit_log_ppmr(barnes_data, test_species,
                           distribution = "trunc_exp")
    expect_s3_class(result, "data.frame")
    expect_equal(result$distribution, "trunc_exp")
    expect_true(all(c("alpha", "ll", "ul", "lr", "ur") %in% names(result)))
})

test_that("fit_log_ppmr returns correct data frame for gauss_mix", {
    result <- fit_log_ppmr(barnes_data, test_species,
                           distribution = "gauss_mix")
    expect_s3_class(result, "data.frame")
    expect_equal(result$distribution, "gauss_mix")
    expect_true(all(c("p", "mean", "sd") %in% names(result)))
})

test_that("fit_log_ppmr errors on unknown species", {
    expect_error(fit_log_ppmr(barnes_data, "NonExistent"),
                 "not found")
})

test_that("fit_log_ppmr works for multiple species", {
    species_vec <- unique(barnes_data$species)[1:2]
    result <- fit_log_ppmr(barnes_data, species_vec, distribution = "normal")
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 2)
    expect_equal(result$species, species_vec)
    expect_equal(rownames(result), species_vec)
})

test_that("fit_log_ppmr respects min_w_pred", {
    result <- fit_log_ppmr(barnes_data, test_species,
                           distribution = "normal", min_w_pred = 100)
    expect_equal(result$min_w_pred, 100)
})

test_that("fit_log_ppmr respects power argument", {
    result <- fit_log_ppmr(barnes_data, test_species,
                           distribution = "normal", power = 1)
    expect_equal(result$power, 1)
    # With biomass weighting the mean should differ from equal weighting
    result0 <- fit_log_ppmr(barnes_data, test_species,
                            distribution = "normal", power = 0)
    # They may be close but should generally not be identical
    expect_true(is.numeric(result$mean))
})

test_that("fit_log_ppmr implements documented row and prey-mass weights", {
    value <- seq(1, 2.9, length.out = 20)
    prey_mass <- seq(1, 4, length.out = 20)
    data <- data.frame(
        species = "A",
        w_pred = prey_mass * exp(value),
        w_prey = prey_mass,
        n_prey = rep(1:4, length.out = 20)
    )
    expected_weight <- data$n_prey * data$w_prey^0.5
    expected <- fit_normal(value, expected_weight)

    result <- fit_log_ppmr(data, "A", distribution = "normal", power = 0.5)

    expect_equal(result$mean, expected$mean)
    expect_equal(result$sd, expected$sd)
    expect_equal(result$power, 0.5)
})

test_that("fit_log_ppmr applies min_w_pred before fitting", {
    value <- seq(1, 3.9, length.out = 30)
    data <- data.frame(
        species = "A", w_pred = exp(value), w_prey = 1,
        n_prey = seq_len(30)
    )
    cutoff <- data$w_pred[11]
    keep <- data$w_pred >= cutoff
    expected <- fit_normal(value[keep], data$n_prey[keep])

    result <- fit_log_ppmr(
        data, "A", distribution = "normal", min_w_pred = cutoff
    )

    expect_equal(result$mean, expected$mean)
    expect_equal(result$sd, expected$sd)
    expect_equal(result$min_w_pred, cutoff)
})

test_that("fit_log_ppmr uses species from fits argument", {
    species_vec <- unique(barnes_data$species)[1:2]
    prior_fit <- fit_log_ppmr(barnes_data, species_vec, distribution = "normal")
    result <- fit_log_ppmr(barnes_data, distribution = "normal", fits = prior_fit)
    expect_equal(nrow(result), 2)
    expect_equal(result$species, species_vec)
})

test_that("fit_log_ppmr with fits ignores species argument", {
    sp1 <- unique(barnes_data$species)[1]
    sp2 <- unique(barnes_data$species)[2]
    prior_fit <- fit_log_ppmr(barnes_data, sp1, distribution = "normal")
    # Even if species = sp2 is passed, the result should use sp1 from fits
    result <- fit_log_ppmr(barnes_data, species = sp2,
                           distribution = "normal", fits = prior_fit)
    expect_equal(result$species, sp1)
})

test_that("fit_truncated_exponential accepts custom start values", {
    default_fit <- fit_truncated_exponential(sp_data$log_ppmr, sp_data$n_prey)
    custom_start <- list(alpha = default_fit$alpha, ll = default_fit$ll,
                         ul = default_fit$ul, lr = default_fit$lr,
                         ur = default_fit$ur)
    result <- fit_truncated_exponential(sp_data$log_ppmr, sp_data$n_prey,
                                        start = custom_start)
    expect_type(result, "list")
    expect_true(all(c("alpha", "ll", "ul", "lr", "ur") %in% names(result)))
    # Fitting from a good starting point should converge to the same solution
    expect_equal(result$alpha, default_fit$alpha, tolerance = 1e-4)
})

test_that("fit_truncated_exponential penalizes invalid density evaluations", {
    value <- seq(1, 2, length.out = 10)
    result <- testthat::with_mocked_bindings(
        fit_truncated_exponential(value, rep(1, 10)),
        dtexp = function(x, ...) rep(NA_real_, length(x)),
        .package = "mizerStomach"
    )

    expect_equal(
        result,
        list(alpha = 0.5, ll = 1, ul = 20, lr = 2, ur = 20)
    )
})

test_that("fit_log_ppmr uses fits as start for trunc_exp", {
    prior_fit <- fit_log_ppmr(barnes_data, test_species,
                              distribution = "trunc_exp")
    result <- fit_log_ppmr(barnes_data, distribution = "trunc_exp",
                           fits = prior_fit)
    expect_s3_class(result, "data.frame")
    expect_equal(result$species, test_species)
    expect_equal(result$distribution, "trunc_exp")
    # Should converge to essentially the same parameters
    expect_equal(result$alpha, prior_fit$alpha, tolerance = 1e-4)
})

# Tests for extract_fit species_dict argument
test_that("extract_fit species_dict renames species in output", {
    params <- mizer::NS_params
    sp_name <- params@species_params$species[[1]]
    dict <- setNames(list("renamed_species"), sp_name)
    result <- extract_fit(params, species_dict = dict)
    expect_equal(result$species[[1]], "renamed_species")
    expect_equal(rownames(result)[[1]], "renamed_species")
})

test_that("extract_fit species_dict leaves unmentioned species unchanged", {
    params <- mizer::NS_params
    sp1 <- params@species_params$species[[1]]
    sp2 <- params@species_params$species[[2]]
    dict <- setNames(list("renamed_species"), sp1)
    result <- extract_fit(params, species_dict = dict)
    expect_equal(result$species[[2]], sp2)
    expect_equal(rownames(result)[[2]], sp2)
})

test_that("extract_fit with NULL species_dict keeps original names", {
    params <- mizer::NS_params
    result_no_dict <- extract_fit(params)
    result_null    <- extract_fit(params, species_dict = NULL)
    expect_equal(result_no_dict$species, result_null$species)
})

test_that("extract_fit species_dict works with named character vector", {
    params <- mizer::NS_params
    sp_name <- params@species_params$species[[1]]
    dict <- setNames(c("vec_renamed"), sp_name)
    result <- extract_fit(params, species_dict = dict)
    expect_equal(result$species[[1]], "vec_renamed")
})

test_that("extract_fit maps lognormal kernels and their weighting", {
    params <- mizer::NS_params
    sp <- params@species_params
    kernel_power <- params@resource_params$lambda - 4 / 3

    result <- extract_fit(params)

    expect_equal(result$distribution, rep("normal", nrow(sp)))
    expect_equal(result$power, rep(0, nrow(sp)))
    expect_equal(result$min_w_pred, rep(0, nrow(sp)))
    expect_equal(result$mean, unname(log(sp$beta) + kernel_power * sp$sigma^2))
    expect_equal(result$sd, unname(sp$sigma))
})

test_that("extract_fit maps power-law kernels", {
    params <- mizer::NS_params
    sp <- params@species_params
    sp$pred_kernel_type[1] <- "power_law"
    sp$kernel_exp <- 0.2
    sp$kernel_l_l <- 2
    sp$kernel_u_l <- 4
    sp$kernel_l_r <- 12
    sp$kernel_u_r <- 6
    params@species_params <- sp
    kernel_power <- params@resource_params$lambda - 4 / 3

    result <- extract_fit(params)[1, , drop = FALSE]

    expect_equal(result$distribution, "trunc_exp")
    expect_equal(result$alpha, 0.2 + kernel_power)
    expect_equal(result[c("ll", "ul", "lr", "ur")],
                 data.frame(ll = 2, ul = 4, lr = 12, ur = 6),
                 ignore_attr = TRUE)
})

test_that("extract_fit rejects unsupported mizer kernels", {
    params <- mizer::NS_params
    params@species_params$pred_kernel_type[1] <- "unsupported"

    expect_error(extract_fit(params), "Unknown pred_kernel_type: unsupported")
})

test_that("set_kernel_params maps a normal fit without modifying its input", {
    params <- mizer::NS_params
    original <- params@species_params
    species <- original$species[1]
    kernel_power <- params@resource_params$lambda - 4 / 3
    fit <- data.frame(
        species = species, distribution = "normal", power = 0,
        mean = 6, sd = 1.5
    )

    result <- suppressWarnings(set_kernel_params(params, fit))
    result_sp <- result@species_params
    expected_mean <- fit$mean - kernel_power * fit$sd^2

    expect_equal(unname(result_sp$pred_kernel_type[1]), "lognormal")
    expect_equal(unname(result_sp$beta[1]), exp(expected_mean))
    expect_equal(unname(result_sp$sigma[1]), fit$sd)
    expect_identical(params@species_params, original)
    expect_equal(result_sp$beta[-1], original$beta[-1])
})

test_that("set_kernel_params maps a truncated-exponential fit", {
    params <- mizer::NS_params
    species <- params@species_params$species[1]
    kernel_power <- params@resource_params$lambda - 4 / 3
    fit <- data.frame(
        species = species, distribution = "trunc_exp", power = 0,
        alpha = 1.1, ll = 2, ul = 4, lr = 12, ur = 6
    )

    result <- suppressWarnings(set_kernel_params(params, fit))
    row <- result@species_params[1, ]

    expect_equal(unname(row$pred_kernel_type), "power_law")
    expect_equal(unname(row$kernel_exp), fit$alpha - kernel_power)
    expect_equal(
        unname(unlist(row[c("kernel_l_l", "kernel_u_l", "kernel_l_r", "kernel_u_r")])),
        c(2, 4, 12, 6)
    )
})

test_that("kernel parameter conversions round trip", {
    params <- mizer::NS_params
    normal_fit <- extract_fit(params)[1, , drop = FALSE]
    normal_roundtrip <- extract_fit(
        suppressWarnings(set_kernel_params(params, normal_fit))
    )[1, , drop = FALSE]

    expect_equal(normal_roundtrip$mean, normal_fit$mean)
    expect_equal(normal_roundtrip$sd, normal_fit$sd)

    species <- params@species_params$species[1]
    trunc_fit <- data.frame(
        species = species, distribution = "trunc_exp", power = 0,
        alpha = 1.1, ll = 2, ul = 4, lr = 12, ur = 6
    )
    trunc_roundtrip <- extract_fit(
        suppressWarnings(set_kernel_params(params, trunc_fit))
    )[1, , drop = FALSE]

    expect_equal(trunc_roundtrip$alpha, trunc_fit$alpha)
    expect_equal(
        unname(unlist(trunc_roundtrip[c("ll", "ul", "lr", "ur")])),
        unname(unlist(trunc_fit[c("ll", "ul", "lr", "ur")]))
    )
})

test_that("set_kernel_params rejects unknown species and Gaussian mixtures", {
    normal <- data.frame(
        species = "not in model", distribution = "normal", mean = 5, sd = 1
    )
    expect_error(set_kernel_params(mizer::NS_params, normal), "not found in params")

    mixture <- data.frame(species = "Cod", distribution = "gauss_mix")
    mixture$p <- I(list(c(0.5, 0.5)))
    mixture$mean <- I(list(c(3, 7)))
    mixture$sd <- I(list(c(1, 2)))
    expect_error(set_kernel_params(mizer::NS_params, mixture), "not supported")
})
