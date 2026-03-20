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
