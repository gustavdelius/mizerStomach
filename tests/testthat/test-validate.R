# Tests for validate.R

test_that("validate_ppmr_data works on barnes_data", {
    result <- validate_ppmr_data(barnes_data)
    expect_s3_class(result, "data.frame")
    expect_true(all(c("species", "w_pred", "w_prey", "n_prey", "log_ppmr") %in%
                        names(result)))
    expect_equal(result$log_ppmr, log(result$w_pred) - log(result$w_prey))
})

test_that("validate_ppmr_data filters by species", {
    species <- unique(barnes_data$species)[1]
    result <- validate_ppmr_data(barnes_data, species = species)
    expect_true(all(result$species == species))
    expect_gt(nrow(result), 0)
})

test_that("validate_ppmr_data errors on non-existent species", {
    expect_error(validate_ppmr_data(barnes_data, species = "NonExistent"),
                 "not found")
})

test_that("validate_ppmr_data errors on non-data-frame input", {
    expect_error(validate_ppmr_data("not a data frame"),
                 "must be a data frame")
})

test_that("validate_ppmr_data errors when columns are missing", {
    bad_data <- data.frame(species = "A", w_pred = 1)
    expect_error(validate_ppmr_data(bad_data), "must have the columns")
})

test_that("validate_ppmr_data warns when prey heavier than predator", {
    d <- data.frame(
        species = rep("test_sp", 20),
        w_pred = c(1, rep(100, 19)),
        w_prey = c(10, rep(1, 19)),
        n_prey = rep(1, 20)
    )
    expect_warning(validate_ppmr_data(d), "prey are heavier")
})

test_that("validate_fit works with normal distribution data frame", {
    fit <- data.frame(species = "A", distribution = "normal",
                      mean = 5, sd = 1, stringsAsFactors = FALSE)
    result <- validate_fit(fit)
    expect_s3_class(result, "data.frame")
    expect_equal(result$power, 0)
    expect_equal(result$min_w_pred, 0)
    expect_equal(rownames(result), "A")
})

test_that("validate_fit works with trunc_exp distribution data frame", {
    fit <- data.frame(species = "B", distribution = "trunc_exp",
                      alpha = 0.5, ll = 1, ul = 20, lr = 15, ur = 20,
                      stringsAsFactors = FALSE)
    result <- validate_fit(fit)
    expect_s3_class(result, "data.frame")
    expect_equal(rownames(result), "B")
})

test_that("validate_fit works with gauss_mix distribution data frame", {
    fit <- data.frame(species = "C", distribution = "gauss_mix",
                      stringsAsFactors = FALSE)
    fit$mean <- I(list(c(3, 7)))
    fit$sd <- I(list(c(1, 1)))
    fit$p <- I(list(c(0.5, 0.5)))
    result <- validate_fit(fit)
    expect_s3_class(result, "data.frame")
})

test_that("validate_fit errors on missing species column", {
    fit <- data.frame(distribution = "normal", mean = 5, sd = 1)
    expect_error(validate_fit(fit), "species")
})

test_that("validate_fit errors on missing distribution column", {
    fit <- data.frame(species = "A", mean = 5, sd = 1)
    expect_error(validate_fit(fit), "distribution")
})

test_that("validate_fit errors on unknown distribution", {
    fit <- data.frame(species = "A", distribution = "unknown",
                      stringsAsFactors = FALSE)
    expect_error(validate_fit(fit), "Unknown distribution")
})

test_that("validate_fit errors on missing normal parameters", {
    fit <- data.frame(species = "A", distribution = "normal", mean = 5,
                      stringsAsFactors = FALSE)
    expect_error(validate_fit(fit), "sd")
})

test_that("validate_fit converts named list to data frame", {
    fit_list <- list(species = "A", distribution = "normal",
                     mean = 5, sd = 1)
    result <- validate_fit(fit_list)
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1)
    expect_equal(result$species, "A")
})

test_that("validate_fit errors on non-data-frame non-list input", {
    expect_error(validate_fit("not a fit"), "must be a data frame")
})
