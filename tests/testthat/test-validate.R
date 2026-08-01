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

test_that("validate_ppmr_data preserves a correct existing log_ppmr column", {
    d <- data.frame(
        species = rep("A", 10), w_pred = 10:19, w_prey = rep(1, 10),
        n_prey = rep(1, 10), note = letters[1:10]
    )
    d$log_ppmr <- log(d$w_pred / d$w_prey)

    expect_no_warning(result <- validate_ppmr_data(d))
    expect_identical(result$log_ppmr, d$log_ppmr)
    expect_identical(result$note, d$note)
})

test_that("validate_ppmr_data warns and retains a conflicting log_ppmr", {
    d <- data.frame(
        species = rep("A", 10), w_pred = 10:19, w_prey = rep(1, 10),
        n_prey = rep(1, 10), log_ppmr = rep(99, 10)
    )

    expect_warning(result <- validate_ppmr_data(d), "does not agree")
    expect_identical(result$log_ppmr, d$log_ppmr)
})

test_that("validate_ppmr_data rejects missing masses", {
    d <- data.frame(
        species = rep("A", 10), w_pred = c(NA, 11:19),
        w_prey = rep(1, 10), n_prey = rep(1, 10)
    )

    expect_error(validate_ppmr_data(d), "weights are missing")
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

    fit <- data.frame(species = "A", distribution = "normal", sd = 1,
                      stringsAsFactors = FALSE)
    expect_error(validate_fit(fit), "mean")
})

test_that("validate_fit converts named list to data frame", {
    fit_list <- list(species = "A", distribution = "normal",
                     mean = 5, sd = 1)
    result <- validate_fit(fit_list)
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1)
    expect_equal(result$species, "A")

    mixture <- list(
        species = "C", distribution = "gauss_mix",
        mean = c(3, 7), sd = c(1, 2), p = c(0.4, 0.6)
    )
    mixture_result <- validate_fit(mixture)
    expect_equal(mixture_result$mean[[1]], c(3, 7))
    expect_equal(mixture_result$sd[[1]], c(1, 2))
    expect_equal(mixture_result$p[[1]], c(0.4, 0.6))
})

test_that("validate_fit errors on non-data-frame non-list input", {
    expect_error(validate_fit("not a fit"), "must be a data frame")
})

test_that("validate_fit accepts mixed families and preserves supplied metadata", {
    fit <- data.frame(
        species = c("A", "B"),
        distribution = c("normal", "trunc_exp"),
        power = c(0.5, 1), min_w_pred = c(10, 20),
        mean = c(5, NA), sd = c(1, NA),
        alpha = c(NA, 0.2), ll = c(NA, 2), ul = c(NA, 4),
        lr = c(NA, 12), ur = c(NA, 6)
    )

    result <- validate_fit(fit)
    expect_equal(result$power, c(0.5, 1))
    expect_equal(result$min_w_pred, c(10, 20))
    expect_equal(rownames(result), c("A", "B"))
})

test_that("validate_fit checks every family-specific parameter set", {
    expect_error(
        validate_fit(data.frame(
            species = "B", distribution = "trunc_exp",
            alpha = 0.2, ll = 2, ul = 4, lr = 12
        )),
        "ur"
    )

    mixture <- data.frame(species = "C", distribution = "gauss_mix")
    mixture$mean <- I(list(c(3, 7)))
    mixture$sd <- I(list(c(1, 2)))
    expect_error(validate_fit(mixture), "p")

    mixture$p <- I(list(1))
    expect_error(validate_fit(mixture), "same length")
})

test_that("validate_fit does not modify its input", {
    fit <- data.frame(species = "A", distribution = "normal", mean = 5, sd = 1)
    original <- fit

    result <- validate_fit(fit)

    expect_identical(fit, original)
    expect_true(all(c("power", "min_w_pred") %in% names(result)))
})

test_that("weighted observation validation covers its documented contract", {
    validate <- mizerStomach:::validate_weighted_observations

    expect_true(validate(1:10, rep(1, 10)))
    expect_error(validate(1:9, rep(1, 9)), "Not enough data")
    expect_error(validate(1:10, rep(1, 9)), "same")
    expect_error(validate(as.character(1:10), rep(1, 10)), "value must be numeric")
    expect_error(validate(1:10, as.character(rep(1, 10))), "weight must be numeric")
    expect_error(validate(1:10, c(-1, rep(1, 9))), "non-negative")
})
