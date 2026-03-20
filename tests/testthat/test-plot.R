# Tests for plot.R

test_species <- unique(barnes_data$species)[1]

test_that("plot_log_ppmr_fit returns a ggplot for normal fit (kernel)", {
    fit <- fit_log_ppmr(barnes_data, test_species, distribution = "normal")
    p <- plot_log_ppmr_fit(barnes_data, fit, type = "kernel")
    expect_s3_class(p, "ggplot")
})

test_that("plot_log_ppmr_fit returns a ggplot for normal fit (histogram)", {
    fit <- fit_log_ppmr(barnes_data, test_species, distribution = "normal")
    p <- plot_log_ppmr_fit(barnes_data, fit, type = "histogram")
    expect_s3_class(p, "ggplot")
})

test_that("plot_log_ppmr_fit returns a ggplot for trunc_exp fit", {
    fit <- fit_log_ppmr(barnes_data, test_species, distribution = "trunc_exp")
    p <- plot_log_ppmr_fit(barnes_data, fit)
    expect_s3_class(p, "ggplot")
})

test_that("plot_log_ppmr_fit returns a ggplot for gauss_mix fit", {
    fit <- fit_log_ppmr(barnes_data, test_species, distribution = "gauss_mix")
    p <- plot_log_ppmr_fit(barnes_data, fit)
    expect_s3_class(p, "ggplot")
})

test_that("plot_log_ppmr_fit works for multi-species fit", {
    species_vec <- unique(barnes_data$species)[1:2]
    fit <- fit_log_ppmr(barnes_data, species_vec, distribution = "normal")
    p <- plot_log_ppmr_fit(barnes_data, fit)
    expect_s3_class(p, "ggplot")
})

test_that("plot_ppmr_violins returns a ggplot", {
    p <- plot_ppmr_violins(barnes_data, test_species)
    expect_s3_class(p, "ggplot")
})

test_that("plot_ppmr_violins works with power = 0", {
    p <- plot_ppmr_violins(barnes_data, test_species, power = 0)
    expect_s3_class(p, "ggplot")
})
