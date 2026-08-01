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
    expect_warning(
        p <- plot_ppmr_violins(barnes_data, test_species),
        NA
    )
    expect_s3_class(p, "ggplot")
})

test_that("plot_ppmr_violins works with power = 0", {
    p <- plot_ppmr_violins(barnes_data, test_species, power = 0)
    expect_s3_class(p, "ggplot")
})

test_that("plot_log_ppmr_fit reports every documented weighting label", {
    fit <- fit_log_ppmr(barnes_data, test_species, distribution = "normal")

    number_plot <- plot_log_ppmr_fit(barnes_data, fit)
    biomass_plot <- plot_log_ppmr_fit(barnes_data, transform_fit(fit, 1))
    other_plot <- plot_log_ppmr_fit(barnes_data, transform_fit(fit, 0.5))

    expect_match(number_plot$labels$subtitle, "number density")
    expect_match(biomass_plot$labels$subtitle, "biomass density")
    expect_match(other_plot$labels$subtitle, "density with power 0.5", fixed = TRUE)
})

test_that("plot_log_ppmr_fit applies the fit predator-mass threshold", {
    fit <- fit_log_ppmr(
        barnes_data, test_species, distribution = "normal", min_w_pred = 100
    )
    p <- plot_log_ppmr_fit(barnes_data, fit)

    expect_true(all(p$data$w_pred >= 100))
})

test_that("plot_log_ppmr_fit normalizes histogram densities", {
    fit <- fit_log_ppmr(barnes_data, test_species, distribution = "normal")
    p <- plot_log_ppmr_fit(barnes_data, fit, type = "histogram")
    binsize <- min(diff(sort(unique(p$data$log_ppmr))))
    area <- tapply(p$data$Density, p$data$Type, sum) * binsize

    expect_equal(as.numeric(area), c(1, 1), tolerance = 1e-10)
})

test_that("multi-species plots use faceted kernel densities for either type", {
    species <- unique(barnes_data$species)[1:2]
    fit <- fit_log_ppmr(barnes_data, species, distribution = "normal")

    p <- plot_log_ppmr_fit(barnes_data, fit, type = "histogram")

    expect_s3_class(p$facet, "FacetWrap")
    expect_equal(length(p$layers), 3)
    expect_s3_class(p$layers[[1]]$geom, "GeomDensity")
    expect_setequal(unique(p$data$species), species)
})

test_that("plot_ppmr_violins implements its bins, weights, and reproducibility", {
    set.seed(123)
    p1 <- plot_ppmr_violins(barnes_data, test_species, power = 0.5)
    set.seed(123)
    p2 <- plot_ppmr_violins(barnes_data, test_species, power = 0.5)

    expect_equal(nlevels(p1$data$bin), 10)
    expect_equal(p1$data$weight, p1$data$n_prey * p1$data$w_prey^0.5)
    expect_identical(p1$data$bin, p2$data$bin)
    expect_match(p1$labels$title, "with power  0.5", fixed = TRUE)
})
