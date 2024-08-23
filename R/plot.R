#' Plot log ppmr observations and fitted distribution
#'
#' Plots the fitted distribution on top of the log ppmr observations, both
#' for the number distribution and the biomass distribution.
#'
#' @param ppmr_data A data frame with log ppmr observations
#' @param fit A list with the fitted distribution parameters
#' @export
plot_log_ppmr_fit <- function(ppmr_data, fit) {
    fit <- validate_fit(fit)
    ppmr_data <- validate_ppmr_data(ppmr_data, species = fit$species) |>
        filter(species == !!fit$species,
               w_pred >= fit$min_w_pred)
    n_prey <- sum(ppmr_data$n_prey)
    ppmr_data$biomass <- ppmr_data$n_prey * ppmr_data$w_prey
    lmin <- max(0, Hmisc::wtd.quantile(ppmr_data$log_ppmr, ppmr_data$biomass, 0.001) - 2)
    lmax <- Hmisc::wtd.quantile(ppmr_data$log_ppmr, ppmr_data$n_prey, 0.999) + 2
    grid = seq(lmin, lmax, length.out = 200)
    fit0 <- transform_fit(fit, 0)
    dist0 <- data.frame(log_ppmr = grid, Density = get_density(grid, fit0))
    fit1 <- transform_fit(fit, 1)
    dist1 <- data.frame(log_ppmr = grid, Density = get_density(grid, fit1))

    plot_subtitle <- paste("Fit of", fit$distribution, "to")
    if (fit$power == 0) {
        plot_subtitle <- paste(plot_subtitle, "number density")
    } else if (fit$power == 1) {
        plot_subtitle <- paste(plot_subtitle, "biomass density")
    } else {
        plot_subtitle <- paste(plot_subtitle, "density with power", fit$power)
    }
    plot_subtitle <- paste(plot_subtitle, "with", n_prey, "prey.")

    ggplot(ppmr_data) +
        geom_density(aes(log_ppmr, weight = n_prey, fill = "Number"),
                     alpha = 0.7) +
        geom_density(aes(log_ppmr, weight = biomass, fill = "Biomass"),
                     alpha = 0.5) +
        geom_line(aes(log_ppmr, Density, color = "Number"), data = dist0) +
        geom_line(aes(log_ppmr, Density, color = "Biomass"), data = dist1) +
        xlab("Log of predator/prey mass ratio") +
        xlim(lmin, lmax) +
        ggtitle(fit$species, subtitle = plot_subtitle) +
        scale_fill_manual(name = "Observed density",
                          values = c("Number" = "lightblue",
                                     "Biomass" = "#ffcccb")) +
        scale_color_manual(name = paste("Fitted", fit$distribution),
                           values = c("Number" = "blue", "Biomass" = "red"))

}

#' Violin plots of predator/prey mass ratios for different predator weights
#'
#' @param ppmr_data A data frame with log ppmr observations
#' @param species The species to select
#' @param power The power to raise the weights to
#' @export
plot_ppmr_violins <- function(ppmr_data, species, power = 1) {
    ppmr_data <- ppmr_data |>
        validate_ppmr_data(species = species) |>
        filter(species == !!species)
    n_prey <- sum(ppmr_data$n_prey)
    # bin data
    # We need to wiggle the data a bit to avoid
    # duplicates which `cut_number()` does not like
    stomach_binned <- ppmr_data |>
        mutate(bin = cut_number(w_pred * rnorm(length(w_pred), 1, 0.001), 10))

    if (power == 1) {
        plot_title <- paste("Biomass distribution for", species)
    } else if (power == 0) {
        plot_title <- paste("Number distribution for ", species)
    } else {
        plot_title <- paste("Distribution for ", species, " with power ", power)
    }

    stomach_binned$weight <- stomach_binned$n_prey * stomach_binned$w_prey ^ power

    ggplot(stomach_binned, aes(bin, log_ppmr)) +
        geom_violin(aes(weight = weight),
                    draw_quantiles = 0.5) +
        xlab("Predator weight [g]") +
        ylab("Log of predator/prey mass ratio") +
        ggtitle(plot_title, subtitle = paste("Total number of prey:", n_prey))

}
