#' Compare observed and fitted log-PPMR distributions
#'
#' Overlays a fitted distribution on empirical prey-number and prey-biomass
#' distributions for one or more predator species.
#'
#' @param ppmr_data A data frame accepted by [validate_ppmr_data()].
#' @param fit A one- or multi-row fit data frame accepted by [validate_fit()].
#'   Only species in this object are plotted. Each row's `min_w_pred` determines
#'   which observations are included.
#' @param type For a single fit row, either `"kernel"` (the default) for kernel
#'   density estimates or `"histogram"` for normalized histogram bars. A
#'   multi-species plot currently always uses faceted kernel densities.
#'
#' @details
#' The observed number distribution weights rows by `n_prey`; the observed
#' biomass distribution weights them by `n_prey * w_prey`. For every fit row,
#' [transform_fit()] is used to obtain fitted curves at `power = 0` (number) and
#' `power = 1` (biomass), whatever weighting was originally used for fitting.
#'
#' For `type = "kernel"`, [ggplot2::geom_density()] chooses the smoothing
#' bandwidth. For a one-species histogram, observations are placed in 30
#' equally spaced log-PPMR bins and each set of bars is divided by its total
#' weight and bin width, so its area is one. The curve grid for kernel plots is
#' based on the 0.1% biomass-weighted and 99.9% number-weighted quantiles, with
#' padding. Multi-species results use one facet per predator species with free
#' scales.
#'
#' These plots are diagnostics rather than goodness-of-fit tests. In
#' particular, the empirical kernel density depends on its automatically chosen
#' bandwidth.
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @seealso [fit_log_ppmr()] to estimate the fit and [get_density()] to obtain
#'   the curve values directly.
#' @examples
#' # Plot a normal fit for one species
#' fit <- fit_log_ppmr(barnes_data, "Albacore", distribution = "normal")
#' plot_log_ppmr_fit(barnes_data, fit)
#'
#' # Plot with histogram style
#' plot_log_ppmr_fit(barnes_data, fit, type = "histogram")
#'
#' # Plot fits for multiple species
#' fit2 <- fit_log_ppmr(barnes_data,
#'                      c("Albacore", "Atlantic cod"),
#'                      distribution = "normal")
#' plot_log_ppmr_fit(barnes_data, fit2)
#' @export
plot_log_ppmr_fit <- function(ppmr_data, fit, type = c("kernel", "histogram")) {
    type <- match.arg(type)
    fit <- validate_fit(fit)

    # Build per-species data for overlay curves and filtered observations
    all_ppmr <- list()
    all_dist <- list()
    for (i in seq_len(nrow(fit))) {
        sp <- fit$species[i]
        fit_row <- as.list(fit[i, ])
        sp_data <- validate_ppmr_data(ppmr_data, species = sp) |>
            filter(species == !!sp,
                   w_pred >= fit$min_w_pred[i])
        sp_data$biomass <- sp_data$n_prey * sp_data$w_prey
        all_ppmr[[i]] <- sp_data

        lmin <- max(0, Hmisc::wtd.quantile(sp_data$log_ppmr, sp_data$biomass, 0.001) - 2)
        lmax <- Hmisc::wtd.quantile(sp_data$log_ppmr, sp_data$n_prey, 0.999) + 2
        grid <- seq(lmin, lmax, length.out = 200)
        fit0 <- transform_fit(fit[i, , drop = FALSE], 0)
        fit1 <- transform_fit(fit[i, , drop = FALSE], 1)
        all_dist[[i]] <- rbind(
            data.frame(species = sp, log_ppmr = grid,
                       Density = get_density(grid, fit0), Type = "Number"),
            data.frame(species = sp, log_ppmr = grid,
                       Density = get_density(grid, fit1), Type = "Biomass")
        )
    }
    ppmr_combined <- do.call(rbind, all_ppmr)
    dist_combined <- do.call(rbind, all_dist)

    # For single species, use existing detailed plot style
    if (nrow(fit) == 1) {
        fit_row <- as.list(fit[1, ])
        n_prey <- sum(ppmr_combined$n_prey)
        plot_subtitle <- paste("Fit of", fit_row$distribution, "to")
        if (fit_row$power == 0) {
            plot_subtitle <- paste(plot_subtitle, "number density")
        } else if (fit_row$power == 1) {
            plot_subtitle <- paste(plot_subtitle, "biomass density")
        } else {
            plot_subtitle <- paste(plot_subtitle, "density with power", fit_row$power)
        }
        plot_subtitle <- paste(plot_subtitle, "with", n_prey, "prey.")

        fill_scale <- scale_fill_manual(
            name = "Observed density",
            values = c("Number" = "lightblue", "Biomass" = "#ffcccb")
        )
        colour_scale <- scale_color_manual(
            name = paste("Fitted", fit_row$distribution),
            values = c("Number" = "blue", "Biomass" = "red")
        )

        if (type == "kernel") {
            return(
                ggplot(ppmr_combined) +
                    geom_density(aes(log_ppmr, weight = n_prey, fill = "Number"),
                                 alpha = 0.7) +
                    geom_density(aes(log_ppmr, weight = biomass, fill = "Biomass"),
                                 alpha = 0.5) +
                    geom_line(aes(log_ppmr, Density, color = Type), data = dist_combined) +
                    xlab("Log of predator/prey mass ratio") +
                    xlim(min(dist_combined$log_ppmr), max(dist_combined$log_ppmr)) +
                    ggtitle(fit_row$species, subtitle = plot_subtitle) +
                    fill_scale + colour_scale +
                    theme(text = element_text(size = 14))
            )
        } else {
            no_bins <- 30
            hist_min <- min(ppmr_combined$log_ppmr)
            hist_max <- max(ppmr_combined$log_ppmr)
            binsize <- (hist_max - hist_min) / (no_bins - 1)
            breaks <- seq(hist_min - binsize / 2, by = binsize, length.out = no_bins + 1)
            grid <- seq(hist_min, hist_max, length.out = 200)
            fit0 <- transform_fit(fit, 0)
            fit1 <- transform_fit(fit, 1)
            dist_combined <- rbind(
                data.frame(log_ppmr = grid, Density = get_density(grid, fit0), Type = "Number"),
                data.frame(log_ppmr = grid, Density = get_density(grid, fit1), Type = "Biomass")
            )
            binned <- ppmr_combined |>
                mutate(bin = cut(log_ppmr, breaks = breaks, right = FALSE,
                                 labels = FALSE)) |>
                filter(!is.na(bin)) |>
                group_by(bin) |>
                summarise(Number  = sum(n_prey),
                          Biomass = sum(biomass),
                          .groups = "drop") |>
                mutate(Number  = Number  / sum(Number)  / binsize,
                       Biomass = Biomass / sum(Biomass) / binsize,
                       log_ppmr = breaks[bin] + binsize / 2)
            binned_long <- rbind(
                data.frame(log_ppmr = binned$log_ppmr,
                           Density  = binned$Number,  Type = "Number"),
                data.frame(log_ppmr = binned$log_ppmr,
                           Density  = binned$Biomass, Type = "Biomass")
            )
            return(
                ggplot(binned_long) +
                    geom_col(aes(log_ppmr, Density, fill = Type)) +
                    geom_line(aes(log_ppmr, Density, colour = Type), data = dist_combined) +
                    facet_grid(~ Type, scales = "free_y") +
                    xlab("Log of predator/prey mass ratio") +
                    ggtitle(fit_row$species, subtitle = plot_subtitle) +
                    fill_scale + colour_scale +
                    theme(text = element_text(size = 14))
            )
        }
    }

    # Multi-species: faceted kernel density plot
    fill_scale <- scale_fill_manual(
        name = "Observed density",
        values = c("Number" = "lightblue", "Biomass" = "#ffcccb")
    )
    colour_scale <- scale_color_manual(
        name = "Fitted density",
        values = c("Number" = "blue", "Biomass" = "red")
    )
    ggplot(ppmr_combined) +
        geom_density(aes(log_ppmr, weight = n_prey, fill = "Number"),
                     alpha = 0.7) +
        geom_density(aes(log_ppmr, weight = biomass, fill = "Biomass"),
                     alpha = 0.5) +
        geom_line(aes(log_ppmr, Density, color = Type), data = dist_combined) +
        facet_wrap(~ species, scales = "free") +
        xlab("Log of predator/prey mass ratio") +
        fill_scale + colour_scale +
        theme(text = element_text(size = 14))
}


#' Plot log-PPMR distributions across predator-size classes
#'
#' Uses violin plots to inspect whether the log predator/prey mass-ratio
#' distribution changes with predator size.
#'
#' @param ppmr_data A data frame accepted by [validate_ppmr_data()].
#' @param species Single character string naming the predator species to plot.
#' @param power Numeric prey-mass weighting exponent. Each observation receives
#'   weight `n_prey * w_prey^power`; zero gives a number distribution and one a
#'   biomass distribution.
#'
#' @details
#' Observations are split into ten predator-mass classes with approximately
#' equal numbers of data-frame rows using [ggplot2::cut_number()]. Predator
#' masses are multiplied by tiny random perturbations before binning so that
#' repeated, rounded masses do not prevent the quantile cut. Consequently, bin
#' boundaries can vary slightly between calls; use [base::set.seed()] before
#' the function when exact reproducibility is important.
#'
#' Within each class, [ggplot2::geom_violin()] estimates the weighted log-PPMR
#' density and marks its median. The bins balance rows, not `n_prey` or the
#' derived weights, so strongly aggregated data can still have uneven effective
#' sample sizes across classes.
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @seealso [plot_log_ppmr_fit()] for fitted marginal distributions and
#'   [fit_log_ppmr()] for fitting after the predator-size assumption has been
#'   assessed.
#' @examples
#' # Biomass-weighted violin plot
#' plot_ppmr_violins(barnes_data, "Albacore")
#'
#' # Number-weighted violin plot
#' plot_ppmr_violins(barnes_data, "Albacore", power = 0)
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
                    quantiles = 0.5,
                    quantile.linetype = 1) +
        xlab("Predator weight [g]") +
        ylab("Log of predator/prey mass ratio") +
        ggtitle(plot_title, subtitle = paste("Total number of prey:", n_prey))

}
