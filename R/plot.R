#' Plot log ppmr observations and fitted distribution
#'
#' Plots the fitted distribution on top of the log ppmr observations, both
#' for the number distribution and the biomass distribution.
#'
#' @param ppmr_data A data frame with log ppmr observations
#' @param fit A list with the fitted distribution parameters
#' @export
plot_log_ppmr_fit <- function(ppmr_data, fit) {
  ppmr_data <- validate_ppmr_data(ppmr_data)
  fit <- validate_fit(fit)
  if (!fit$species %in% unique(ppmr_data$species)) {
    stop("Species", fit$species, "not found in ppmr data")
  }
  ppmr_data$biomass <- ppmr_data$n_prey * ppmr_data$w_prey
  lmin <- Hmisc::wtd.quantile(ppmr_data$log_ppmr, ppmr_data$biomass, 0.001)
  lmax <- Hmisc::wtd.quantile(ppmr_data$log_ppmr, ppmr_data$n_prey, 0.999)
  grid = seq(lmin, lmax, length.out = 200)
  fit0 <- transform_fit(fit, 0)
  dist0 <- data.frame(log_ppmr = grid, Density = get_density(grid, fit0))
  fit1 <- transform_fit(fit, 1)
  dist1 <- data.frame(log_ppmr = grid, Density = get_density(grid, fit1))

  ggplot(df) +
    geom_density(aes(log_ppmr, weight = n_prey), fill = "lightblue") +
    geom_density(aes(log_ppmr, weight = biomass), fill = "#ffcccb", alpha = 0.5) +
    xlab("Log of predator/prey mass ratio") +
    geom_line(aes(log_ppmr, Density), data = dist0, color = "blue") +
    geom_line(aes(log_ppmr, Density), data = dist1, color = "red") +
    xlim(lmin, lmax)
}


plot_ppmr_violins <- function(ppmr_data, power = 1) {
  validate_ppmr_data(ppmr_data)
  # TODO: Implement this function
}
