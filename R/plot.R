#' Plot log ppmr observations and fitted distribution
#'
#' Plots the fitted distribution on top of the log ppmr observations, both
#' for the number distribution and the biomass distribution.
#'
#' @param ppmr_data A data frame with log ppmr observations
#' @param fit A list with the fitted distribution parameters
#' @export
plot_log_ppmr_fit <- function(ppmr_data, fit) {
  validate_ppmr_data(ppmr_data)
  validate_fit(fit)
  if (!fit$species %in% unique(ppmr_data$species)) {
    stop("Species", fit$species, "not found in ppmr data")
  }
  # TODO: Implement this function
}


plot_ppmr_violins <- function(ppmr_data, power = 1) {
  validate_ppmr_data(ppmr_data)
  # TODO: Implement this function
}
