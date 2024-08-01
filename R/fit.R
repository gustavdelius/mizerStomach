#' Fit a distribution to log ppmr observations
#'
#' @param ppmr_data A data frame with log ppmr observations, See
#'   [validate_ppmr_data()] for details
#' @param species A character string with the species name
#' @param distribution The distribution to fit. One of "normal",
#'   "truncated_exponential" or "gaussian_mixture".
#' @param power Each observation is weighted by a power of the prey weight. The
#'   default is 0 which means that each prey individual contributes equally.
#'   `power = 1` means each prey individual contributes in proportion to its
#'   biomass.
#' @return A `fit` object. See [validate_fit()] for details
#' @export
fit_log_ppmr <-
  function(ppmr_data, species,
           distribution = c("normal", "truncated_exponential", "gaussian_mixture"),
           power = 0) {
    distribution <- match.arg(distribution)
    validate_ppmr_data(ppmr_data)
    if (!species %in% unique(ppmr_data$species)) {
      stop("Species", fit$species, "not found in ppmr data")
    }
    data <- ppmr_data[ppmr_data$species == species, ]
    value <- data$log_ppmr
    weight <- data$n_prey * data$w_prey ^ power

    if (distribution == "normal") {
      fit <- fit_normal(value, weight)
    } else if (distribution == "truncated_exponential") {
      fit <- fit_truncated_exponential(value, weight)
    } else if (distribution == "gaussian_mixture") {
      fit <- fit_gaussian_mixture(value, weight)
    }
    fit$species <- species
    fit$distribution <- distribution
    fit$power <- power
    return(fit)
  }

#' Fit a normal distribution to weighted observations
#'
#' @param value A numeric vector of observed values
#' @param weight A numeric vector of weights
#' @return A list with the fitted parameters `mean` and `sd`
#' @export
#' @keywords internal
fit_normal <- function(value, weight) {
  validate_weighted_observations(value, weight)
  fit <- list(mean = weighted.mean(ppmr_data$value, w = ppmr_data$w_prey),
              sd = weighted.sd(ppmr_data$value, w = ppmr_data$w_prey))
  return(fit)
}

weighted.sd <- function(x, w) {
  sqrt(sum(w * (x - weighted.mean(x, w))^2))
}

#' Fit a truncated exponential distribution to weighted observations
#'
#' @param value A numeric vector of observed values
#' @param weight A numeric vector of weights
#' @return A list with the fitted parameters `exp`, `ll`, `ul`, `lr`, `ur`
#' @export
#' @keywords internal
fit_truncated_exponential <- function(value, weight) {
  validate_weighted_observations(value, weight)
  fit <- list() # TODO: Implement this function
  return(fit)
}

#' Fit a Gaussian mixture distribution to weighted observations
#'
#' @param value A numeric vector of observed values
#' @param weight A numeric vector of weights
#' @return A list with the fitted parameters `mean`, `sd` and `p`, each of which
#'   is a vector with one entry for each component of the mixture
#' @export
#' @keywords internal
fit_gaussian_mixture <- function(value, weight) {
  validate_weighted_observations(value, weight)
  fit <- list() # TODO: Implement this function
  return(fit)
}
