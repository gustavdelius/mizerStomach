# Functions for validating arguments

#' Validate weighted observations
#'
#' @param value A numeric vector of observed values
#' @param weight A numeric vector of weights
#' @return TRUE if the arguments are valid. Raises and error otherwise.
#' @keywords internal
validate_weighted_observations <- function(value, weight) {
  if (length(value) < 100) {
    stop("Not enough data to fit a distribution")
  }
  if (length(value) != length(weight)) {
    stop("Length of value and weight must be the same")
  }
  if (!all(is.numeric(value))) {
    stop("value must be numeric")
  }
  if (!all(is.numeric(weight))) {
    stop("weight must be numeric")
  }
  if (any(weight < 0)) {
    stop("Weights must be non-negative")
  }
  return(TRUE)
}

#' Validate a fit object
#'
#' @param fit A list with the fitted distribution parameters
#' @return TRUE if the fit object is valid. Raises an error otherwise.
#' @export
validate_fit <- function(fit) {
  if (!is.list(fit)) {
    stop("fit must be a list")
  }
  if (!hasName(fit, "species")) {
    stop("fit must have a species attribute")
  }
  if (!hasName(fit, "power")) {
    stop("fit must have a power attribute")
  }
  if (!hasName(fit, "distribution")) {
    stop("fit must have a distribution attribute")
  }
  if (fit$distribution == "normal") {
    if (!hasName(fit, "mean")) {
      stop("fit must have a mean attribute")
    }
    if (!hasName(fit, "sd")) {
      stop("fit must have a sd attribute")
    }
  } else if (fit$distribution == "truncated_exp") {
    if (!hasName(fit, "exp")) {
      stop("fit must have an exp attribute")
    }
    if (!hasName(fit, "ll")) {
      stop("fit must have an ll attribute")
    }
    if (!hasName(fit, "ul")) {
      stop("fit must have an ul attribute")
    }
    if (!hasName(fit, "lr")) {
      stop("fit must have an lr attribute")
    }
    if (!hasName(fit, "ur")) {
      stop("fit must have an ur attribute")
    }
  } else if (fit$distribution == "gaussian_mixture") {
    if (!hasName(fit, "mean")) {
      stop("fit must have a mean attribute")
    }
    if (!hasName(fit, "sd")) {
      stop("fit must have a sd attribute")
    }
    if (!hasName(fit, "p")) {
      stop("fit must have a p attribute")
    }
    if (!all(length(fit$mean) == length(fit$sd),
             length(fit$mean) == length(fit$p))) {
      stop("mean, sd and p must have the same length")
    }
  } else {
    stop("Unknown distribution", fit$distribution)
  }
  return(TRUE)
}

#' Validate a data frame with predator/prey mass ratio observations
#'
#' @param ppmr_data A data frame with log ppmr observations
#' @return TRUE if the data frame is valid. Raises an error otherwise.
#' @export
validate_ppmr_data <- function(ppmr_data) {
  if (!is.data.frame(ppmr_data)) {
    stop("ppmr data must be a data frame")
  }
  varnames <- c("species", "log_ppmr", "w_prey", "n_prey")
  if (!all(varnames %in% names(ppmr_data))) {
    stop("ppmr data must have the columns", paste(varnames, collapse = ", "))
  }
  return(TRUE)
}
