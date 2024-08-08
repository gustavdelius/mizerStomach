# Functions for validating arguments

#' Validate weighted observations
#'
#' This is an internal function that checks that the arguments to the fitting
#'
#'
#' @param value A numeric vector of observed values
#' @param weight A numeric vector of weights
#' @return TRUE if the arguments are valid. Raises and error otherwise.
#' @keywords internal
#' @family validation functions
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
#' @return The valid fit object
#' @family validation functions
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
    } else if (fit$distribution == "truncated_exponential") {
        if (!hasName(fit, "alpha")) {
            stop("fit must have an `alpha` attribute")
        }
        if (!hasName(fit, "ll")) {
            stop("fit must have an `ll` attribute")
        }
        if (!hasName(fit, "ul")) {
            stop("fit must have an `ul` attribute")
        }
        if (!hasName(fit, "lr")) {
            stop("fit must have an `lr` attribute")
        }
        if (!hasName(fit, "ur")) {
            stop("fit must have an `ur` attribute")
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
        stop("Unknown distribution ", fit$distribution)
    }
    return(fit)
}

#' Validate a data frame with predator/prey mass ratio observations
#'
#' The data frame needs to have the columns "species", "w_pred", "w_prey" and
#' "n_prey". The function adds a `log_ppmr` column to the data frame with the
#' natural logarithm of the predator/prey mass ratio. If the `species` argument
#' is supplied the function then only selects the rows for the specified
#' species. If the species is not found, an error is raised.
#'
#' If a `log_ppmr` column already exists, it is overwritten and a warning is
#' issued if discrepancies were found. A warning is also issued if any prey
#' is heavier than the predator.
#'
#' @param ppmr_data A data frame with log ppmr observations
#' @param species The species to select. Optional.
#' @return Valid ppmr data frame with columns "species", "w_pred", "w_prey",
#'  "n_prey" and "log_ppmr"
#' @family validation functions
#' @export
validate_ppmr_data <- function(ppmr_data, species = NULL) {
    if (!is.data.frame(ppmr_data)) {
        stop("ppmr data must be a data frame")
    }
    varnames <- c("species", "w_pred", "w_prey", "n_prey")
    if (!all(varnames %in% names(ppmr_data))) {
        stop("ppmr data must have the columns ", paste(varnames, collapse = ", "))
    }
    if (!is.null(species)) {
        if (!species %in% unique(ppmr_data$species)) {
            stop("Species", fit$species, "not found in ppmr data")
        }
        ppmr_data <- ppmr_data[ppmr_data$species == species, ]
    }
    if (any(ppmr_data$w_prey > ppmr_data$w_pred)) {
        warning("Some prey are heavier than the predator")
    }

    log_ppmr <- log(ppmr_data$w_pred) - log(ppmr_data$w_prey)
    if (any(is.na(log_ppmr))) {
        stop("Some prey or predator weights are missing")
    }
    if (hasName(ppmr_data, "log_ppmr")) {
        if (!isTRUE(all.equal(ppmr_data$log_ppmr, log_ppmr))) {
            warning("The existing `log_ppmr` column does not agree with the one calculated from `w_pred` and `w_prey`.")
        }
    } else {
        ppmr_data$log_ppmr <- log_ppmr
    }
    return(ppmr_data)
}
