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
    if (length(value) < 10) {
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
#' A fit object is a data frame with one row per species, containing columns
#' for the fitted distribution parameters. Required columns are `species` and
#' `distribution`. Optional columns `power` (default 0) and `min_w_pred`
#' (default 0) are added if missing.
#'
#' For backwards compatibility, a named list with the same fields is also
#' accepted and silently converted to a single-row data frame.
#'
#' @param fit A data frame or list with the fitted distribution parameters
#' @return The valid fit data frame with species as rownames
#' @family validation functions
#' @export
validate_fit <- function(fit) {
    # Backwards compatibility: accept a named list and convert to data frame
    if (is.list(fit) && !is.data.frame(fit)) {
        fit <- as.data.frame(lapply(fit, function(v) {
            if (length(v) > 1) I(list(v)) else v
        }), stringsAsFactors = FALSE)
    }
    if (!is.data.frame(fit)) {
        stop("fit must be a data frame (or a named list)")
    }
    if (!hasName(fit, "species")) {
        stop("fit must have a `species` column")
    }
    if (!hasName(fit, "power")) {
        fit$power <- 0
    }
    if (!hasName(fit, "distribution")) {
        stop("fit must have a `distribution` column")
    }
    if (!hasName(fit, "min_w_pred")) {
        fit$min_w_pred <- 0
    }
    for (i in seq_len(nrow(fit))) {
        row <- fit[i, ]
        dist <- row$distribution
        sp <- row$species
        if (dist == "normal") {
            if (!hasName(fit, "mean")) {
                stop("fit for ", sp, " must have a `mean` column")
            }
            if (!hasName(fit, "sd")) {
                stop("fit for ", sp, " must have a `sd` column")
            }
        } else if (dist == "trunc_exp") {
            for (col in c("alpha", "ll", "ul", "lr", "ur")) {
                if (!hasName(fit, col)) {
                    stop("fit for ", sp, " must have a `", col, "` column")
                }
            }
        } else if (dist == "gauss_mix") {
            for (col in c("mean", "sd", "p")) {
                if (!hasName(fit, col)) {
                    stop("fit for ", sp, " must have a `", col, "` column")
                }
            }
            row_mean <- if (is.list(row$mean)) row$mean[[1]] else row$mean
            row_sd <- if (is.list(row$sd)) row$sd[[1]] else row$sd
            row_p <- if (is.list(row$p)) row$p[[1]] else row$p
            if (!all(length(row_mean) == length(row_sd),
                     length(row_mean) == length(row_p))) {
                stop("mean, sd and p must have the same length for ", sp)
            }
        } else {
            stop("Unknown distribution ", dist, " for ", sp)
        }
    }
    rownames(fit) <- fit$species
    return(fit)
}

#' Validate a data frame with predator/prey mass ratio observations
#'
#' The data frame needs to have the columns "species", "w_pred", "w_prey" and
#' "n_prey". The function adds a `log_ppmr` column to the data frame with the
#' logarithm of the predator/prey mass ratio.
#'
#' If a `log_ppmr` column already exists, it is overwritten and a warning is
#' issued if discrepancies were found. A warning is also issued if any prey
#' is heavier than the predator.
#'
#' @param ppmr_data A data frame with log ppmr observations
#' @param species The species to select. Optional. If supplied the function only
#'   selects the rows for the specified species. If the species is not found, an
#'   error is raised.
#' @return Valid ppmr data frame with columns "species", "w_pred", "w_prey",
#'  "n_prey" and "log_ppmr" and rows for the specified species.
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
            stop("Species ", species, " not found in ppmr data")
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
