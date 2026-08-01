# Functions for validating arguments

#' Validate weighted observations
#'
#' Checks the basic shape and type requirements shared by the low-level fitting
#' functions.
#'
#' @param value Vector of observed values.
#' @param weight Vector of observation weights.
#'
#' @details
#' The function requires at least ten entries in `value`, equal vector lengths,
#' numeric values and weights, and no negative weights. The ten-entry condition
#' concerns rows rather than the sum of frequency weights. Distribution-
#' specific conditions are left to the fitting routines.
#'
#' @return `TRUE`. An informative error is raised when a check fails.
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

#' Validate and standardize a fit data frame
#'
#' Checks that each row has the parameter columns required by its distribution,
#' adds standard metadata defaults, and sets row names from species names.
#'
#' @param fit A data frame with one row per species, or a named list describing
#'   one fit for backwards compatibility.
#'
#' @details
#' Every fit needs `species` and `distribution`. Missing `power` and
#' `min_w_pred` columns are added with value zero. Required family-specific
#' columns are:
#'
#' * `mean` and `sd` for `distribution = "normal"`;
#' * `alpha`, `ll`, `ul`, `lr`, and `ur` for `"trunc_exp"`;
#' * `mean`, `sd`, and `p` for `"gauss_mix"`.
#'
#' Gaussian-mixture parameters are normally list-columns; within each row,
#' their three vectors must have equal lengths. A named list is converted to a
#' one-row data frame, with vector-valued elements retained as list-columns.
#'
#' This function validates structure, not statistical admissibility: it does
#' not, for example, check that standard deviations are positive, mixture
#' weights sum to one, or cutoff parameters define a stable density.
#'
#' @return A data frame with any missing metadata columns added and row names
#'   set to `fit$species`. The input object itself is not modified.
#' @family validation functions
#' @examples
#' # Validate a fit data frame produced by fit_log_ppmr()
#' fit <- fit_log_ppmr(barnes_data, "Albacore", distribution = "normal")
#' validate_fit(fit)
#'
#' # A named list is also accepted (backwards compatibility)
#' fit_list <- list(species = "test", distribution = "normal",
#'                  mean = 5, sd = 2)
#' validate_fit(fit_list)
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

#' Validate predator/prey mass-ratio observations
#'
#' Checks the columns required by the fitting and plotting functions, optionally
#' selects one predator species, and supplies log predator/prey mass ratios.
#'
#' @param ppmr_data A data frame containing at least `species`, `w_pred`,
#'   `w_prey`, and `n_prey`. Predator and prey masses must use the same mass
#'   unit; package examples use grams. Extra columns are preserved.
#' @param species Optional single character string. If supplied, only rows for
#'   that predator species are returned; an absent species causes an error.
#'
#' @details
#' The calculated value is
#' \deqn{\mathrm{log\_ppmr}=\log(w_{pred})-\log(w_{prey})
#' =\log(w_{pred}/w_{prey}).}
#' If `log_ppmr` is absent, this column is appended. If it already exists, it is
#' checked against the calculation and retained; a disagreement produces a
#' warning. A warning is also issued when prey are heavier than their predator,
#' which produces a negative log PPMR. Missing masses that make the calculated
#' ratio `NA` cause an error.
#'
#' This is a format validator rather than a full data-cleaning routine. In
#' particular, callers should ensure that masses are finite and positive and
#' that `n_prey` contains meaningful non-negative counts or frequencies before
#' fitting.
#'
#' @return The input data frame, optionally filtered by `species`, with a
#'   `log_ppmr` column guaranteed to be present. Column classes and extra
#'   columns are otherwise retained.
#'
#' @seealso `vignette("mizerStomach", package = "mizerStomach")` for the input
#'   workflow and `vignette("PPMR_distributions", package = "mizerStomach")`
#'   for scientific data-quality considerations beyond these structural checks.
#' @family validation functions
#' @examples
#' # Validate the barnes_data dataset
#' valid_data <- validate_ppmr_data(barnes_data)
#' head(valid_data)
#'
#' # Validate and filter to a single species
#' cod_data <- validate_ppmr_data(barnes_data, species = "Atlantic cod")
#' nrow(cod_data)
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
