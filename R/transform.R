#' Re-express a fit at a different prey-mass weighting
#'
#' Applies family-specific parameter changes to fitted log-PPMR distributions
#' when observations are reweighted by another power of prey mass.
#'
#' @param fit A fit data frame with one or more rows, accepted by
#'   [validate_fit()]. Each row's current weighting exponent is read from its
#'   `power` column; a missing column is interpreted as zero.
#' @param power Single numeric value giving the target exponent of `w_prey` in
#'   the observation weights.
#'
#' @details
#' If a fit was obtained with weights \eqn{n_{prey}w_{prey}^{q_0}} and the
#' requested exponent is \eqn{q_1}, let \eqn{\Delta q=q_1-q_0}. For
#' \eqn{l=\log(w_{pred}/w_{prey})}, changing the exponent exponentially tilts
#' the fitted density by a factor proportional to
#' \eqn{\exp(-\Delta q\,l)}. For the normal and truncated-exponential families,
#' this tilt remains in the same family and is exact:
#'
#' * a normal mean changes from \eqn{\mu} to
#'   \eqn{\mu-\Delta q\,\sigma^2}, while `sd` is unchanged;
#' * a truncated-exponential `alpha` changes to `alpha - delta_q`, while its
#'   cutoff parameters are unchanged.
#'
#' For a Gaussian mixture, every component mean and the component proportions
#' are updated by [transform_gaussian_mixture()], while component standard
#' deviations are unchanged.
#'
#' Each row is transformed using its own existing `power`, after which all
#' rows have `power` set to the requested value. The input data frame is not
#' modified. This transformation assumes that the fitted log-PPMR distribution
#' can be represented independently of predator size; it is not a refit and
#' does not inspect the original observations.
#'
#' @return A validated fit data frame with transformed family parameters and
#'   `power` set to the requested value.
#'
#' @seealso [fit_log_ppmr()] for fitting at a chosen power and
#'   `vignette("density_functions", package = "mizerStomach")` for weighting
#'   interpretations and derivations. The kernel-specific target power is
#'   derived in `vignette("feeding_kernels", package = "mizerStomach")`.
#' @examples
#' # Fit a normal distribution and transform to biomass weighting
#' fit <- fit_log_ppmr(barnes_data, "Albacore", distribution = "normal")
#' fit_biomass <- transform_fit(fit, power = 1)
#' fit_biomass
#'
#' # Transform multiple species at once
#' fit2 <- fit_log_ppmr(barnes_data,
#'                      c("Albacore", "Atlantic cod"),
#'                      distribution = "normal")
#' transform_fit(fit2, power = 1)
#' @export
transform_fit <- function(fit, power) {
    fit <- validate_fit(fit)
    if (!is.numeric(power) || length(power) != 1) {
        stop("power must be a single numeric value")
    }
    for (i in seq_len(nrow(fit))) {
        dp <- power - fit$power[i]
        dist <- fit$distribution[i]
        if (dist == "normal") {
            fit$mean[i] <- fit$mean[i] - dp * fit$sd[i]^2
        } else if (dist == "trunc_exp") {
            fit$alpha[i] <- fit$alpha[i] - dp
        } else if (dist == "gauss_mix") {
            means <- if (is.list(fit$mean)) fit$mean[[i]] else fit$mean[i]
            sds <- if (is.list(fit$sd)) fit$sd[[i]] else fit$sd[i]
            ps <- if (is.list(fit$p)) fit$p[[i]] else fit$p[i]
            ps <- ps * exp(-dp * means + 0.5 * dp^2 * sds^2)
            ps <- ps / sum(ps)
            means <- means - dp * sds^2
            if (is.list(fit$mean)) {
                fit$mean[[i]] <- means
                fit$sd[[i]] <- sds
                fit$p[[i]] <- ps
            }
        }
        fit$power[i] <- power
    }
    return(fit)
}

#' Exponentially tilt a normal distribution
#'
#' @param fit A named list with numeric elements `mean` and `sd`.
#' @param power Numeric exponent by which prey-mass weighting is increased.
#'
#' @details
#' Multiplying a normal density in log PPMR \eqn{l} by
#' \eqn{\exp(-q l)} and renormalizing gives another normal density. Its mean is
#' `mean - power * sd^2` and its standard deviation is unchanged. Unlike
#' [transform_fit()], this low-level helper interprets `power` as the change in
#' exponent, not as an absolute target, and it does no validation.
#'
#' @return `fit` with its `mean` replaced by the transformed mean.
#' @examples
#' fit <- list(mean = 5.0, sd = 2.0)
#' transform_normal(fit, power = 1)
#' @export
#' @keywords internal
transform_normal <- function(fit, power = 1) {
    fit$mean <- fit$mean - power * fit$sd^2
    return(fit)
}

#' Exponentially tilt a truncated exponential distribution
#'
#' @param fit A named list with numeric elements `alpha`, `ll`, `ul`, `lr`, and
#'   `ur`; see [dtexp()].
#' @param power Numeric exponent by which prey-mass weighting is increased.
#'
#' @details
#' Multiplication by \eqn{\exp(-q l)} changes the exponential slope from
#' `alpha` to `alpha - power`; the two cutoff locations and steepnesses are
#' unchanged. Unlike [transform_fit()], this low-level helper interprets
#' `power` as the change in exponent and does no validation.
#'
#' @return `fit` with its `alpha` element replaced by the transformed value.
#' @examples
#' fit <- list(alpha = 0.5, ll = 2, ul = 20, lr = 12, ur = 20)
#' transform_truncated_exp(fit, power = 1)
#' @export
#' @keywords internal
transform_truncated_exp <- function(fit, power = 1) {
    fit$alpha <- fit$alpha - power
    return(fit)
}

#' Transform the weighting of a Gaussian mixture distribution
#'
#' @param fit A named list with numeric vectors `mean`, `sd`, and `p`, one value
#'   per mixture component. The component proportions in `p` should sum to one.
#' @param power Numeric exponent by which prey-mass weighting is increased.
#'
#' @details
#' Let \eqn{q} be `power`. Each component mean changes from \eqn{\mu_j} to
#' \eqn{\tilde\mu_j=\mu_j-q\sigma_j^2}. Standard deviations are retained and
#' component proportions are set proportional to
#' \eqn{p_j\exp(-q\mu_j+q^2\sigma_j^2/2)}, then normalized to sum to one. These
#' are the exact parameters obtained by multiplying the mixture density by
#' \eqn{\exp(-q l)} and renormalizing. Unlike [transform_fit()], this low-level
#' helper interprets `power` as the change in exponent and does no validation.
#'
#' @return `fit` with transformed `mean` and normalized `p`; `sd` is unchanged.
#' @examples
#' fit <- list(p = c(0.3, 0.7), mean = c(3, 7), sd = c(1, 2))
#' transform_gaussian_mixture(fit, power = 1)
#' @export
#' @keywords internal
transform_gaussian_mixture <- function(fit, power = 1) {
    p <- fit$p * exp(-power * fit$mean + 0.5 * power^2 * fit$sd^2)
    fit$p <- p / sum(p)
    fit$mean <- fit$mean - power * fit$sd^2
    return(fit)
}
