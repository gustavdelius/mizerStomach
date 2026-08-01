#' Evaluate fitted log-PPMR densities
#'
#' Evaluates one or more fitted probability densities on the log
#' predator/prey mass-ratio scale.
#'
#' @param x Numeric vector of log-PPMR values at which to evaluate the density.
#' @param fit A one- or multi-row fit data frame accepted by [validate_fit()].
#'
#' @details
#' A `"normal"` row is evaluated with [stats::dnorm()], a `"trunc_exp"` row
#' with [dtexp()], and a `"gauss_mix"` row as the weighted sum
#' \eqn{\sum_j p_j\,\mathrm{dnorm}(x;\mu_j,\sigma_j)}. The `power` column is
#' metadata describing the weighting represented by the fit; this function
#' does not transform it. Call [transform_fit()] first when a density at
#' another prey-mass weighting is required.
#'
#' @return If `fit` has one row, a numeric vector with the same length as `x`.
#'   If it has multiple rows, a numeric matrix with `length(x)` rows and one
#'   column per fit row, named with `fit$species`.
#'
#' @seealso [fit_log_ppmr()] to create fits and [plot_log_ppmr_fit()] to compare
#'   fitted and empirical densities.
#' @examples
#' # Fit a normal distribution and evaluate its density
#' fit <- fit_log_ppmr(barnes_data, "Albacore", distribution = "normal")
#' x <- seq(0, 15, length.out = 100)
#' density <- get_density(x, fit)
#' plot(x, density, type = "l", main = "Albacore normal fit")
#'
#' # Density for multiple species returns a matrix
#' fit2 <- fit_log_ppmr(barnes_data,
#'                      c("Albacore", "Atlantic cod"),
#'                      distribution = "normal")
#' densities <- get_density(x, fit2)
#' head(densities)
#' @export
get_density <- function(x, fit) {
    fit <- validate_fit(fit)
    if (nrow(fit) == 1) {
        return(get_density_single(x, as.list(fit[1, ])))
    }
    result <- sapply(seq_len(nrow(fit)), function(i) {
        get_density_single(x, as.list(fit[i, ]))
    })
    colnames(result) <- fit$species
    return(result)
}

#' Evaluate one fitted density
#'
#' Internal dispatcher used by [get_density()].
#'
#' @param x Numeric vector of log-PPMR values.
#' @param fit Named list containing `distribution` and the parameters required
#'   by that family for one species.
#' @return A numeric vector with the same length as `x`.
#' @keywords internal
get_density_single <- function(x, fit) {
    if (fit$distribution == "normal") {
        d <- dnorm(x, mean = fit$mean, sd = fit$sd)
    } else if (fit$distribution == "trunc_exp") {
        d <- dtexp(x, alpha = fit$alpha, ll = fit$ll, ul = fit$ul,
                   lr = fit$lr, ur = fit$ur)
    } else if (fit$distribution == "gauss_mix") {
        p <- if (is.list(fit$p)) fit$p[[1]] else fit$p
        means <- if (is.list(fit$mean)) fit$mean[[1]] else fit$mean
        sds <- if (is.list(fit$sd)) fit$sd[[1]] else fit$sd
        d <- x * 0
        for (i in seq_along(p)) {
            d <- d + p[i] * dnorm(x, mean = means[i], sd = sds[i])
        }
    }
    return(d)
}

#' Smoothly truncated exponential density
#'
#' Evaluates the exponentially sloped density with smooth logistic cutoffs used
#' for truncated-power-law feeding kernels.
#'
#' @param x Numeric vector, normally containing log predator/prey mass ratios.
#' @param alpha Numeric exponential slope between the cutoffs. Positive values
#'   favor larger `x`; negative values favor smaller `x`.
#' @param ll Numeric location of the lower (left) logistic cutoff.
#' @param ul Numeric steepness of the lower cutoff. Larger positive values make
#'   the transition sharper.
#' @param lr Numeric location of the upper (right) logistic cutoff.
#' @param ur Numeric steepness of the upper cutoff. Larger positive values make
#'   the transition sharper.
#'
#' @details
#' Before normalization, the density is proportional to
#' \deqn{\frac{\exp(\alpha x)}
#' {(1+\exp(u_l(l_l-x)))(1+\exp(u_r(x-l_r)))}}.
#' The implementation evaluates an algebraically equivalent expression in log
#' space, centered at `(ll + lr) / 2`, to reduce overflow during optimization.
#' The normalizing constant is obtained by numerical integration over
#' \eqn{0 \le x \le 30}, the intended log-PPMR range. The function can evaluate
#' `x` outside that range, but it remains normalized only with respect to the
#' interval `[0, 30]`.
#'
#' If numerical integration fails, or any requested density is non-finite or
#' non-positive, the function returns `NA` for every element of `x`.
#' This all-or-nothing behavior lets [fit_truncated_exponential()] reject
#' unstable parameter combinations during optimization.
#'
#' @return A density vector with the same length as `x`, or an all-`NA` vector
#'   when the density cannot be evaluated reliably.
#'
#' @seealso [fit_truncated_exponential()] for weighted maximum-likelihood
#'   estimation and [transform_truncated_exp()] for changing its weighting.
#' @examples
#' # Evaluate the truncated exponential density
#' x <- seq(0, 15, length.out = 100)
#' d <- dtexp(x, alpha = 0.5, ll = 2, ul = 20, lr = 12, ur = 20)
#' plot(x, d, type = "l", main = "Truncated exponential density")
#' @keywords internal
#' @export
dtexp <- function(x, alpha, ll, ul, lr, ur) {
    d <- fl(x, alpha, ll, ul, lr, ur)
    integral_result <- tryCatch(
        integrate(fl, 0, 30, alpha = alpha, ll = ll, ul = ul, lr = lr, ur = ur),
        error = function(e) {
            print("Integration failed")
            print(e)
            return(NULL)
        }
    )
    # The following is crucial to keep the search from going deeper into
    # nonsense values.
    if (is.null(integral_result)) {
        return(rep(NA, length(x)))
    }

    d <- d / integral_result$value

    if (any(!is.finite(d) | d <= 0)) {
        return(rep(NA, length(x)))
    }
    return(d)
}

fl <- function(x, alpha, ll, ul, lr, ur) {
    # Computed in log-space, normalised by midpoint to keep values O(1).
    # The normalisation constant cancels in dtexp (numerator / integral),
    # so this is numerically equivalent but avoids exp() overflow when the
    # optimizer explores large alpha or ul/ur values.
    mid <- (ll + lr) / 2
    log_fl <- alpha * (x - mid) -
        log1p(exp(pmin(ul * (ll - x), 500))) -
        log1p(exp(pmin(ur * (x - lr), 500)))
    exp(log_fl)
}
