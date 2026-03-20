#' Get density from a fit object
#'
#' @param x A numeric vector of values at which to evaluate the density
#' @param fit A fit object
#' @return A numeric vector of densities
#' @export
get_density <- function(x, fit) {
    fit <- validate_fit(fit)
    if (fit$distribution == "normal") {
        d <- dnorm(x, mean = fit$mean, sd = fit$sd)
    } else if (fit$distribution == "trunc_exp") {
        d <- dtexp(x, alpha = fit$alpha, ll = fit$ll, ul = fit$ul,
                   lr = fit$lr, ur = fit$ur)
    } else if (fit$distribution == "gauss_mix") {
        d <- x * 0
        for (i in seq_along(fit$p)) {
            d <- d + fit$p[i] * dnorm(x, mean = fit$mean[i], sd = fit$sd[i])
        }
    }
    return(d)
}

#' Density function of truncated exponential distribution
#'
#' @param x A numeric vector of values
#' @param alpha exponent
#' @param ll location of lower sigmoid
#' @param ul steepness of lower sigmoid
#' @param lr location of upper sigmoid
#' @param ur steepness of upper sigmoid
#' @return A numeric vector of densities
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
