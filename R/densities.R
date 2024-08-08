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
    } else if (fit$distribution == "truncated_exponential") {
        d <- dtexp(x, alpha = fit$alpha, ll = fit$ll, ul = fit$ul,
                   lr = fit$lr, ur = fit$ur)
    } else if (fit$distribution == "gaussian_mixture") {
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

    if (any(d <= 0)) {
        stop("The density contains non-positive values when",
             " alpha = ", alpha, " ll = ", ll, " ul = ", ul,
             " lr = ", lr, " ur = ", ur)
    }
    return(d)
}

fl <- function(x, alpha, ll, ul, lr, ur) {
    dl <- ll - x
    dr <- x - lr
    fl <- exp(alpha * x) /
        (1 + exp(ul * dl)) /
        (1 + exp(ur * dr))
    # fl[fl <= 0] <- 0
}
