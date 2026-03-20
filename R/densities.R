#' Get density from a fit object
#'
#' @param x A numeric vector of values at which to evaluate the density
#' @param fit A fit data frame (one or more rows)
#' @return If `fit` has a single row, a numeric vector of densities. If `fit`
#'   has multiple rows, a matrix with one column per species.
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

#' Get density for a single species from a fit list
#' @param x A numeric vector of values
#' @param fit A named list with fitted parameters for one species
#' @return A numeric vector of densities
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

#' Density function of truncated exponential distribution
#'
#' @param x A numeric vector of values
#' @param alpha exponent
#' @param ll location of lower sigmoid
#' @param ul steepness of lower sigmoid
#' @param lr location of upper sigmoid
#' @param ur steepness of upper sigmoid
#' @return A numeric vector of densities
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
