#' Transform a fitted distribution to represent data weighted by a different
#' power of w
#'
#' @param fit A list with the fitted distribution parameters
#' @param power The power to raise the weights to
#' @return An updated `fit` object
#' @export
transform_fit <- function(fit, power) {
    fit <- validate_fit(fit)
    if (!is.numeric(power) || length(power) != 1) {
        stop("power must be a single numeric value")
    }
    power <- power - fit$power
    if (fit$distribution == "normal") {
        fit <- transform_normal(fit, power)
    } else if (fit$distribution == "truncated_exponential") {
        fit <- transform_truncated_exp(fit, power)
    } else if (fit$distribution == "gaussian_mixture") {
        fit <- transform_gaussian_mixture(fit, power)
    }
    fit$power <- power
    return(fit)
}

#' Transform a normal distribution to represent data weighted by a power of w
#'
#' @param fit A list with the parameters `mean` and `sd`
#' @param power The power to raise the weights to
#' @return A list with the updated parameters
#' @export
#' @keywords internal
transform_normal <- function(fit, power = 1) {
    fit$mean <- fit$mean - power * fit$sd^2
    return(fit)
}

#' Transform a truncated exponential distribution to represent data weighted by a power of w
#'
#' @param fit A list with the parameters `exp`, `ll`, `ul`, `lr`, `ur`
#' @param power The power to raise the weights to
#' @return A list with the updated parameters
#' @export
#' @keywords internal
transform_truncated_exp <- function(fit, power = 1) {
    fit$alpha <- fit$alpha - power
    return(fit)
}

#' Transform a Gaussian mixture distribution to represent data weighted by a power of w
#'
#' @param fit A list with entries `mean`, `sd` and `p`, each of which is a
#'   vector with one entry for each component of the mixture
#' @param power The power to raise the weights to
#' @return A list with the updated parameters
#' @export
#' @keywords internal
transform_gaussian_mixture <- function(fit, power = 1) {
    fit$mean <- fit$mean - power * fit$sd^2
    p <- fit$p * exp(-power * fit$mean + 0.5 * power^2 * fit$sd^2)
    fit$p <- p / sum(p)
    return(fit)
}
