#' Transform a fitted distribution to represent data weighted by a different
#' power of w
#'
#' @param fit A fit data frame (one or more rows)
#' @param power The power to raise the weights to
#' @return An updated fit data frame
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
            means <- means - dp * sds^2
            ps <- ps * exp(-dp * means + 0.5 * dp^2 * sds^2)
            ps <- ps / sum(ps)
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

