#' Fit a distribution to log ppmr observations
#'
#' @param ppmr_data A data frame with log ppmr observations, See
#'   [validate_ppmr_data()] for details
#' @param species A character string with the species name
#' @param distribution The distribution to fit. One of "normal",
#'   "truncated exponential" or "gaussian mixture".
#' @param min_w_pred The minimum predator weight to include. Default is 0.
#' @param power Each observation is weighted by a power of the prey weight. The
#'   default is 0 which means that each prey individual contributes equally.
#'   `power = 1` means each prey individual contributes in proportion to its
#'   biomass.
#' @return A `fit` object. See [validate_fit()] for details
#' @export
fit_log_ppmr <-
  function(ppmr_data, species,
           distribution = c("normal", "truncated exponential", "gaussian mixture"),
           min_w_pred = 0, power = 0) {
    distribution <- match.arg(distribution)
    ppmr_data <- validate_ppmr_data(ppmr_data)
    if (!species %in% unique(ppmr_data$species)) {
      stop("Species", fit$species, "not found in ppmr data")
    }
    data <- ppmr_data |>
        filter(species == !!species,
               w_pred >= min_w_pred)
    value <- data$log_ppmr
    weight <- data$n_prey * data$w_prey ^ power

    if (distribution == "normal") {
      fit <- fit_normal(value, weight)
    } else if (distribution == "truncated exponential") {
      fit <- fit_truncated_exponential(value, weight)
    } else if (distribution == "gaussian mixture") {
      fit <- fit_gaussian_mixture(value, weight)
    }
    fit$species <- species
    fit$distribution <- distribution
    fit$power <- power
    fit$min_w_pred <- min_w_pred
    return(fit)
  }

#' Fit a normal distribution to weighted observations
#'
#' @param value A numeric vector of observed values
#' @param weight A numeric vector of weights
#' @return A list with the fitted parameters `mean` and `sd`
#' @export
#' @keywords internal
fit_normal <- function(value, weight) {
  validate_weighted_observations(value, weight)
  fit <- list(mean = weighted.mean(value, w = weight),
              sd = weighted.sd(value, w = weight))
  return(fit)
}

weighted.sd <- function(x, w) {
  sqrt(sum(w * (x - weighted.mean(x, w))^2 / sum(w)))
}

#' Fit a truncated exponential distribution to weighted observations
#'
#' @param value A numeric vector of observed values
#' @param weight A numeric vector of weights
#' @return A list with the fitted parameters `alpha`, `ll`, `ul`, `lr`, `ur`
#' @export
#' @keywords internal
fit_truncated_exponential <- function(value, weight) {
  validate_weighted_observations(value, weight)

  loglik <- function(alpha, ll, ul, lr, ur) {
    L <- dtexp(value, alpha, ll, ul, lr, ur)
    return(-sum(log(L) * weight))
  }
  fit <- mle2(loglik, start = list(
    alpha = 0.5,
    ll = Hmisc::wtd.quantile(value, weight, 0.05),
    lr = Hmisc::wtd.quantile(value, weight, 0.95),
    ul = 5,
    ur = 5),
    method = "L-BFGS-B",
    control = list(maxit = 10000))
  return(as.list(fit@coef))
}

#' Fit a Gaussian mixture distribution to weighted observations
#'
#' @param value A numeric vector of observed values
#' @param weight A numeric vector of weights
#' @return A list with the fitted parameters `mean`, `sd` and `p`, each of which
#'   is a vector with one entry for each component of the mixture
#' @export
#' @keywords internal
fit_gaussian_mixture <- function(value, weight,
                                 k = 2, max_iter = 100, tol = 1e-6) {
    validate_weighted_observations(value, weight)
    n <- length(value)

    # Initialize parameters
    lambda <- rep(1/k, k)
    mu <- seq(min(value), max(value), length.out = k)
    sigma <- rep(sd(value), k)

    log_likelihood <- numeric(max_iter)

    for (iter in 1:max_iter) {
        # E-step: Calculate responsibilities
        gamma <- matrix(0, nrow = n, ncol = k)
        for (j in 1:k) {
            gamma[, j] <- lambda[j] * dnorm(value, mean = mu[j], sd = sigma[j])
        }
        gamma <- gamma / rowSums(gamma)

        # M-step: Update parameters with weights
        for (j in 1:k) {
            w_gamma <- weight * gamma[, j]
            lambda[j] <- sum(w_gamma) / sum(weight)
            mu[j] <- sum(w_gamma * value) / sum(w_gamma)
            sigma[j] <- sqrt(sum(w_gamma * (value - mu[j])^2) / sum(w_gamma))
        }

        # Calculate log-likelihood
        log_likelihood[iter] <- sum(weight * log(rowSums(gamma)))

        # Check for convergence
        if (iter > 1 &&
            abs(log_likelihood[iter] - log_likelihood[iter - 1]) < tol) {
            break
        }
    }

    list(p = lambda, mean = mu, sd = sigma)
}
