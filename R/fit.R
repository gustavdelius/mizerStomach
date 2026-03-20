#' Fit a distribution to log ppmr observations
#'
#' @param ppmr_data A data frame with log ppmr observations. See
#'   [validate_ppmr_data()] for details.
#' @param species A character vector with one or more species names.
#' @param distribution The distribution to fit. One of "normal",
#'   "trunc_exp" or "gauss_mix".
#' @param min_w_pred The minimum predator weight to include. Default is 0.
#' @param power Each observation is weighted by a power of the prey weight. The
#'   default is 0 which means that each prey individual contributes equally.
#'   `power = 1` means each prey individual contributes in proportion to its
#'   biomass.
#' @return A fit data frame with one row per species.
#'   See [validate_fit()] for details.
#' @examples
#' # Fit a normal distribution to one species
#' fit <- fit_log_ppmr(barnes_data, "Albacore", distribution = "normal")
#' fit
#'
#' # Fit multiple species at once
#' fit <- fit_log_ppmr(barnes_data,
#'                     c("Albacore", "Atlantic cod"),
#'                     distribution = "normal")
#' fit
#'
#' \donttest{
#' # Fit a truncated exponential distribution
#' fit_te <- fit_log_ppmr(barnes_data, "Albacore", distribution = "trunc_exp")
#' fit_te
#' }
#'
#' # Fit a Gaussian mixture distribution
#' fit_gm <- fit_log_ppmr(barnes_data, "Albacore", distribution = "gauss_mix")
#' fit_gm
#' @export
fit_log_ppmr <-
  function(ppmr_data, species,
           distribution = c("normal", "trunc_exp", "gauss_mix"),
           min_w_pred = 0, power = 0) {
    distribution <- match.arg(distribution)
    ppmr_data <- validate_ppmr_data(ppmr_data)
    missing <- setdiff(species, unique(ppmr_data$species))
    if (length(missing) > 0) {
      stop("Species not found in ppmr data: ",
           paste(missing, collapse = ", "))
    }

    rows <- lapply(species, function(sp) {
      data <- ppmr_data |>
        filter(species == !!sp,
               w_pred >= min_w_pred)
      value <- data$log_ppmr
      weight <- data$n_prey * data$w_prey ^ power

      if (distribution == "normal") {
        fit <- fit_normal(value, weight)
      } else if (distribution == "trunc_exp") {
        fit <- fit_truncated_exponential(value, weight)
      } else if (distribution == "gauss_mix") {
        fit <- fit_gaussian_mixture(value, weight)
      }
      fit$species <- sp
      fit$distribution <- distribution
      fit$power <- power
      fit$min_w_pred <- min_w_pred
      # Convert to a one-row data frame, wrapping vector values in lists
      as.data.frame(lapply(fit, function(v) {
        if (length(v) > 1) I(list(v)) else v
      }), stringsAsFactors = FALSE)
    })
    result <- do.call(rbind, rows)
    rownames(result) <- result$species
    return(result)
  }

#' Fit a normal distribution to weighted observations
#'
#' @param value A numeric vector of observed values
#' @param weight A numeric vector of weights
#' @return A list with the fitted parameters `mean` and `sd`
#' @examples
#' cod_data <- validate_ppmr_data(barnes_data, species = "Atlantic cod")
#' fit_normal(cod_data$log_ppmr, cod_data$n_prey)
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
#' @examples
#' \donttest{
#' cod_data <- validate_ppmr_data(barnes_data, species = "Atlantic cod")
#' fit_truncated_exponential(cod_data$log_ppmr, cod_data$n_prey)
#' }
#' @export
#' @keywords internal
fit_truncated_exponential <- function(value, weight) {
  validate_weighted_observations(value, weight)

  loglik <- function(alpha, ll, ul, lr, ur) {
    if (ll < 0.9 * min(value) || lr < ll ||
        (alpha + ul) < 1.01 || (ur - alpha) < 0.01) return(1e15)
    L <- tryCatch(
      dtexp(value, alpha, ll, ul, lr, ur),
      error = function(e) NULL
    )
    if (is.null(L) || anyNA(L)) return(1e15)
    return(-sum(log(L) * weight))
  }
  fit <- mle2(loglik, start = list(
    alpha = 0.5,
    ll = max(0.1, min(value)),
    lr = max(value),
    ul = 20,
    ur = 20),
    control = list(maxit = 10000))
  return(as.list(fit@coef))
}

#' Fit a Gaussian mixture distribution to weighted observations
#'
#' @param value A numeric vector of observed values
#' @param weight A numeric vector of weights
#' @return A list with the fitted parameters `mean`, `sd` and `p`, each of which
#'   is a vector with one entry for each component of the mixture
#' @examples
#' cod_data <- validate_ppmr_data(barnes_data, species = "Atlantic cod")
#' fit_gaussian_mixture(cod_data$log_ppmr, cod_data$n_prey)
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
