#' Fit a distribution to log ppmr observations
#'
#' @param ppmr_data A data frame with log ppmr observations, See
#'   [validate_ppmr_data()] for details
#' @param species A character string with the species name
#' @param distribution The distribution to fit. One of "normal",
#'   "truncated_exponential" or "gaussian_mixture".
#' @param power Each observation is weighted by a power of the prey weight. The
#'   default is 0 which means that each prey individual contributes equally.
#'   `power = 1` means each prey individual contributes in proportion to its
#'   biomass.
#' @return A `fit` object. See [validate_fit()] for details
#' @export
fit_log_ppmr <-
  function(ppmr_data, species,
           distribution = c("normal", "truncated_exponential", "gaussian_mixture"),
           power = 0) {
    distribution <- match.arg(distribution)
    ppmr_data <- validate_ppmr_data(ppmr_data)
    if (!species %in% unique(ppmr_data$species)) {
      stop("Species", fit$species, "not found in ppmr data")
    }
    data <- ppmr_data[ppmr_data$species == species, ]
    value <- data$log_ppmr
    weight <- data$n_prey * data$w_prey ^ power

    if (distribution == "normal") {
      fit <- fit_normal(value, weight)
    } else if (distribution == "truncated_exponential") {
      fit <- fit_truncated_exponential(value, weight)
    } else if (distribution == "gaussian_mixture") {
      fit <- fit_gaussian_mixture(value, weight)
    }
    fit$species <- species
    fit$distribution <- distribution
    fit$power <- power
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

fl <- function(x, alpha, ll, ul, lr, ur) {
  dl <- ll - x
  dr <- x - lr
  fl <- exp(alpha * x) /
    (1 + exp(ul * dl)) /
    (1 + exp(ur * dr))
  # fl[fl <= 0] <- 0
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
    ll = min(value),
    lr = max(value),
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
fit_gaussian_mixture <- function(value, weight) {
  validate_weighted_observations(value, weight)
  fit <- list() # TODO: Implement this function
  return(fit)
}

get_density <- function(x, fit) {
  fit <- validate_fit(fit)
  if (fit$distribution == "normal") {
    d <- dnorm(x, mean = fit$mean, sd = fit$sd)
  } else if (fit$distribution == "truncated_exponential") {
    d <- dtexp(x, alpha = fit$alpha, ll = fit$ll, ul = fit$ul,
               lr = fit$lr, ur = fit$ur)
  } else if (fit$distribution == "gaussian_mixture") {
    d <- dnorm(x, mean = fit$mean, sd = fit$sd)
    for (i in seq_along(fit$p)) {
      d <- d + fit$p[i] * dnorm(x, mean = fit$mean[i], sd = fit$sd[i])
    }
  }
}
