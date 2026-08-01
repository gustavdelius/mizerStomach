#' Fit distributions to log predator/prey mass ratios
#'
#' Fits one distribution to the log predator/prey mass-ratio observations for
#' each requested predator species. Rows can represent groups of identical
#' observations through `n_prey`, and can additionally be weighted by prey
#' mass.
#'
#' @param ppmr_data A data frame accepted by [validate_ppmr_data()]. It must
#'   contain `species`, `w_pred`, `w_prey`, and `n_prey` columns; `log_ppmr` is
#'   calculated when it is absent.
#' @param species A character vector naming one or more predator species in
#'   `ppmr_data`. Ignored when `fits` is supplied.
#' @param distribution Character string selecting the fitted family. One of
#'   `"normal"`, `"trunc_exp"`, or `"gauss_mix"`.
#' @param min_w_pred Single numeric value. Observations with predator mass
#'   `w_pred < min_w_pred` are excluded before fitting. The same threshold is
#'   applied to every requested species and is stored in the result.
#' @param power Single numeric value giving the exponent of prey mass in the
#'   observation weights. Observation \eqn{i} receives weight
#'   \eqn{n_i w_i^q}, where \eqn{n_i} is `n_prey`, \eqn{w_i} is `w_prey`, and
#'   \eqn{q} is `power`. Thus `power = 0` fits the prey-number distribution and
#'   `power = 1` fits the prey-biomass distribution.
#' @param fits An optional fit data frame accepted by [validate_fit()]. When
#'   supplied, its `species` column determines which species are fitted. For a
#'   `"trunc_exp"` refit, a row already using that family also supplies the
#'   starting values `alpha`, `ll`, `ul`, `lr`, and `ur`; other families do not
#'   use values from `fits` as starting values.
#'
#' @details
#' For each species, the function first validates the input, applies
#' `min_w_pred`, uses the validated `log_ppmr` column as the response, and sets
#' the frequency weight to
#' \eqn{n_{prey}w_{prey}^{q}}. Multiplying every weight by the same positive
#' constant does not change any of the fits.
#'
#' The fitting method depends on `distribution`:
#'
#' * `"normal"` uses the weighted mean and the maximum-likelihood weighted
#'   standard deviation (the variance divisor is the sum of the weights, with
#'   no small-sample correction); see [fit_normal()].
#' * `"trunc_exp"` minimizes the weighted negative log-likelihood with
#'   [bbmle::mle2()]. The density is an exponential curve with smooth logistic
#'   cutoffs at both ends; see [fit_truncated_exponential()] and [dtexp()].
#' * `"gauss_mix"` uses [fit_gaussian_mixture()] with two normal components,
#'   equal initial mixing proportions, and at most 100 EM iterations.
#'
#' Fits are made independently by species; there is no pooling of parameters.
#' The returned `power` records which weighted distribution was fitted. Use
#' [transform_fit()] to express the fitted family at another prey-mass
#' weighting without refitting the observations.
#'
#' @return A data frame with one row per species and row names equal to
#'   `species`. Every row contains `species`, `distribution`, `power`, and
#'   `min_w_pred`, plus family-specific parameters: `mean` and `sd` for a
#'   normal fit; `alpha`, `ll`, `ul`, `lr`, and `ur` for a truncated
#'   exponential fit; or list-columns `p`, `mean`, and `sd` for a Gaussian
#'   mixture. See [validate_fit()].
#'
#' @seealso [plot_log_ppmr_fit()] to compare a fit with the observations,
#'   [transform_fit()] to change its weighting, and [set_kernel_params()] to
#'   transfer supported fits to a mizer model.
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
#'
#' # Refit using previous fit as starting point
#' fit_te2 <- fit_log_ppmr(barnes_data, distribution = "trunc_exp",
#'                          fits = fit_te)
#' fit_te2
#' }
#'
#' # Fit a Gaussian mixture distribution
#' fit_gm <- fit_log_ppmr(barnes_data, "Albacore", distribution = "gauss_mix")
#' fit_gm
#' @export
fit_log_ppmr <-
  function(ppmr_data, species,
           distribution = c("normal", "trunc_exp", "gauss_mix"),
           min_w_pred = 0, power = 0, fits = NULL) {
    distribution <- match.arg(distribution)
    ppmr_data <- validate_ppmr_data(ppmr_data)

    if (!is.null(fits)) {
      fits <- validate_fit(fits)
      species <- fits$species
    }

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
        start <- NULL
        if (!is.null(fits) && sp %in% fits$species &&
            fits[sp, "distribution"] == "trunc_exp") {
          fits_row <- fits[sp, ]
          start <- lapply(
            as.list(fits_row[c("alpha", "ll", "ul", "lr", "ur")]),
            as.numeric
          )
        }
        fit <- fit_truncated_exponential(value, weight, start = start)
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

#' Extract feeding-kernel parameters from a mizer model
#'
#' Converts each species' feeding-kernel parameters in a mizer model to a fit
#' data frame compatible with the other mizerStomach functions.
#'
#' @param params A `MizerParams` object.
#' @param species_dict An optional named list or named character vector. Names
#'   are species names in `params` and values are their names in the returned
#'   fit data frame. Species not present among the names are left unchanged.
#'
#' @details
#' A mizer kernel is expressed at the prey-mass weighting exponent
#' \eqn{q_m = \lambda - 4/3}, where `lambda` is read from
#' `params@resource_params$lambda`. This function first reads the parameters at
#' \eqn{q_m} and then calls [transform_fit()] to return number-weighted fits
#' (`power = 0`). This makes `extract_fit()` the inverse of
#' [set_kernel_params()], apart from ordinary floating-point error.
#'
#' Kernel types are mapped as follows:
#'
#' * `pred_kernel_type = "lognormal"` becomes `distribution = "normal"`; its
#'   parameters are read from `beta` and `sigma`.
#' * `pred_kernel_type = "power_law"` becomes
#'   `distribution = "trunc_exp"`; its parameters are read from `kernel_exp`,
#'   `kernel_l_l`, `kernel_u_l`, `kernel_l_r`, and `kernel_u_r`.
#'
#' Other kernel types, including a Gaussian mixture, are not supported and
#' cause an error.
#'
#' @return A validated fit data frame with one row per model species,
#'   `power = 0`, `min_w_pred = 0`, and the family-specific parameters populated
#'   from `params`. Parameter columns that do not apply to a row contain `NA`.
#'
#' @examples
#' fits <- extract_fit(mizer::NS_params)
#' fits[c("Cod", "Herring"), c("species", "distribution", "mean", "sd")]
#'
#' @seealso [set_kernel_params()] for the reverse conversion and
#'   [transform_fit()] for the weighting transformation.
#' @export
extract_fit <- function(params, species_dict = NULL) {
  species_params <- params@species_params
  dist_map <- c(lognormal = "normal", power_law = "trunc_exp")
  distribution <- unname(dist_map[species_params$pred_kernel_type])
  if (anyNA(distribution)) {
    unknown <- unique(species_params$pred_kernel_type[is.na(distribution)])
    stop("Unknown pred_kernel_type: ", paste(unknown, collapse = ", "))
  }

  is_normal   <- distribution == "normal"
  is_trunc    <- distribution == "trunc_exp"

  species_names <- species_params$species
  if (!is.null(species_dict)) {
    idx <- match(species_names, names(species_dict))
    species_names <- ifelse(is.na(idx), species_names, unlist(species_dict)[idx])
  }

  fit <- data.frame(
    species      = species_names,
    distribution = distribution,
    power        = params@resource_params$lambda - 4/3,
    min_w_pred   = 0,
    mean         = ifelse(is_normal, log(species_params$beta),    NA_real_),
    sd           = ifelse(is_normal, species_params$sigma,        NA_real_),
    alpha        = ifelse(is_trunc,  species_params$kernel_exp,   NA_real_),
    ll           = ifelse(is_trunc,  species_params$kernel_l_l,   NA_real_),
    ul           = ifelse(is_trunc,  species_params$kernel_u_l,   NA_real_),
    lr           = ifelse(is_trunc,  species_params$kernel_l_r,   NA_real_),
    ur           = ifelse(is_trunc,  species_params$kernel_u_r,   NA_real_),
    stringsAsFactors = FALSE
  )
  rownames(fit) <- fit$species
  fit <- transform_fit(fit, power = 0)
  return(fit)
}

#' Set mizer feeding-kernel parameters from fitted stomach distributions
#'
#' Transforms fitted log-PPMR distributions to the weighting required by a
#' mizer model and writes the resulting parameters to the corresponding model
#' species.
#'
#' @param params A `MizerParams` object.
#' @param fit A fit data frame accepted by [validate_fit()], usually returned
#'   by [fit_log_ppmr()] or [extract_fit()]. It may contain any subset of model
#'   species, but every name in `fit$species` must occur in
#'   `params@species_params$species`.
#'
#' @details
#' Let \eqn{\lambda} be `params@resource_params$lambda`. Before setting model
#' parameters, each fit is transformed from its recorded `power` to
#' \eqn{\lambda - 4/3} with [transform_fit()]. This is the exponential tilt
#' that relates the distribution of prey in stomachs to mizer's feeding kernel
#' under the package's resource-spectrum and digestion assumptions.
#'
#' A normal fit selects mizer's `"lognormal"` kernel and sets
#' `beta = exp(mean)` and `sigma = sd`. A truncated-exponential fit selects the
#' `"power_law"` kernel and sets the five `kernel_*` parameters. Gaussian
#' mixtures cannot currently be represented by mizer kernel parameters and
#' cause an error.
#'
#' Only species occurring in `fit` are changed. The input object is not
#' modified in place; the updated object must be assigned from the return
#' value.
#'
#' @return The updated `MizerParams` object.
#'
#' @examples
#' params <- mizer::NS_params
#' cod_fit <- extract_fit(params)["Cod", , drop = FALSE]
#' cod_fit$mean <- cod_fit$mean + 0.1
#' params <- set_kernel_params(params, cod_fit)
#'
#' @seealso [extract_fit()] for the reverse conversion.
#' @export
set_kernel_params <- function(params, fit) {
  fit <- validate_fit(fit)
  sp <- params@species_params

  unknown <- setdiff(fit$species, sp$species)
  if (length(unknown) > 0) {
    stop("Species not found in params: ", paste(unknown, collapse = ", "))
  }

  # Transform fit to the power used internally by mizer
  mizer_power <- params@resource_params$lambda - 4/3
  fit <- transform_fit(fit, power = mizer_power)

  dist_map <- c(normal = "lognormal", trunc_exp = "power_law")

  for (i in seq_len(nrow(fit))) {
    row <- fit[i, ]
    s <- row$species
    dist <- row$distribution

    if (dist == "gauss_mix") {
      stop("gauss_mix distribution is not supported by mizer kernel parameters")
    }

    sp[sp$species == s, "pred_kernel_type"] <- dist_map[[dist]]

    if (dist == "normal") {
      sp[sp$species == s, "beta"]  <- exp(row$mean)
      sp[sp$species == s, "sigma"] <- row$sd
    } else if (dist == "trunc_exp") {
      if (!hasName(sp, "kernel_exp")) sp$kernel_exp <- NA_real_
      if (!hasName(sp, "kernel_l_l")) sp$kernel_l_l <- NA_real_
      if (!hasName(sp, "kernel_u_l")) sp$kernel_u_l <- NA_real_
      if (!hasName(sp, "kernel_l_r")) sp$kernel_l_r <- NA_real_
      if (!hasName(sp, "kernel_u_r")) sp$kernel_u_r <- NA_real_
      sp[sp$species == s, "kernel_exp"] <- row$alpha
      sp[sp$species == s, "kernel_l_l"] <- row$ll
      sp[sp$species == s, "kernel_u_l"] <- row$ul
      sp[sp$species == s, "kernel_l_r"] <- row$lr
      sp[sp$species == s, "kernel_u_r"] <- row$ur
    }
  }

  mizer::species_params(params) <- sp
  return(params)
}

#' Fit a normal distribution to weighted observations
#'
#' Estimates the parameters of a normal distribution by weighted maximum
#' likelihood.
#'
#' @param value Numeric vector containing at least ten observations.
#' @param weight Numeric vector, of the same length as `value`, containing
#'   non-negative frequency weights.
#'
#' @details
#' The fitted mean is `weighted.mean(value, weight)`. The fitted standard
#' deviation is
#' \deqn{\sqrt{\frac{\sum_i w_i(x_i-\bar{x}_w)^2}{\sum_i w_i}}.}
#' This is the maximum-likelihood rather than the unbiased estimate: there is no
#' finite-sample correction. Consequently, the result is invariant to a common
#' positive rescaling of all weights. Inputs are checked by the internal
#' weighted-observation validator.
#'
#' @return A list with numeric elements `mean` and `sd`.
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
#' Estimates a smoothly truncated exponential density by weighted maximum
#' likelihood.
#'
#' @param value Numeric vector containing at least ten observations. Values are
#'   normally log predator/prey mass ratios.
#' @param weight Numeric vector, of the same length as `value`, containing
#'   non-negative frequency weights.
#' @param start An optional named list with numeric elements `alpha`, `ll`,
#'   `ul`, `lr`, and `ur`, passed to [bbmle::mle2()] as starting values. See
#'   [dtexp()] for the parameterization.
#'
#' @details
#' The objective minimized by [bbmle::mle2()] is the weighted negative
#' log-likelihood
#' \deqn{-\sum_i w_i \log f(x_i; \alpha,l_l,u_l,l_r,u_r),}
#' where `f` is [dtexp()]. Weights therefore act as observation frequencies and
#' need not be integers.
#'
#' If `start` is `NULL`, the initial values are `alpha = 0.5`, `ll` equal to
#' `max(0.1, min(value))`, `lr = max(value)`, and `ul = ur = 20`. Optimizer
#' evaluations that violate `ll >= 0.9 * min(value)`, `lr >= ll`,
#' `alpha + ul >= 1.01`, or `ur - alpha >= 0.01`, or that yield an invalid
#' density, receive a large objective penalty. The optimizer is allowed up to
#' 10,000 iterations. Because this is a nonlinear five-parameter optimization,
#' convergence can depend on the starting values; [fit_log_ppmr()] can reuse a
#' previous truncated-exponential fit as `start`.
#'
#' @return A list with numeric elements `alpha`, `ll`, `ul`, `lr`, and `ur`.
#' @examples
#' \donttest{
#' cod_data <- validate_ppmr_data(barnes_data, species = "Atlantic cod")
#' fit_truncated_exponential(cod_data$log_ppmr, cod_data$n_prey)
#' }
#' @export
#' @keywords internal
fit_truncated_exponential <- function(value, weight, start = NULL) {
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
  if (is.null(start)) {
    start <- list(
      alpha = 0.5,
      ll = max(0.1, min(value)),
      lr = max(value),
      ul = 20,
      ur = 20
    )
  }
  fit <- mle2(loglik, start = start,
    control = list(maxit = 10000))
  return(as.list(fit@coef))
}

#' Fit a Gaussian mixture distribution to weighted observations
#'
#' Fits a finite mixture of normal densities using weighted expectation-
#' maximization (EM) updates.
#'
#' @param value Numeric vector containing at least ten observations.
#' @param weight Numeric vector, of the same length as `value`, containing
#'   non-negative frequency weights.
#' @param k Positive integer giving the number of mixture components.
#' @param max_iter Positive integer giving the maximum number of EM iterations.
#' @param tol Non-negative numeric convergence tolerance for the absolute change
#'   in the stored iteration criterion.
#'
#' @details
#' Mixing proportions are initialized to `1 / k`, component means are equally
#' spaced from `min(value)` to `max(value)`, and all component standard
#' deviations are initialized to the unweighted [stats::sd()] of `value`.
#'
#' In each E-step, posterior component responsibilities are computed from the
#' current normal densities. In the M-step, `weight * responsibility` is used
#' to update each component's mixing proportion, mean, and maximum-likelihood
#' standard deviation. Iteration stops when the absolute change in the stored
#' criterion is below `tol` or after `max_iter` iterations.
#'
#' The current implementation computes that criterion from the row sums of the
#' already normalized responsibility matrix. For ordinary finite inputs those
#' row sums are one, so the criterion is zero and the routine stops after its
#' second EM update (or after one update if `max_iter = 1`). `tol` therefore
#' does not currently provide a meaningful likelihood-convergence check.
#'
#' Mixture likelihoods can have local optima and degenerate solutions. This
#' routine makes one deterministic initialization and does not impose a lower
#' bound on component standard deviations, so the result should be checked
#' visually with [plot_log_ppmr_fit()].
#'
#' @return A list with numeric vectors `p`, `mean`, and `sd`, each of length
#'   `k`. `p` contains the mixing proportions and sums to one.
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
