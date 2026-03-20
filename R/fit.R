#' Fit a distribution to log ppmr observations
#'
#' @param ppmr_data A data frame with log ppmr observations. See
#'   [validate_ppmr_data()] for details.
#' @param species A character vector with one or more species names. Ignored
#'   if `fits` is provided.
#' @param distribution The distribution to fit. One of "normal",
#'   "trunc_exp" or "gauss_mix".
#' @param min_w_pred The minimum predator weight to include. Default is 0.
#' @param power Each observation is weighted by a power of the prey weight. The
#'   default is 0 which means that each prey individual contributes equally.
#'   `power = 1` means each prey individual contributes in proportion to its
#'   biomass.
#' @param fits An optional fit data frame (as returned by [fit_log_ppmr()]).
#'   When provided, the species are taken from `fits` instead of the `species`
#'   argument. For `distribution = "trunc_exp"`, the fitted parameters in
#'   `fits` are used as starting values for the `mle2()` optimisation (only
#'   when the corresponding row in `fits` also has `distribution = "trunc_exp"`).
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

#' Extract fit parameters from mizer model
#'
#' Extracts distribution parameters from a mizer params object
#' and returns a `fits` data frame compatible with [fit_log_ppmr()].
#'
#' The `pred_kernel_type` column is mapped as: `"lognormal"` → `"normal"`,
#' `"power_law"` → `"trunc_exp"`. Gaussian mixture distribution is not yet
#' supported
#'
#' @param params A MizerParams object
#' @param species_dict A named list (or named character vector) mapping species
#'   names in `params@species_params$species` to the names they should have in
#'   the returned fit data frame. Only species listed as names in
#'   `species_dict` are renamed; others keep their original names.
#' @return A fit data frame with `power = 2/3` and distribution parameters
#'   populated from `species_params`. Only the parameters relevant to each
#'   species' distribution are filled; others are `NA`.
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

#' Set kernel parameters in a mizer model from a fit data frame
#'
#' Sets the predation kernel parameters in a MizerParams object from a `fits`
#' data frame as returned by [fit_log_ppmr()]. This is the inverse of
#' [extract_fit()].
#'
#' The `distribution` column is mapped as: `"normal"` → `pred_kernel_type =
#' "lognormal"` (sets `beta = exp(mean)`, `sigma = sd`), `"trunc_exp"` →
#' `pred_kernel_type = "power_law"` (sets `kernel_exp`, `kernel_l_l`,
#' `kernel_u_l`, `kernel_l_r`, `kernel_u_r`). Gaussian mixture distribution is
#' not supported.
#'
#' @param params A MizerParams object
#' @param fit A fit data frame (as returned by [fit_log_ppmr()] or
#'   [extract_fit()]). May contain one or more species. All species in `fit`
#'   must be present in `params`.
#' @return The updated MizerParams object.
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
#' @param start An optional named list of starting values for `mle2()` with
#'   elements `alpha`, `ll`, `ul`, `lr`, `ur`. If `NULL`, defaults are derived
#'   from the data.
#' @return A list with the fitted parameters `alpha`, `ll`, `ul`, `lr`, `ur`
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
