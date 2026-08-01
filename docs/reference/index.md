# Package index

## Fit and transform distributions

- [`fit_log_ppmr()`](https://gustavdelius.github.io/mizerStomach/reference/fit_log_ppmr.md)
  : Fit distributions to log predator/prey mass ratios
- [`transform_fit()`](https://gustavdelius.github.io/mizerStomach/reference/transform_fit.md)
  : Re-express a fit at a different prey-mass weighting
- [`get_density()`](https://gustavdelius.github.io/mizerStomach/reference/get_density.md)
  : Evaluate fitted log-PPMR densities

## Diagnose and tune fits

- [`plot_log_ppmr_fit()`](https://gustavdelius.github.io/mizerStomach/reference/plot_log_ppmr_fit.md)
  : Compare observed and fitted log-PPMR distributions
- [`plot_ppmr_violins()`](https://gustavdelius.github.io/mizerStomach/reference/plot_ppmr_violins.md)
  : Plot log-PPMR distributions across predator-size classes
- [`fit_shiny()`](https://gustavdelius.github.io/mizerStomach/reference/fit_shiny.md)
  : Interactively tune PPMR distribution fits

## Use fits in mizer

- [`set_kernel_params()`](https://gustavdelius.github.io/mizerStomach/reference/set_kernel_params.md)
  : Set mizer feeding-kernel parameters from fitted stomach
  distributions
- [`extract_fit()`](https://gustavdelius.github.io/mizerStomach/reference/extract_fit.md)
  : Extract feeding-kernel parameters from a mizer model

## Data

- [`barnes_data`](https://gustavdelius.github.io/mizerStomach/reference/barnes_data.md)
  : Predator-prey body size data from Barnes et al.

## Validation

- [`validate_fit()`](https://gustavdelius.github.io/mizerStomach/reference/validate_fit.md)
  : Validate and standardize a fit data frame
- [`validate_ppmr_data()`](https://gustavdelius.github.io/mizerStomach/reference/validate_ppmr_data.md)
  : Validate predator/prey mass-ratio observations

## Low-level functions

- [`dtexp()`](https://gustavdelius.github.io/mizerStomach/reference/dtexp.md)
  : Smoothly truncated exponential density
- [`fit_gaussian_mixture()`](https://gustavdelius.github.io/mizerStomach/reference/fit_gaussian_mixture.md)
  : Fit a Gaussian mixture distribution to weighted observations
- [`fit_normal()`](https://gustavdelius.github.io/mizerStomach/reference/fit_normal.md)
  : Fit a normal distribution to weighted observations
- [`fit_truncated_exponential()`](https://gustavdelius.github.io/mizerStomach/reference/fit_truncated_exponential.md)
  : Fit a truncated exponential distribution to weighted observations
- [`get_density_single()`](https://gustavdelius.github.io/mizerStomach/reference/get_density_single.md)
  : Evaluate one fitted density
- [`mizerStomach`](https://gustavdelius.github.io/mizerStomach/reference/mizerStomach-package.md)
  [`mizerStomach-package`](https://gustavdelius.github.io/mizerStomach/reference/mizerStomach-package.md)
  : mizerStomach: Using stomach content data in mizer
- [`transform_gaussian_mixture()`](https://gustavdelius.github.io/mizerStomach/reference/transform_gaussian_mixture.md)
  : Transform the weighting of a Gaussian mixture distribution
- [`transform_normal()`](https://gustavdelius.github.io/mizerStomach/reference/transform_normal.md)
  : Exponentially tilt a normal distribution
- [`transform_truncated_exp()`](https://gustavdelius.github.io/mizerStomach/reference/transform_truncated_exp.md)
  : Exponentially tilt a truncated exponential distribution
- [`validate_weighted_observations()`](https://gustavdelius.github.io/mizerStomach/reference/validate_weighted_observations.md)
  : Validate weighted observations
