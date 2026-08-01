# Fit distributions to log predator/prey mass ratios

Fits one distribution to the log predator/prey mass-ratio observations
for each requested predator species. Rows can represent groups of
identical observations through `n_prey`, and can additionally be
weighted by prey mass.

## Usage

``` r
fit_log_ppmr(
  ppmr_data,
  species,
  distribution = c("normal", "trunc_exp", "gauss_mix"),
  min_w_pred = 0,
  power = 0,
  fits = NULL
)
```

## Arguments

- ppmr_data:

  A data frame accepted by
  [`validate_ppmr_data()`](https://gustavdelius.github.io/mizerStomach/reference/validate_ppmr_data.md).
  It must contain `species`, `w_pred`, `w_prey`, and `n_prey` columns;
  `log_ppmr` is calculated when it is absent.

- species:

  A character vector naming one or more predator species in `ppmr_data`.
  Ignored when `fits` is supplied.

- distribution:

  Character string selecting the fitted family. One of `"normal"`,
  `"trunc_exp"`, or `"gauss_mix"`.

- min_w_pred:

  Single numeric value. Observations with predator mass
  `w_pred < min_w_pred` are excluded before fitting. The same threshold
  is applied to every requested species and is stored in the result.

- power:

  Single numeric value giving the exponent of prey mass in the
  observation weights. Observation \\i\\ receives weight \\n_i w_i^q\\,
  where \\n_i\\ is `n_prey`, \\w_i\\ is `w_prey`, and \\q\\ is `power`.
  Thus `power = 0` fits the prey-number distribution and `power = 1`
  fits the prey-biomass distribution.

- fits:

  An optional fit data frame accepted by
  [`validate_fit()`](https://gustavdelius.github.io/mizerStomach/reference/validate_fit.md).
  When supplied, its `species` column determines which species are
  fitted. For a `"trunc_exp"` refit, a row already using that family
  also supplies the starting values `alpha`, `ll`, `ul`, `lr`, and `ur`;
  other families do not use values from `fits` as starting values.

## Value

A data frame with one row per species and row names equal to `species`.
Every row contains `species`, `distribution`, `power`, and `min_w_pred`,
plus family-specific parameters: `mean` and `sd` for a normal fit;
`alpha`, `ll`, `ul`, `lr`, and `ur` for a truncated exponential fit; or
list-columns `p`, `mean`, and `sd` for a Gaussian mixture. See
[`validate_fit()`](https://gustavdelius.github.io/mizerStomach/reference/validate_fit.md).

## Details

For each species, the function first validates the input, applies
`min_w_pred`, uses the validated `log_ppmr` column as the response, and
sets the frequency weight to \\n\_{prey}w\_{prey}^{q}\\. Multiplying
every weight by the same positive constant does not change any of the
fits.

The fitting method depends on `distribution`:

- `"normal"` uses the weighted mean and the maximum-likelihood weighted
  standard deviation (the variance divisor is the sum of the weights,
  with no small-sample correction); see
  [`fit_normal()`](https://gustavdelius.github.io/mizerStomach/reference/fit_normal.md).

- `"trunc_exp"` minimizes the weighted negative log-likelihood with
  [`bbmle::mle2()`](https://rdrr.io/pkg/bbmle/man/mle2.html). The
  density is an exponential curve with smooth logistic cutoffs at both
  ends; see
  [`fit_truncated_exponential()`](https://gustavdelius.github.io/mizerStomach/reference/fit_truncated_exponential.md)
  and
  [`dtexp()`](https://gustavdelius.github.io/mizerStomach/reference/dtexp.md).

- `"gauss_mix"` uses
  [`fit_gaussian_mixture()`](https://gustavdelius.github.io/mizerStomach/reference/fit_gaussian_mixture.md)
  with two normal components, equal initial mixing proportions, and at
  most 100 EM iterations.

Fits are made independently by species; there is no pooling of
parameters. The returned `power` records which weighted distribution was
fitted. Use
[`transform_fit()`](https://gustavdelius.github.io/mizerStomach/reference/transform_fit.md)
to express the fitted family at another prey-mass weighting without
refitting the observations.

## See also

[`plot_log_ppmr_fit()`](https://gustavdelius.github.io/mizerStomach/reference/plot_log_ppmr_fit.md)
to compare a fit with the observations,
[`transform_fit()`](https://gustavdelius.github.io/mizerStomach/reference/transform_fit.md)
to change its weighting, and
[`set_kernel_params()`](https://gustavdelius.github.io/mizerStomach/reference/set_kernel_params.md)
to transfer supported fits to a mizer model. See
[`vignette("mizerStomach", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/mizerStomach.md)
for the basic workflow and
[`vignette("PPMR_distributions", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/PPMR_distributions.md)
for family selection and diagnostics.

## Examples

``` r
# Fit a normal distribution to one species
fit <- fit_log_ppmr(barnes_data, "Albacore", distribution = "normal")
fit
#>              mean       sd  species distribution power min_w_pred
#> Albacore 9.546003 1.146738 Albacore       normal     0          0

# Fit multiple species at once
fit <- fit_log_ppmr(barnes_data,
                    c("Albacore", "Atlantic cod"),
                    distribution = "normal")
fit
#>                  mean       sd      species distribution power min_w_pred
#> Albacore     9.546003 1.146738     Albacore       normal     0          0
#> Atlantic cod 5.725514 2.600442 Atlantic cod       normal     0          0

# \donttest{
# Fit a truncated exponential distribution
fit_te <- fit_log_ppmr(barnes_data, "Albacore", distribution = "trunc_exp")
fit_te
#>             alpha       ll       ul       lr       ur  species distribution
#> Albacore 1.520876 3.725438 22.40173 9.694653 3.385282 Albacore    trunc_exp
#>          power min_w_pred
#> Albacore     0          0

# Refit using previous fit as starting point
fit_te2 <- fit_log_ppmr(barnes_data, distribution = "trunc_exp",
                         fits = fit_te)
fit_te2
#>             alpha       ll       ul       lr       ur  species distribution
#> Albacore 1.520901 3.725408 22.40173 9.694679 3.385272 Albacore    trunc_exp
#>          power min_w_pred
#> Albacore     0          0
# }

# Fit a Gaussian mixture distribution
fit_gm <- fit_log_ppmr(barnes_data, "Albacore", distribution = "gauss_mix")
fit_gm
#>                     p         mean           sd  species distribution power
#> Albacore 0.044998.... 5.799645.... 0.744409.... Albacore    gauss_mix     0
#>          min_w_pred
#> Albacore          0
```
