# Re-express a fit at a different prey-mass weighting

Applies family-specific parameter changes to fitted log-PPMR
distributions when observations are reweighted by another power of prey
mass.

## Usage

``` r
transform_fit(fit, power)
```

## Arguments

- fit:

  A fit data frame with one or more rows, accepted by
  [`validate_fit()`](https://gustavdelius.github.io/mizerStomach/reference/validate_fit.md).
  Each row's current weighting exponent is read from its `power` column;
  a missing column is interpreted as zero.

- power:

  Single numeric value giving the target exponent of `w_prey` in the
  observation weights.

## Value

A validated fit data frame with transformed family parameters and
`power` set to the requested value.

## Details

If a fit was obtained with weights \\n\_{prey}w\_{prey}^{q_0}\\ and the
requested exponent is \\q_1\\, let \\\Delta q=q_1-q_0\\. For
\\l=\log(w\_{pred}/w\_{prey})\\, changing the exponent exponentially
tilts the fitted density by a factor proportional to \\\exp(-\Delta
q\\l)\\. For the normal and truncated-exponential families, this tilt
remains in the same family and is exact:

- a normal mean changes from \\\mu\\ to \\\mu-\Delta q\\\sigma^2\\,
  while `sd` is unchanged;

- a truncated-exponential `alpha` changes to `alpha - delta_q`, while
  its cutoff parameters are unchanged.

For a Gaussian mixture, every component mean and the component
proportions are updated by
[`transform_gaussian_mixture()`](https://gustavdelius.github.io/mizerStomach/reference/transform_gaussian_mixture.md),
while component standard deviations are unchanged.

Each row is transformed using its own existing `power`, after which all
rows have `power` set to the requested value. The input data frame is
not modified. This transformation assumes that the fitted log-PPMR
distribution can be represented independently of predator size; it is
not a refit and does not inspect the original observations.

## See also

[`fit_log_ppmr()`](https://gustavdelius.github.io/mizerStomach/reference/fit_log_ppmr.md)
for fitting at a chosen power and
[`vignette("density_functions", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/density_functions.md)
for weighting interpretations and derivations. The kernel-specific
target power is derived in
[`vignette("feeding_kernels", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/feeding_kernels.md).

## Examples

``` r
# Fit a normal distribution and transform to biomass weighting
fit <- fit_log_ppmr(barnes_data, "Albacore", distribution = "normal")
fit_biomass <- transform_fit(fit, power = 1)
fit_biomass
#>              mean       sd  species distribution power min_w_pred
#> Albacore 8.230995 1.146738 Albacore       normal     1          0

# Transform multiple species at once
fit2 <- fit_log_ppmr(barnes_data,
                     c("Albacore", "Atlantic cod"),
                     distribution = "normal")
transform_fit(fit2, power = 1)
#>                   mean       sd      species distribution power min_w_pred
#> Albacore      8.230995 1.146738     Albacore       normal     1          0
#> Atlantic cod -1.036782 2.600442 Atlantic cod       normal     1          0
```
