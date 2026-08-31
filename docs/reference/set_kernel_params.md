# Set mizer feeding-kernel parameters from fitted stomach distributions

Transforms fitted log-PPMR distributions to the weighting required by a
mizer model and writes the resulting parameters to the corresponding
model species.

## Usage

``` r
set_kernel_params(params, fit)
```

## Arguments

- params:

  A `MizerParams` object.

- fit:

  A fit data frame accepted by
  [`validate_fit()`](https://gustavdelius.github.io/mizerStomach/reference/validate_fit.md),
  usually returned by
  [`fit_log_ppmr()`](https://gustavdelius.github.io/mizerStomach/reference/fit_log_ppmr.md)
  or
  [`extract_fit()`](https://gustavdelius.github.io/mizerStomach/reference/extract_fit.md).
  It may contain any subset of model species, but every name in
  `fit$species` must occur in
  [`mizer::species_params()`](https://sizespectrum.org/mizer/reference/species_params.html)`$species`.

## Value

The updated `MizerParams` object.

## Details

Let \\\lambda\\ be
[`mizer::resource_params()`](https://sizespectrum.org/mizer/reference/resource_params.html)`$lambda`.
Before setting model parameters, each fit is transformed from its
recorded `power` to \\\lambda - 4/3\\ with
[`transform_fit()`](https://gustavdelius.github.io/mizerStomach/reference/transform_fit.md).
This is the exponential tilt that relates the distribution of prey in
stomachs to mizer's feeding kernel under the package's resource-spectrum
and digestion assumptions.

A normal fit selects mizer's `"lognormal"` kernel and sets
`beta = exp(mean)` and `sigma = sd`. A truncated-exponential fit selects
the `"power_law"` kernel and sets the five scalar `kernel_*` parameters.
A Gaussian-mixture fit selects the `"gaussian_mixture"` kernel and
stores its component vectors in the list-columns `kernel_p`,
`kernel_mean`, and `kernel_sd`.

Only species occurring in `fit` are changed. The input object is not
modified in place; the updated object must be assigned from the return
value.

## See also

[`extract_fit()`](https://gustavdelius.github.io/mizerStomach/reference/extract_fit.md)
for the reverse conversion and
[`vignette("feeding_kernels", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/feeding_kernels.md)
for the assumptions, derivation, and a worked example.

## Examples

``` r
params <- mizer::NS_params
cod_fit <- extract_fit(params)["Cod", , drop = FALSE]
cod_fit$mean <- cod_fit$mean + 0.1
params <- set_kernel_params(params, cod_fit)
```
