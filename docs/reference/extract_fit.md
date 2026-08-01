# Extract feeding-kernel parameters from a mizer model

Converts each species' feeding-kernel parameters in a mizer model to a
fit data frame compatible with the other mizerStomach functions.

## Usage

``` r
extract_fit(params, species_dict = NULL)
```

## Arguments

- params:

  A `MizerParams` object.

- species_dict:

  An optional named list or named character vector. Names are species
  names in `params` and values are their names in the returned fit data
  frame. Species not present among the names are left unchanged.

## Value

A validated fit data frame with one row per model species, `power = 0`,
`min_w_pred = 0`, and the family-specific parameters populated from
`params`. Parameter columns that do not apply to a row contain `NA`.

## Details

A mizer kernel is expressed at the prey-mass weighting exponent \\q_m =
\lambda - 4/3\\, where `lambda` is read from
`params@resource_params$lambda`. This function first reads the
parameters at \\q_m\\ and then calls
[`transform_fit()`](https://gustavdelius.github.io/mizerStomach/reference/transform_fit.md)
to return number-weighted fits (`power = 0`). This makes `extract_fit()`
the inverse of
[`set_kernel_params()`](https://gustavdelius.github.io/mizerStomach/reference/set_kernel_params.md),
apart from ordinary floating-point error.

Kernel types are mapped as follows:

- `pred_kernel_type = "lognormal"` becomes `distribution = "normal"`;
  its parameters are read from `beta` and `sigma`.

- `pred_kernel_type = "power_law"` becomes `distribution = "trunc_exp"`;
  its parameters are read from `kernel_exp`, `kernel_l_l`, `kernel_u_l`,
  `kernel_l_r`, and `kernel_u_r`.

Other kernel types, including a Gaussian mixture, are not supported and
cause an error.

## See also

[`set_kernel_params()`](https://gustavdelius.github.io/mizerStomach/reference/set_kernel_params.md)
for the reverse conversion and
[`transform_fit()`](https://gustavdelius.github.io/mizerStomach/reference/transform_fit.md)
for the weighting transformation. The derivation and a worked round trip
are in
[`vignette("feeding_kernels", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/feeding_kernels.md).

## Examples

``` r
fits <- extract_fit(mizer::NS_params)
fits[c("Cod", "Herring"), c("species", "distribution", "mean", "sd")]
#>         species distribution      mean  sd
#> Cod         Cod       normal  5.541655 1.3
#> Herring Herring       normal 20.736472 3.2
```
