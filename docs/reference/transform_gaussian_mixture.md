# Transform the weighting of a Gaussian mixture distribution

Transform the weighting of a Gaussian mixture distribution

## Usage

``` r
transform_gaussian_mixture(fit, power = 1)
```

## Arguments

- fit:

  A named list with numeric vectors `mean`, `sd`, and `p`, one value per
  mixture component. The component proportions in `p` should sum to one.

- power:

  Numeric exponent by which prey-mass weighting is increased.

## Value

`fit` with transformed `mean` and normalized `p`; `sd` is unchanged.

## Details

Let \\q\\ be `power`. Each component mean changes from \\\mu_j\\ to
\\\tilde\mu_j=\mu_j-q\sigma_j^2\\. Standard deviations are retained and
component proportions are set proportional to
\\p_j\exp(-q\mu_j+q^2\sigma_j^2/2)\\, then normalized to sum to one.
These are the exact parameters obtained by multiplying the mixture
density by \\\exp(-q l)\\ and renormalizing. Unlike
[`transform_fit()`](https://gustavdelius.github.io/mizerStomach/reference/transform_fit.md),
this low-level helper interprets `power` as the change in exponent and
does no validation.

## Examples

``` r
fit <- list(p = c(0.3, 0.7), mean = c(3, 7), sd = c(1, 2))
transform_gaussian_mixture(fit, power = 1)
#> $p
#> [1] 0.8392559 0.1607441
#> 
#> $mean
#> [1] 2 3
#> 
#> $sd
#> [1] 1 2
#> 
```
