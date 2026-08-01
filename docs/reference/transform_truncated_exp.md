# Exponentially tilt a truncated exponential distribution

Exponentially tilt a truncated exponential distribution

## Usage

``` r
transform_truncated_exp(fit, power = 1)
```

## Arguments

- fit:

  A named list with numeric elements `alpha`, `ll`, `ul`, `lr`, and
  `ur`; see
  [`dtexp()`](https://gustavdelius.github.io/mizerStomach/reference/dtexp.md).

- power:

  Numeric exponent by which prey-mass weighting is increased.

## Value

`fit` with its `alpha` element replaced by the transformed value.

## Details

Multiplication by \\\exp(-q l)\\ changes the exponential slope from
`alpha` to `alpha - power`; the two cutoff locations and steepnesses are
unchanged. Unlike
[`transform_fit()`](https://gustavdelius.github.io/mizerStomach/reference/transform_fit.md),
this low-level helper interprets `power` as the change in exponent and
does no validation.

## Examples

``` r
fit <- list(alpha = 0.5, ll = 2, ul = 20, lr = 12, ur = 20)
transform_truncated_exp(fit, power = 1)
#> $alpha
#> [1] -0.5
#> 
#> $ll
#> [1] 2
#> 
#> $ul
#> [1] 20
#> 
#> $lr
#> [1] 12
#> 
#> $ur
#> [1] 20
#> 
```
