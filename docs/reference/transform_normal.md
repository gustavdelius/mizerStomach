# Exponentially tilt a normal distribution

Exponentially tilt a normal distribution

## Usage

``` r
transform_normal(fit, power = 1)
```

## Arguments

- fit:

  A named list with numeric elements `mean` and `sd`.

- power:

  Numeric exponent by which prey-mass weighting is increased.

## Value

`fit` with its `mean` replaced by the transformed mean.

## Details

Multiplying a normal density in log PPMR \\l\\ by \\\exp(-q l)\\ and
renormalizing gives another normal density. Its mean is
`mean - power * sd^2` and its standard deviation is unchanged. Unlike
[`transform_fit()`](https://gustavdelius.github.io/mizerStomach/reference/transform_fit.md),
this low-level helper interprets `power` as the change in exponent, not
as an absolute target, and it does no validation.

## Examples

``` r
fit <- list(mean = 5.0, sd = 2.0)
transform_normal(fit, power = 1)
#> $mean
#> [1] 1
#> 
#> $sd
#> [1] 2
#> 
```
