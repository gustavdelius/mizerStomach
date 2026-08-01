# Evaluate one fitted density

Internal dispatcher used by
[`get_density()`](https://gustavdelius.github.io/mizerStomach/reference/get_density.md).

## Usage

``` r
get_density_single(x, fit)
```

## Arguments

- x:

  Numeric vector of log-PPMR values.

- fit:

  Named list containing `distribution` and the parameters required by
  that family for one species.

## Value

A numeric vector with the same length as `x`.
