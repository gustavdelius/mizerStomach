# Validate weighted observations

Checks the basic shape and type requirements shared by the low-level
fitting functions.

## Usage

``` r
validate_weighted_observations(value, weight)
```

## Arguments

- value:

  Vector of observed values.

- weight:

  Vector of observation weights.

## Value

`TRUE`. An informative error is raised when a check fails.

## Details

The function requires at least ten entries in `value`, equal vector
lengths, numeric values and weights, and no negative weights. The
ten-entry condition concerns rows rather than the sum of frequency
weights. Distribution- specific conditions are left to the fitting
routines.

## See also

Other validation functions:
[`validate_fit()`](https://gustavdelius.github.io/mizerStomach/reference/validate_fit.md),
[`validate_ppmr_data()`](https://gustavdelius.github.io/mizerStomach/reference/validate_ppmr_data.md)
