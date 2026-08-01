# Validate and standardize a fit data frame

Checks that each row has the parameter columns required by its
distribution, adds standard metadata defaults, and sets row names from
species names.

## Usage

``` r
validate_fit(fit)
```

## Arguments

- fit:

  A data frame with one row per species, or a named list describing one
  fit for backwards compatibility.

## Value

A data frame with any missing metadata columns added and row names set
to `fit$species`. The input object itself is not modified.

## Details

Every fit needs `species` and `distribution`. Missing `power` and
`min_w_pred` columns are added with value zero. Required family-specific
columns are:

- `mean` and `sd` for `distribution = "normal"`;

- `alpha`, `ll`, `ul`, `lr`, and `ur` for `"trunc_exp"`;

- `mean`, `sd`, and `p` for `"gauss_mix"`.

Gaussian-mixture parameters are normally list-columns; within each row,
their three vectors must have equal lengths. A named list is converted
to a one-row data frame, with vector-valued elements retained as
list-columns.

This function validates structure, not statistical admissibility: it
does not, for example, check that standard deviations are positive,
mixture weights sum to one, or cutoff parameters define a stable
density.

## See also

Other validation functions:
[`validate_ppmr_data()`](https://gustavdelius.github.io/mizerStomach/reference/validate_ppmr_data.md),
[`validate_weighted_observations()`](https://gustavdelius.github.io/mizerStomach/reference/validate_weighted_observations.md)

## Examples

``` r
# Validate a fit data frame produced by fit_log_ppmr()
fit <- fit_log_ppmr(barnes_data, "Albacore", distribution = "normal")
validate_fit(fit)
#>              mean       sd  species distribution power min_w_pred
#> Albacore 9.546003 1.146738 Albacore       normal     0          0

# A named list is also accepted (backwards compatibility)
fit_list <- list(species = "test", distribution = "normal",
                 mean = 5, sd = 2)
validate_fit(fit_list)
#>      species distribution mean sd power min_w_pred
#> test    test       normal    5  2     0          0
```
