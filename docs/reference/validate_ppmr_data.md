# Validate predator/prey mass-ratio observations

Checks the columns required by the fitting and plotting functions,
optionally selects one predator species, and supplies log predator/prey
mass ratios.

## Usage

``` r
validate_ppmr_data(ppmr_data, species = NULL)
```

## Arguments

- ppmr_data:

  A data frame containing at least `species`, `w_pred`, `w_prey`, and
  `n_prey`. Predator and prey masses must use the same mass unit;
  package examples use grams. Extra columns are preserved.

- species:

  Optional single character string. If supplied, only rows for that
  predator species are returned; an absent species causes an error.

## Value

The input data frame, optionally filtered by `species`, with a
`log_ppmr` column guaranteed to be present. Column classes and extra
columns are otherwise retained.

## Details

The calculated value is
\$\$\mathrm{log\\ppmr}=\log(w\_{pred})-\log(w\_{prey})
=\log(w\_{pred}/w\_{prey}).\$\$ If `log_ppmr` is absent, this column is
appended. If it already exists, it is checked against the calculation
and retained; a disagreement produces a warning. A warning is also
issued when prey are heavier than their predator, which produces a
negative log PPMR. Missing masses that make the calculated ratio `NA`
cause an error.

This is a format validator rather than a full data-cleaning routine. In
particular, callers should ensure that masses are finite and positive
and that `n_prey` contains meaningful non-negative counts or frequencies
before fitting.

## See also

[`vignette("mizerStomach", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/mizerStomach.md)
for the input workflow and
[`vignette("PPMR_distributions", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/PPMR_distributions.md)
for scientific data-quality considerations beyond these structural
checks.

Other validation functions:
[`validate_fit()`](https://gustavdelius.github.io/mizerStomach/reference/validate_fit.md),
[`validate_weighted_observations()`](https://gustavdelius.github.io/mizerStomach/reference/validate_weighted_observations.md)

## Examples

``` r
# Validate the barnes_data dataset
valid_data <- validate_ppmr_data(barnes_data)
head(valid_data)
#> # A tibble: 6 × 5
#>   species               w_pred w_prey n_prey log_ppmr
#>   <chr>                  <dbl>  <dbl>  <dbl>    <dbl>
#> 1 Atlantic bluefin tuna  61666   3.51      1     9.77
#> 2 Atlantic bluefin tuna  61666   3.51      1     9.77
#> 3 Atlantic bluefin tuna  61666   3.51      1     9.77
#> 4 Atlantic bluefin tuna  61666   4.63      1     9.50
#> 5 Atlantic bluefin tuna  61666   4.63      1     9.50
#> 6 Atlantic bluefin tuna  61666   4.63      1     9.50

# Validate and filter to a single species
cod_data <- validate_ppmr_data(barnes_data, species = "Atlantic cod")
nrow(cod_data)
#> [1] 2518
```
