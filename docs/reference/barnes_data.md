# Predator-prey body size data from Barnes et al.

A dataset of predator-prey body mass observations compiled by Barnes et
al. (2008), filtered to include only predator species with more than
1000 stomach content observations. Each row represents a single prey
item found in a predator's stomach.

## Usage

``` r
barnes_data
```

## Format

A tibble with 23,164 rows and 5 columns:

- species:

  Common name of the predator species (character).

- w_pred:

  Body mass of the predator in grams.

- w_prey:

  Body mass of the prey item in grams.

- n_prey:

  Number of prey items (always 1 in this dataset, since each row
  represents a single observation).

- log_ppmr:

  Logarithm of the predator/prey mass ratio, i.e., \\\log(w\_{pred} /
  w\_{prey})\\.

The dataset contains observations for the following 12 predator species:
Albacore, Antarctic fish, Atlantic bluefin tuna, Atlantic cod, Bigeye
tuna, Bluefish, European sea bass, Silver hake, Smooth dogfish, Spurdog
/ spiny dogfish, Winter skate, and Yellowfin tuna.

## Source

Barnes, C., Bethea, D.M., Brodeur, R.D., et al. (2008). Predator and
prey body sizes in marine food webs. *Ecology*, 89(3), 881.
[doi:10.1890/07-1551.1](https://doi.org/10.1890/07-1551.1)

## See also

[`validate_ppmr_data()`](https://gustavdelius.github.io/mizerStomach/reference/validate_ppmr_data.md)
for validating similar datasets,
[`vignette("mizerStomach", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/mizerStomach.md)
for an introduction, and
[`vignette("PPMR_distributions", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/PPMR_distributions.md)
for a diagnostic analysis using this data.

## Examples

``` r
head(barnes_data)
#> # A tibble: 6 × 5
#>   species               w_pred w_prey n_prey log_ppmr
#>   <chr>                  <dbl>  <dbl>  <dbl>    <dbl>
#> 1 Atlantic bluefin tuna  61666   3.51      1     9.77
#> 2 Atlantic bluefin tuna  61666   3.51      1     9.77
#> 3 Atlantic bluefin tuna  61666   3.51      1     9.77
#> 4 Atlantic bluefin tuna  61666   4.63      1     9.50
#> 5 Atlantic bluefin tuna  61666   4.63      1     9.50
#> 6 Atlantic bluefin tuna  61666   4.63      1     9.50
unique(barnes_data$species)
#>  [1] "Atlantic bluefin tuna"   "Atlantic cod"           
#>  [3] "Yellowfin tuna"          "Bigeye tuna"            
#>  [5] "Silver hake"             "Bluefish"               
#>  [7] "Albacore"                "Winter skate"           
#>  [9] "Spurdog / spiny dogfish" "Smooth dogfish"         
#> [11] "Antarctic fish"          "European sea bass"      
```
