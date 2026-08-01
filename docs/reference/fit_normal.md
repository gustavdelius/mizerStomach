# Fit a normal distribution to weighted observations

Estimates the parameters of a normal distribution by weighted maximum
likelihood.

## Usage

``` r
fit_normal(value, weight)
```

## Arguments

- value:

  Numeric vector containing at least ten observations.

- weight:

  Numeric vector, of the same length as `value`, containing non-negative
  frequency weights.

## Value

A list with numeric elements `mean` and `sd`.

## Details

The fitted mean is `weighted.mean(value, weight)`. The fitted standard
deviation is \$\$\sqrt{\frac{\sum_i w_i(x_i-\bar{x}\_w)^2}{\sum_i
w_i}}.\$\$ This is the maximum-likelihood rather than the unbiased
estimate: there is no finite-sample correction. Consequently, the result
is invariant to a common positive rescaling of all weights. Inputs are
checked by the internal weighted-observation validator.

## Examples

``` r
cod_data <- validate_ppmr_data(barnes_data, species = "Atlantic cod")
fit_normal(cod_data$log_ppmr, cod_data$n_prey)
#> $mean
#> [1] 5.725514
#> 
#> $sd
#> [1] 2.600442
#> 
```
