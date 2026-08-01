# Plot log-PPMR distributions across predator-size classes

Uses violin plots to inspect whether the log predator/prey mass-ratio
distribution changes with predator size.

## Usage

``` r
plot_ppmr_violins(ppmr_data, species, power = 1)
```

## Arguments

- ppmr_data:

  A data frame accepted by
  [`validate_ppmr_data()`](https://gustavdelius.github.io/mizerStomach/reference/validate_ppmr_data.md).

- species:

  Single character string naming the predator species to plot.

- power:

  Numeric prey-mass weighting exponent. Each observation receives weight
  `n_prey * w_prey^power`; zero gives a number distribution and one a
  biomass distribution.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Details

Observations are split into ten predator-mass classes with approximately
equal numbers of data-frame rows using
[`ggplot2::cut_number()`](https://ggplot2.tidyverse.org/reference/cut_interval.html).
Predator masses are multiplied by tiny random perturbations before
binning so that repeated, rounded masses do not prevent the quantile
cut. Consequently, bin boundaries can vary slightly between calls; use
[`base::set.seed()`](https://rdrr.io/r/base/Random.html) before the
function when exact reproducibility is important.

Within each class,
[`ggplot2::geom_violin()`](https://ggplot2.tidyverse.org/reference/geom_violin.html)
estimates the weighted log-PPMR density and marks its median. The bins
balance rows, not `n_prey` or the derived weights, so strongly
aggregated data can still have uneven effective sample sizes across
classes.

## See also

[`plot_log_ppmr_fit()`](https://gustavdelius.github.io/mizerStomach/reference/plot_log_ppmr_fit.md)
for fitted marginal distributions and
[`fit_log_ppmr()`](https://gustavdelius.github.io/mizerStomach/reference/fit_log_ppmr.md)
for fitting after the predator-size assumption has been assessed. Worked
examples are in
[`vignette("PPMR_distributions", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/PPMR_distributions.md).

## Examples

``` r
# Biomass-weighted violin plot
plot_ppmr_violins(barnes_data, "Albacore")
#> Warning: `quantiles` for weighted data is not implemented.
#> Warning: `quantiles` for weighted data is not implemented.
#> Warning: `quantiles` for weighted data is not implemented.
#> Warning: `quantiles` for weighted data is not implemented.
#> Warning: `quantiles` for weighted data is not implemented.
#> Warning: `quantiles` for weighted data is not implemented.
#> Warning: `quantiles` for weighted data is not implemented.
#> Warning: `quantiles` for weighted data is not implemented.
#> Warning: `quantiles` for weighted data is not implemented.
#> Warning: `quantiles` for weighted data is not implemented.


# Number-weighted violin plot
plot_ppmr_violins(barnes_data, "Albacore", power = 0)
#> Warning: `quantiles` for weighted data is not implemented.
#> Warning: `quantiles` for weighted data is not implemented.
#> Warning: `quantiles` for weighted data is not implemented.
#> Warning: `quantiles` for weighted data is not implemented.
#> Warning: `quantiles` for weighted data is not implemented.
#> Warning: `quantiles` for weighted data is not implemented.
#> Warning: `quantiles` for weighted data is not implemented.
#> Warning: `quantiles` for weighted data is not implemented.
#> Warning: `quantiles` for weighted data is not implemented.
#> Warning: `quantiles` for weighted data is not implemented.
```
