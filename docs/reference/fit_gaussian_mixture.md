# Fit a Gaussian mixture distribution to weighted observations

Fits a finite mixture of normal densities using weighted expectation-
maximization (EM) updates.

## Usage

``` r
fit_gaussian_mixture(value, weight, k = 2, max_iter = 100, tol = 1e-06)
```

## Arguments

- value:

  Numeric vector containing at least ten observations.

- weight:

  Numeric vector, of the same length as `value`, containing non-negative
  frequency weights.

- k:

  Positive integer giving the number of mixture components.

- max_iter:

  Positive integer giving the maximum number of EM iterations.

- tol:

  Non-negative numeric convergence tolerance for the absolute change in
  weighted log-likelihood between EM iterations.

## Value

A list with numeric vectors `p`, `mean`, and `sd`, each of length `k`.
`p` contains the mixing proportions and sums to one.

## Details

Mixing proportions are initialized to `1 / k`, component means are
equally spaced from `min(value)` to `max(value)`, and all component
standard deviations are initialized to the unweighted
[`stats::sd()`](https://rdrr.io/r/stats/sd.html) of `value`.

In each E-step, posterior component responsibilities are computed from
the current normal densities. In the M-step, `weight * responsibility`
is used to update each component's mixing proportion, mean, and
maximum-likelihood standard deviation. The weighted log-likelihood is
evaluated from the mixture density before responsibilities are
normalized. Iteration stops when its absolute change is below `tol` or
after `max_iter` iterations.

Mixture likelihoods can have local optima and degenerate solutions. This
routine makes one deterministic initialization and does not impose a
lower bound on component standard deviations, so the result should be
checked visually with
[`plot_log_ppmr_fit()`](https://gustavdelius.github.io/mizerStomach/reference/plot_log_ppmr_fit.md).

## See also

[`vignette("density_functions", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/density_functions.md)
for the Gaussian-mixture weighting transformation and
[`vignette("PPMR_distributions", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/PPMR_distributions.md)
for diagnostic guidance.

## Examples

``` r
cod_data <- validate_ppmr_data(barnes_data, species = "Atlantic cod")
fit_gaussian_mixture(cod_data$log_ppmr, cod_data$n_prey)
#> $p
#> [1] 0.4139351 0.5860649
#> 
#> $mean
#> [1] 3.702259 7.154531
#> 
#> $sd
#> [1] 0.9841034 2.4333321
#> 
```
