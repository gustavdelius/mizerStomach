# Fit a truncated exponential distribution to weighted observations

Estimates a smoothly truncated exponential density by weighted maximum
likelihood.

## Usage

``` r
fit_truncated_exponential(value, weight, start = NULL)
```

## Arguments

- value:

  Numeric vector containing at least ten observations. Values are
  normally log predator/prey mass ratios.

- weight:

  Numeric vector, of the same length as `value`, containing non-negative
  frequency weights.

- start:

  An optional named list with numeric elements `alpha`, `ll`, `ul`,
  `lr`, and `ur`, passed to
  [`bbmle::mle2()`](https://rdrr.io/pkg/bbmle/man/mle2.html) as starting
  values. See
  [`dtexp()`](https://gustavdelius.github.io/mizerStomach/reference/dtexp.md)
  for the parameterization.

## Value

A list with numeric elements `alpha`, `ll`, `ul`, `lr`, and `ur`.

## Details

The objective minimized by
[`bbmle::mle2()`](https://rdrr.io/pkg/bbmle/man/mle2.html) is the
weighted negative log-likelihood \$\$-\sum_i w_i \log f(x_i;
\alpha,l_l,u_l,l_r,u_r),\$\$ where `f` is
[`dtexp()`](https://gustavdelius.github.io/mizerStomach/reference/dtexp.md).
Weights therefore act as observation frequencies and need not be
integers.

If `start` is `NULL`, the initial values are `alpha = 0.5`, `ll` equal
to `max(0.1, min(value))`, `lr = max(value)`, and `ul = ur = 20`.
Optimizer evaluations that violate `ll >= 0.9 * min(value)`, `lr >= ll`,
`alpha + ul >= 1.01`, or `ur - alpha >= 0.01`, or that yield an invalid
density, receive a large objective penalty. The optimizer is allowed up
to 10,000 iterations. Because this is a nonlinear five-parameter
optimization, convergence can depend on the starting values;
[`fit_log_ppmr()`](https://gustavdelius.github.io/mizerStomach/reference/fit_log_ppmr.md)
can reuse a previous truncated-exponential fit as `start`.

## Examples

``` r
# \donttest{
cod_data <- validate_ppmr_data(barnes_data, species = "Atlantic cod")
fit_truncated_exponential(cod_data$log_ppmr, cod_data$n_prey)
#> $alpha
#> [1] -0.3011413
#> 
#> $ll
#> [1] 2.897734
#> 
#> $ul
#> [1] 2.28755
#> 
#> $lr
#> [1] 15.22522
#> 
#> $ur
#> [1] 17.88541
#> 
# }
```
