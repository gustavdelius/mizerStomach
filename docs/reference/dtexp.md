# Smoothly truncated exponential density

Evaluates the exponentially sloped density with smooth logistic cutoffs
used for truncated-power-law feeding kernels.

## Usage

``` r
dtexp(x, alpha, ll, ul, lr, ur)
```

## Arguments

- x:

  Numeric vector, normally containing log predator/prey mass ratios.

- alpha:

  Numeric exponential slope between the cutoffs. Positive values favor
  larger `x`; negative values favor smaller `x`.

- ll:

  Numeric location of the lower (left) logistic cutoff.

- ul:

  Numeric steepness of the lower cutoff. Larger positive values make the
  transition sharper.

- lr:

  Numeric location of the upper (right) logistic cutoff.

- ur:

  Numeric steepness of the upper cutoff. Larger positive values make the
  transition sharper.

## Value

A density vector with the same length as `x`, or an all-`NA` vector when
the density cannot be evaluated reliably.

## Details

Before normalization, the density is proportional to
\$\$\frac{\exp(\alpha x)}
{(1+\exp(u_l(l_l-x)))(1+\exp(u_r(x-l_r)))}\$\$. The implementation
evaluates an algebraically equivalent expression in log space, centered
at `(ll + lr) / 2`, to reduce overflow during optimization. The
normalizing constant is obtained by numerical integration over \\0 \le x
\le 30\\, the intended log-PPMR range. The function can evaluate `x`
outside that range, but it remains normalized only with respect to the
interval `[0, 30]`.

If numerical integration fails, or any requested density is non-finite
or non-positive, the function returns `NA` for every element of `x`.
This all-or-nothing behavior lets
[`fit_truncated_exponential()`](https://gustavdelius.github.io/mizerStomach/reference/fit_truncated_exponential.md)
reject unstable parameter combinations during optimization.

## See also

[`fit_truncated_exponential()`](https://gustavdelius.github.io/mizerStomach/reference/fit_truncated_exponential.md)
for weighted maximum-likelihood estimation,
[`transform_truncated_exp()`](https://gustavdelius.github.io/mizerStomach/reference/transform_truncated_exp.md)
for changing its weighting, and
[`vignette("density_functions", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/density_functions.md)
for the density transformation.

## Examples

``` r
# Evaluate the truncated exponential density
x <- seq(0, 15, length.out = 100)
d <- dtexp(x, alpha = 0.5, ll = 2, ul = 20, lr = 12, ur = 20)
plot(x, d, type = "l", main = "Truncated exponential density")
```
