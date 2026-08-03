# Weighting and log-PPMR distributions

``` r

library(mizerStomach)
```

This article explains the weighting convention used by
[`fit_log_ppmr()`](https://gustavdelius.github.io/mizerStomach/reference/fit_log_ppmr.md)
and
[`transform_fit()`](https://gustavdelius.github.io/mizerStomach/reference/transform_fit.md).
It is the conceptual reference for the shorter discussion in
[`vignette("mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/mizerStomach.md).
The following article,
[`vignette("feeding_kernels")`](https://gustavdelius.github.io/mizerStomach/articles/feeding_kernels.md),
applies the same convention to a mizer feeding kernel.

## From prey mass to log PPMR

Let $`w'`$ be predator mass and $`w`$ prey mass. Define the
predator/prey mass ratio and its logarithm by

``` math
r = \frac{w'}{w},
\qquad
l = \log(r) = \log\left(\frac{w'}{w}\right).
```

Suppose $`f_w(w\mid w')`$ is the probability density of prey mass in
stomachs of predators of mass $`w'`$. Since $`w = w'e^{-l}`$ and
$`|dw/dl| = w`$, the density on the log-PPMR scale is

``` math
f_0(l\mid w') = w f_w(w\mid w').
```

The subscript zero anticipates that this density gives equal weight to
every prey individual. Similarly, because $`w=w'/r`$,

``` math
f_r(r\mid w') =
\frac{w'}{r^2}f_w\left(\frac{w'}{r}\,\middle|\,w'\right).
```

Working with $`l`$ is convenient because multiplicative predator/prey
ratios become additive differences and the package’s fitted families
have simple weighting transformations on this scale.

## A family of weighted densities

Rows in stomach datasets can represent more than one prey individual
through `n_prey`. At weighting exponent $`q`$,
[`fit_log_ppmr()`](https://gustavdelius.github.io/mizerStomach/reference/fit_log_ppmr.md)
gives row $`i`$ weight

``` math
n_{prey,i}w_{prey,i}^{q}.
```

For predators of a fixed mass $`w'`$, $`w^q=(w')^q e^{-ql}`$. The factor
$`(w')^q`$ disappears when the conditional density is normalized, giving

``` math
f_q(l\mid w') =
\frac{e^{-ql}f_0(l\mid w')}
{\int e^{-q\tilde l}f_0(\tilde l\mid w')d\tilde l}.
```

More generally, if a fit currently represents exponent $`q_0`$, changing
to $`q_1`$ gives

``` math
f_{q_1}(l) \propto
e^{-(q_1-q_0)l}f_{q_0}(l).
```

This is the exponential tilt implemented by
[`transform_fit()`](https://gustavdelius.github.io/mizerStomach/reference/transform_fit.md).
The function changes fitted parameters; it does not revisit the stomach
observations.

Several exponents have useful interpretations:

| Exponent | Interpretation |
|:---|:---|
| $`q=0`$ | Number distribution of identifiable prey in stomachs |
| $`q=2/3`$ | Diet-biomass distribution under the digestion assumption below |
| $`q=1`$ | Biomass distribution of identifiable prey in stomachs |
| $`q=\lambda-4/3`$ | Feeding-kernel representation used by [`set_kernel_params()`](https://gustavdelius.github.io/mizerStomach/reference/set_kernel_params.md) |

These are different summaries of the same stomach observations, not
competing definitions of PPMR.

### Why diet biomass corresponds to `power = 2/3`

Suppose prey mass is digested at a rate proportional to $`w^{2/3}`$
because the rate is controlled by surface area. If prey remains
identifiable until a fixed fraction of its mass has been digested, its
identifiable residence time is proportional to

``` math
\frac{w}{w^{2/3}} = w^{1/3}.
```

The number of prey of mass $`w`$ observed in stomachs is therefore
proportional to their ingestion rate times $`w^{1/3}`$. Diet biomass is
ingestion rate times $`w`$, so it is proportional to observed stomach
numbers times $`w^{2/3}`$. This motivates `power = 2 / 3`; it is an
assumption about digestion, not a universal constant (Jobling 1981;
Bromley 1994; Andersen 1999).

### Pooling predator sizes

The cancellation of $`(w')^q`$ above occurs within a fixed predator
mass. Transforming a pooled species-level fit assumes that the
conditional log-PPMR distribution is adequately independent of predator
size. If that assumption does not hold, directly fitting the pooled
observations at a new `power` can differ from transforming an old fit.
Use
[`plot_ppmr_violins()`](https://gustavdelius.github.io/mizerStomach/reference/plot_ppmr_violins.md)
to investigate the assumption and consider stratified fits when
necessary.

## Transforming fitted families

The normal, smoothly truncated-exponential, and Gaussian-mixture
families are closed under exponential tilting. Let $`a=q_1-q_0`$.

### Normal distribution

For a normal density with mean $`\mu`$ and standard deviation
$`\sigma`$,

``` math
e^{-al}\exp\left[-\frac{(l-\mu)^2}{2\sigma^2}\right]
\propto
\exp\left[-\frac{(l-(\mu-a\sigma^2))^2}{2\sigma^2}\right].
```

The transformed mean is $`\mu-a\sigma^2`$ and the standard deviation is
unchanged.

``` r

fit_number <- fit_log_ppmr(
  barnes_data,
  "Albacore",
  distribution = "normal",
  power = 0
)
fit_biomass <- transform_fit(fit_number, power = 1)
fit_number[c("power", "mean", "sd")]
#>          power     mean       sd
#> Albacore     0 9.546003 1.146738
fit_biomass[c("power", "mean", "sd")]
#>          power     mean       sd
#> Albacore     1 8.230995 1.146738
```

### Smoothly truncated exponential

The unnormalized density used by
[`dtexp()`](https://gustavdelius.github.io/mizerStomach/reference/dtexp.md)
is

``` math
f(l) \propto
\frac{e^{\alpha l}}
{(1+e^{u_l(l_l-l)})(1+e^{u_r(l-l_r)})}.
```

Multiplication by $`e^{-al}`$ changes only the exponential slope:

``` math
\tilde\alpha=\alpha-a.
```

The cutoff locations $`l_l,l_r`$ and steepnesses $`u_l,u_r`$ are
unchanged. The implemented density is numerically normalized over the
intended log-PPMR range $`0\le l\le30`$; see
[`?dtexp`](https://gustavdelius.github.io/mizerStomach/reference/dtexp.md)
for exact evaluation behavior.

### Gaussian mixture

For a mixture

``` math
f(l)=\sum_{j=1}^k p_j
\mathcal N(l\mid\mu_j,\sigma_j),
```

each transformed component has

``` math
\tilde\mu_j=\mu_j-a\sigma_j^2,
\qquad
\tilde\sigma_j=\sigma_j,
```

and its unnormalized mixing weight is

``` math
\tilde p_j^* =
p_j\exp\left(-a\mu_j+\frac{a^2\sigma_j^2}{2}\right).
```

The final $`\tilde p_j`$ are obtained by dividing these weights by their
sum. The following numerical check compares this parameter
transformation with directly tilting a density on a fine grid.

``` r

mixture <- list(
  species = "example",
  distribution = "gauss_mix",
  power = 0,
  min_w_pred = 0,
  p = c(0.3, 0.7),
  mean = c(3, 7),
  sd = c(1, 1.5)
)

grid <- seq(-5, 18, length.out = 20001)
dx <- grid[2] - grid[1]
original <- get_density(grid, mixture)
numerical <- exp(-0.7 * grid) * original
numerical <- numerical / sum(numerical * dx)
analytical <- get_density(grid, transform_fit(mixture, power = 0.7))

max(abs(numerical - analytical))
#> [1] 1.453837e-13
```

Mixture fitting itself can still be sensitive to initialization and
degenerate components. The transformation above is exact for the
parameters supplied to it, but that does not guarantee that the original
mixture was a good fit.

## Practical use

Fit directly at the weighting most relevant to the analysis, then use
[`transform_fit()`](https://gustavdelius.github.io/mizerStomach/reference/transform_fit.md)
to inspect the implications at other powers. A large diagnostic
discrepancy after transformation indicates that the fitted family, the
predator-size-independence assumption, or both may be inadequate.

For exact arguments and return structures, see
[`?fit_log_ppmr`](https://gustavdelius.github.io/mizerStomach/reference/fit_log_ppmr.md),
[`?transform_fit`](https://gustavdelius.github.io/mizerStomach/reference/transform_fit.md),
and
[`?get_density`](https://gustavdelius.github.io/mizerStomach/reference/get_density.md).
For the additional assumptions that turn a stomach distribution into a
feeding kernel, continue with
[`vignette("feeding_kernels")`](https://gustavdelius.github.io/mizerStomach/articles/feeding_kernels.md).

## References

Andersen, Niels G. 1999. “The Effects of Predator Size, Temperature, and
Prey Characteristics on Gastric Evacuation in Whiting.” *Journal of Fish
Biology* 54 (2): 287–301.

Bromley, PJ. 1994. “The Role of Gastric Evacuation Experiments in
Quantifying the Feeding Rates of Predatory Fish.” *Reviews in Fish
Biology and Fisheries* 4 (1): 36–66.

Jobling, Malcolm. 1981. “Mathematical Models of Gastric Emptying and the
Estimation of Daily Rates of Food Consumption for Fish.” *Journal of
Fish Biology* 19 (3): 245–57.
