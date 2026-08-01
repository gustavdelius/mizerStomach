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

Let $w\prime$ be predator mass and $w$ prey mass. Define the
predator/prey mass ratio and its logarithm by

$$r = \frac{w\prime}{w},\qquad l = \log(r) = \log\left( \frac{w\prime}{w} \right).$$

Suppose $f_{w}(w \mid w\prime)$ is the probability density of prey mass
in stomachs of predators of mass $w\prime$. Since $w = w\prime e^{- l}$
and $|dw/dl| = w$, the density on the log-PPMR scale is

$$f_{0}(l \mid w\prime) = wf_{w}(w \mid w\prime).$$

The subscript zero anticipates that this density gives equal weight to
every prey individual. Similarly, because $w = w\prime/r$,

$$f_{r}(r \mid w\prime) = \frac{w\prime}{r^{2}}f_{w}\left( \frac{w\prime}{r}\, \middle| \, w\prime \right).$$

Working with $l$ is convenient because multiplicative predator/prey
ratios become additive differences and the package’s fitted families
have simple weighting transformations on this scale.

## A family of weighted densities

Rows in stomach datasets can represent more than one prey individual
through `n_prey`. At weighting exponent $q$,
[`fit_log_ppmr()`](https://gustavdelius.github.io/mizerStomach/reference/fit_log_ppmr.md)
gives row $i$ weight

$$n_{prey,i}w_{prey,i}^{q}.$$

For predators of a fixed mass $w\prime$,
$w^{q} = (w\prime)^{q}e^{- ql}$. The factor $(w\prime)^{q}$ disappears
when the conditional density is normalized, giving

$$f_{q}(l \mid w\prime) = \frac{e^{- ql}f_{0}(l \mid w\prime)}{\int e^{- q\widetilde{l}}f_{0}\left( \widetilde{l} \mid w\prime \right)d\widetilde{l}}.$$

More generally, if a fit currently represents exponent $q_{0}$, changing
to $q_{1}$ gives

$$f_{q_{1}}(l) \propto e^{- {(q_{1} - q_{0})}l}f_{q_{0}}(l).$$

This is the exponential tilt implemented by
[`transform_fit()`](https://gustavdelius.github.io/mizerStomach/reference/transform_fit.md).
The function changes fitted parameters; it does not revisit the stomach
observations.

Several exponents have useful interpretations:

| Exponent            | Interpretation                                                                                                                            |
|:--------------------|:------------------------------------------------------------------------------------------------------------------------------------------|
| $q = 0$             | Number distribution of identifiable prey in stomachs                                                                                      |
| $q = 2/3$           | Diet-biomass distribution under the digestion assumption below                                                                            |
| $q = 1$             | Biomass distribution of identifiable prey in stomachs                                                                                     |
| $q = \lambda - 4/3$ | Feeding-kernel representation used by [`set_kernel_params()`](https://gustavdelius.github.io/mizerStomach/reference/set_kernel_params.md) |

These are different summaries of the same stomach observations, not
competing definitions of PPMR.

### Why diet biomass corresponds to `power = 2/3`

Suppose prey mass is digested at a rate proportional to $w^{2/3}$
because the rate is controlled by surface area. If prey remains
identifiable until a fixed fraction of its mass has been digested, its
identifiable residence time is proportional to

$$\frac{w}{w^{2/3}} = w^{1/3}.$$

The number of prey of mass $w$ observed in stomachs is therefore
proportional to their ingestion rate times $w^{1/3}$. Diet biomass is
ingestion rate times $w$, so it is proportional to observed stomach
numbers times $w^{2/3}$. This motivates `power = 2 / 3`; it is an
assumption about digestion, not a universal constant.

### Pooling predator sizes

The cancellation of $(w\prime)^{q}$ above occurs within a fixed predator
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
families are closed under exponential tilting. Let $a = q_{1} - q_{0}$.

### Normal distribution

For a normal density with mean $\mu$ and standard deviation $\sigma$,

$$e^{- al}\exp\left\lbrack - \frac{(l - \mu)^{2}}{2\sigma^{2}} \right\rbrack \propto \exp\left\lbrack - \frac{\left( l - \left( \mu - a\sigma^{2} \right) \right)^{2}}{2\sigma^{2}} \right\rbrack.$$

The transformed mean is $\mu - a\sigma^{2}$ and the standard deviation
is unchanged.

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

$$f(l) \propto \frac{e^{\alpha l}}{\left( 1 + e^{u_{l}{(l_{l} - l)}} \right)\left( 1 + e^{u_{r}{(l - l_{r})}} \right)}.$$

Multiplication by $e^{- al}$ changes only the exponential slope:

$$\widetilde{\alpha} = \alpha - a.$$

The cutoff locations $l_{l},l_{r}$ and steepnesses $u_{l},u_{r}$ are
unchanged. The implemented density is numerically normalized over the
intended log-PPMR range $0 \leq l \leq 30$; see
[`?dtexp`](https://gustavdelius.github.io/mizerStomach/reference/dtexp.md)
for exact evaluation behavior.

### Gaussian mixture

For a mixture

$$f(l) = \sum\limits_{j = 1}^{k}p_{j}\mathcal{N}\left( l \mid \mu_{j},\sigma_{j} \right),$$

each transformed component has

$${\widetilde{\mu}}_{j} = \mu_{j} - a\sigma_{j}^{2},\qquad{\widetilde{\sigma}}_{j} = \sigma_{j},$$

and its unnormalized mixing weight is

$${\widetilde{p}}_{j}^{*} = p_{j}\exp\left( - a\mu_{j} + \frac{a^{2}\sigma_{j}^{2}}{2} \right).$$

The final ${\widetilde{p}}_{j}$ are obtained by dividing these weights
by their sum. The following numerical check compares this parameter
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
