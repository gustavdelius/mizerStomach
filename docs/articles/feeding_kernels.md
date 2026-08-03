# From stomach distributions to mizer feeding kernels

``` r

library(mizerStomach)
```

A stomach-content distribution is a realized outcome: it depends on
predator preference, available prey, ingestion, digestion, and sampling.
A mizer feeding kernel (Scott et al. 2014) represents only the prey-size
dependence of encounter and selection. Connecting the two therefore
requires assumptions.

This article derives the transformation used by
[`set_kernel_params()`](https://gustavdelius.github.io/mizerStomach/reference/set_kernel_params.md)
and shows how it is applied. The weighting notation is introduced in
[`vignette("density_functions")`](https://gustavdelius.github.io/mizerStomach/articles/density_functions.md);
distribution selection and diagnostics are discussed in
[`vignette("PPMR_distributions")`](https://gustavdelius.github.io/mizerStomach/articles/PPMR_distributions.md).

## Assumptions

The package uses the following approximation:

1.  The log predator/prey mass-ratio distribution is adequately
    independent of predator mass over the fitted range.
2.  Stomach contents are in a statistical steady state: prey of each
    size enter stomachs through ingestion at the same average rate at
    which they cease to be identifiable through digestion.
3.  The mass digestion rate scales as prey mass to the power
    $`2/3`$(Jobling 1981; Bromley 1994; Andersen 1999).
4.  The prey community experienced by the predator has number density
    proportional to $`w^{-\lambda}`$.
5.  Other effects on detectability and stomach residence time do not
    introduce an additional systematic prey-size dependence.

These assumptions are a modeling choice, not information recovered from
the stomach data. The resulting kernel should be interpreted and tested
with that uncertainty in mind.

## Ingestion

Let $`w'`$ be predator mass, $`w`$ prey mass, and $`l=\log(w'/w)`$.
Write the rate density at which a predator ingests prey of mass $`w`$ as

``` math
I(w\mid w') =
i(w')\,\phi(w'/w)\,N_c(w).
```

Here $`\phi`$ is the feeding-kernel shape, $`N_c(w)`$ is the number
density of available prey, and $`i(w')`$ contains factors that depend on
predator mass but not prey mass. In a multispecies model, $`N_c`$ can
include species interaction strengths:

``` math
N_c(w)=\sum_j\theta_{ij}N_j(w).
```

The historical prey field experienced by sampled predators is rarely
known. The package approximates it by a power-law resource spectrum,

``` math
N_c(w)=n_c w^{-\lambda}.
```

For a mizer model, $`\lambda`$ is read from
`params@resource_params$lambda`; it is not fixed at two by the package.

## Digestion and stomach residence

If prey mass is digested at a rate proportional to $`w^{2/3}`$ and prey
remains identifiable until a fixed fraction has been digested,
identifiable residence time scales as $`w^{1/3}`$. The corresponding
disappearance rate is

``` math
D(w)=d w^{-1/3}.
```

Let $`N_w(w\mid w')`$ be the stomach prey-number density. The
steady-state balance assumption is

``` math
I(w\mid w')=D(w)N_w(w\mid w').
```

Substitution gives

``` math
i(w')\phi(w'/w)n_cw^{-\lambda}
=d w^{-1/3}N_w(w\mid w'),
```

and hence, up to factors independent of prey mass,

``` math
\phi(w'/w)\propto
w^{\lambda-1/3}N_w(w\mid w').
```

## Express the result on the log-PPMR scale

Let $`f_0(l)`$ be the number-weighted probability density of
identifiable stomach prey. The change of variables derived in
[`vignette("density_functions")`](https://gustavdelius.github.io/mizerStomach/articles/density_functions.md)
gives

``` math
f_0(l)\propto w N_w(w\mid w'),
\qquad
N_w(w\mid w')\propto \frac{f_0(l)}{w}.
```

Therefore

``` math
\phi(w'/w)
\propto w^{\lambda-4/3}f_0(l)
\propto e^{-(\lambda-4/3)l}f_0(l).
```

In the package’s weighting notation, the kernel shape is the stomach
distribution represented at

``` math
q_{mizer}=\lambda-\frac{4}{3}.
```

This is exactly the target power used internally by
[`set_kernel_params()`](https://gustavdelius.github.io/mizerStomach/reference/set_kernel_params.md).
A fit can have been estimated at any recorded `power`;
[`transform_fit()`](https://gustavdelius.github.io/mizerStomach/reference/transform_fit.md)
moves it from that power to $`q_{mizer}`$ before model parameters are
set.

## Consequences for fitted families

Suppose a fit was estimated at exponent $`q_0`$ and let
$`a=q_{mizer}-q_0`$.

For a normal distribution,

``` math
\mu_{kernel}=\mu_{fit}-a\sigma^2,
\qquad
\sigma_{kernel}=\sigma.
```

mizer represents this as a `"lognormal"` kernel with
`beta = exp(mu_kernel)` and `sigma = sigma_kernel`.

For a smoothly truncated exponential,

``` math
\alpha_{kernel}=\alpha_{fit}-a,
```

while the two cutoff locations and steepnesses are unchanged. mizer
represents this as a `"power_law"` kernel using its five `kernel_*`
columns.

Gaussian mixtures are closed under the weighting transformation but do
not currently have a corresponding mizer kernel parameterization, so
[`set_kernel_params()`](https://gustavdelius.github.io/mizerStomach/reference/set_kernel_params.md)
rejects them.

## Worked mizer example

We fit the Atlantic cod diet distribution at `power = 2/3`. The North
Sea example model calls this species `"Cod"`, so the fit must be renamed
before it is transferred.

``` r

cod_fit <- fit_log_ppmr(
  barnes_data,
  species = "Atlantic cod",
  distribution = "normal",
  power = 2 / 3
)
cod_fit
#>                  mean       sd      species distribution     power min_w_pred
#> Atlantic cod 3.443057 1.276434 Atlantic cod       normal 0.6666667          0
```

The model’s resource exponent determines the exact kernel weighting:

``` r

params <- mizer::NS_params
kernel_power <- params@resource_params$lambda - 4 / 3
kernel_power
#> [1] 0.8
transform_fit(cod_fit, power = kernel_power)
#>                  mean       sd      species distribution power min_w_pred
#> Atlantic cod 3.225819 1.276434 Atlantic cod       normal   0.8          0
```

[`set_kernel_params()`](https://gustavdelius.github.io/mizerStomach/reference/set_kernel_params.md)
performs that transformation itself. Only species present in the
supplied fit are changed, and the returned object must be assigned.

``` r

model_cod_fit <- cod_fit
model_cod_fit$species <- "Cod"
rownames(model_cod_fit) <- "Cod"

params <- set_kernel_params(params, model_cod_fit)
subset(
  mizer::species_params(params),
  species == "Cod",
  select = c(species, pred_kernel_type, beta, sigma)
)
#> An object of class "species_params" containing parameters for 1 species:
#>  species pred_kernel_type     beta    sigma
#>      Cod        lognormal 25.17419 1.276434
```

## Extract and inspect existing kernels

[`extract_fit()`](https://gustavdelius.github.io/mizerStomach/reference/extract_fit.md)
reverses the parameter mapping and returns distributions at `power = 0`.
This allows existing model kernels to be inspected with
[`plot_log_ppmr_fit()`](https://gustavdelius.github.io/mizerStomach/reference/plot_log_ppmr_fit.md),
evaluated with
[`get_density()`](https://gustavdelius.github.io/mizerStomach/reference/get_density.md),
or transformed to another weighting.

``` r

model_fits <- extract_fit(params)
model_fits["Cod", c("species", "distribution", "power", "mean", "sd")]
#>     species distribution power     mean       sd
#> Cod     Cod       normal     0 4.529247 1.276434

cod_diet_again <- transform_fit(
  model_fits["Cod", , drop = FALSE],
  power = 2 / 3
)
cod_diet_again[c("power", "mean", "sd")]
#>         power     mean       sd
#> Cod 0.6666667 3.443057 1.276434
```

[`extract_fit()`](https://gustavdelius.github.io/mizerStomach/reference/extract_fit.md)
accepts `species_dict` when model and stomach-data names need to be
reconciled in the opposite direction.

``` r

cod_from_model <- extract_fit(
  params,
  species_dict = c(Cod = "Atlantic cod")
)
cod_from_model["Atlantic cod", c("species", "distribution", "power")]
#>                   species distribution power
#> Atlantic cod Atlantic cod       normal     0
```

## Interpretation and sensitivity

The fitted kernel is conditional on the assumed prey spectrum and
digestion scaling. In particular:

- changing $`\lambda`$ changes the exponential tilt and therefore the
  kernel mean or slope;
- size-dependent prey availability can be mistaken for predator
  preference;
- digestion and identification rates may not follow one allometry for
  all prey types;
- pooled stomach observations can mix different prey fields and predator
  behaviors; and
- a good fit to stomach densities does not validate the resulting
  population dynamics.

Treat the transferred parameters as empirically informed starting
values. Evaluate them with the other evidence available for the predator
and with mizer model diagnostics or calibration. Exact parameter
mappings and error conditions are documented in
[`?set_kernel_params`](https://gustavdelius.github.io/mizerStomach/reference/set_kernel_params.md)
and
[`?extract_fit`](https://gustavdelius.github.io/mizerStomach/reference/extract_fit.md).

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

Scott, Finlay, Julia L Blanchard, and Ken H Andersen. 2014. “Mizer: An r
Package for Multispecies, Trait-Based and Community Size Spectrum
Ecological Modelling.” *Methods in Ecology and Evolution* 5 (10):
1121–25.
