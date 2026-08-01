# mizerStomach

`mizerStomach` turns predator stomach-content observations into candidate
feeding kernels for [mizer](https://sizespectrum.org/mizer/) size-spectrum
models.

It fits distributions to observed prey sizes, helps diagnose whether those
distributions are adequate, translates them among prey-number and prey-biomass
weightings, and transfers supported fits to a `MizerParams` object.

[Get started with the complete workflow](https://gustavdelius.github.io/mizerStomach/articles/mizerStomach.html)
or browse the
[function reference](https://gustavdelius.github.io/mizerStomach/reference/).

## What the package provides

- Validation and standardization of predator/prey body-mass observations.
- Predator-size diagnostics for the assumption of a species-level PPMR
  distribution.
- Weighted fitting of normal, smoothly truncated-exponential, and
  two-component Gaussian-mixture distributions.
- Comparisons of fitted and observed prey-number and prey-biomass
  distributions.
- Analytical transformation of fitted distributions between prey-mass
  weightings.
- Interactive fitting and parameter adjustment with a Shiny gadget.
- Conversion between fitted stomach distributions and mizer feeding-kernel
  parameters.

## Installation

Install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("gustavdelius/mizerStomach")
```

## A quick glimpse

The package works with the log predator/prey mass ratio

$$
\log(\mathrm{PPMR}) = \log\left(\frac{w_{pred}}{w_{prey}}\right).
$$

The package includes `barnes_data`, with 23,164 predator/prey observations for
12 marine predator species. This example fits a normal distribution to
Atlantic cod using the diet-biomass weighting discussed in the articles:

```r
library(mizerStomach)

cod_fit <- fit_log_ppmr(
  barnes_data,
  species = "Atlantic cod",
  distribution = "normal",
  power = 2 / 3
)

plot_log_ppmr_fit(barnes_data, cod_fit, type = "histogram")
```

The fitted object records the weighting in its `power` column. It can be
expressed at another weighting without refitting the observations:

```r
cod_number_fit <- transform_fit(cod_fit, power = 0)

grid <- seq(2, 12, length.out = 100)
density <- get_density(grid, cod_number_fit)
```

Normal, truncated-exponential, and Gaussian-mixture fits can be transferred to
mizer. Species names must agree between the fit and model:

```r
params <- mizer::NS_params

model_cod_fit <- cod_fit
model_cod_fit$species <- "Cod"
rownames(model_cod_fit) <- "Cod"

params <- set_kernel_params(params, model_cod_fit)
```

`extract_fit()` performs the reverse conversion, allowing existing mizer
kernels to be inspected with the same plotting and density tools.

The [Getting started article](https://gustavdelius.github.io/mizerStomach/articles/mizerStomach.html)
explains each of these steps, including data validation, the meaning of
`power`, fit diagnostics, interactive refinement, and safe transfer to mizer.

## Input data

Each row describes one prey observation or a group of identical prey
observations. The required columns are:

| Column | Meaning |
|:--|:--|
| `species` | Predator species name |
| `w_pred` | Predator body mass |
| `w_prey` | Body mass of one prey individual |
| `n_prey` | Number or frequency of prey represented by the row |

Predator and prey masses may use any unit as long as they use the same unit.
`validate_ppmr_data()` checks this structure and calculates `log_ppmr` when it
is absent. It is a format validator, not a substitute for biological data
cleaning.

## Fitted distributions

`fit_log_ppmr()` supports three families:

| Family | Method | Transfer to mizer |
|:--|:--|:--|
| `"normal"` | Weighted maximum likelihood | Yes, as a lognormal kernel |
| `"trunc_exp"` | Weighted nonlinear maximum likelihood with smooth cutoffs | Yes, as a power-law kernel |
| `"gauss_mix"` | Weighted expectation-maximization | Yes, as a Gaussian-mixture kernel |

Observation rows receive weight `n_prey * w_prey^power`. Common choices are
`power = 0` for prey numbers, `power = 1` for stomach prey biomass, and
`power = 2 / 3` for diet biomass under the package's digestion-scaling
assumption. The meaning and limitations of these choices are developed in the
articles rather than repeated here.

## Documentation

After this homepage, the package articles form a progression:

1. [Getting started](https://gustavdelius.github.io/mizerStomach/articles/mizerStomach.html)
   gives the practical end-to-end workflow.
2. [Weighting and log-PPMR distributions](https://gustavdelius.github.io/mizerStomach/articles/density_functions.html)
   derives prey-mass weighting and density transformations.
3. [Choosing and diagnosing a log-PPMR distribution](https://gustavdelius.github.io/mizerStomach/articles/PPMR_distributions.html)
   covers data limitations, family choice, optimization, and diagnostics.
4. [From stomach distributions to mizer feeding kernels](https://gustavdelius.github.io/mizerStomach/articles/feeding_kernels.html)
   derives the mizer conversion and its biological assumptions.

## Scope

A fitted stomach distribution is a candidate description of predator feeding,
not direct proof of preference. Stomach contents can also reflect prey
availability, capture probability, digestion, evacuation, sampling gear,
season, location, and taxonomic resolution. These assumptions should remain
visible when interpreting a fitted distribution or the mizer model built from
it.

Report bugs or request features through the
[GitHub issue tracker](https://github.com/gustavdelius/mizerStomach/issues).
