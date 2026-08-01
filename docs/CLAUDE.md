# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Project Overview

`mizerStomach` is an R package that fits statistical distributions to
predator/prey mass ratio (PPMR) observations from stomach content data.
It integrates with the `mizer` fish ecosystem modeling package to
parameterize size selectivity (predation kernels).

## Common Commands

All development uses standard R/devtools workflows:

``` r
devtools::load_all()          # Load package for interactive development
devtools::document()          # Regenerate roxygen docs (NAMESPACE, man/)
devtools::check()             # Full R CMD check
devtools::build_vignettes()   # Build vignettes
pkgdown::build_site()         # Build documentation website
```

Run the interactive Shiny app:

``` r
shiny::runApp("inst/app.R")
```

Run tests with
[`devtools::test()`](https://devtools.r-lib.org/reference/test.html).
Test files are in `tests/testthat/`. Vignettes in `vignettes/` serve as
executable documentation.

## Architecture

### Core Modules

| File            | Responsibility                                                                                                                         |
|-----------------|----------------------------------------------------------------------------------------------------------------------------------------|
| `R/fit.R`       | MLE fitting of distributions to log(PPMR) data                                                                                         |
| `R/densities.R` | Density functions including custom [`dtexp()`](https://gustavdelius.github.io/mizerStomach/reference/dtexp.md) (truncated exponential) |
| `R/plot.R`      | ggplot2 visualizations of fits and PPMR violin plots                                                                                   |
| `R/transform.R` | Transform fit parameters when changing observation weighting power                                                                     |
| `R/validate.R`  | Input validation helpers called by all public functions                                                                                |

### Three Supported Distributions

1.  **Normal**: `mean`, `sd`
2.  **Truncated exponential** (`trunc_exp`): `alpha`, `ll`, `ul`, `lr`,
    `ur` — fitted via
    [`bbmle::mle2()`](https://rdrr.io/pkg/bbmle/man/mle2.html)
3.  **Gaussian mixture** (`gauss_mix`): `p`, `mean`, `sd` — fitted via
    EM algorithm (default k=2 components)

### Fit Object Convention

All fitting functions return a **data frame** with one row per species
and species names as rownames. Columns include: - `species`,
`distribution`, `power`, `min_w_pred` - Distribution parameters (varies
by type; `gauss_mix` uses list-columns for `p`, `mean`, `sd`)

[`fit_log_ppmr()`](https://gustavdelius.github.io/mizerStomach/reference/fit_log_ppmr.md)
accepts a vector of species names and returns a multi-row data frame.
Functions like
[`get_density()`](https://gustavdelius.github.io/mizerStomach/reference/get_density.md),
[`transform_fit()`](https://gustavdelius.github.io/mizerStomach/reference/transform_fit.md),
and
[`plot_log_ppmr_fit()`](https://gustavdelius.github.io/mizerStomach/reference/plot_log_ppmr_fit.md)
work on single-row or multi-row fit data frames.
[`validate_fit()`](https://gustavdelius.github.io/mizerStomach/reference/validate_fit.md)
also accepts old-style named lists for backwards compatibility.

### Data Flow

    Stomach data (data frame with log_ppmr column)
      → validate_ppmr_data()
      → fit_log_ppmr() / fit_normal() / fit_truncated_exponential() / fit_gaussian_mixture()
      → fit object
      → transform_fit()  (optional, to change power weighting)
      → get_density() / plot_log_ppmr_fit()

### Shiny App (`inst/app.R`)

Interactive parameter tuning for 14 hardcoded fish species. Reads from
an external path `../../stomach_data/data/stomach_dataset.Rdata`
relative to the app file. Provides sliders for distribution parameters
with real-time density visualization and RDS download.

### Key Dependencies

- `mizer` / `mizerExperimental` (GitHub): Ecosystem model this package
  feeds into
- `bbmle`: MLE fitting via `mle2()`
- `ggplot2` / `dplyr`: Visualization and data manipulation
- `Hmisc`: Weighted quantile functions
