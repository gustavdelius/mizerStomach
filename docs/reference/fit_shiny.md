# Interactively tune PPMR distribution fits

Opens a Shiny gadget for fitting, comparing, and manually adjusting one
log-PPMR distribution per predator species.

## Usage

``` r
fit_shiny(ppmr_data, fits = NULL, predators = NULL)
```

## Arguments

- ppmr_data:

  A data frame with log ppmr observations. See
  [`validate_ppmr_data()`](https://gustavdelius.github.io/mizerStomach/reference/validate_ppmr_data.md)
  for details.

- fits:

  Optional fit data frame accepted by
  [`validate_fit()`](https://gustavdelius.github.io/mizerStomach/reference/validate_fit.md).
  Its rows determine the species shown and provide their initial
  distribution and parameters. If omitted, the gadget starts each
  selected species with a default truncated-exponential fit.

- predators:

  Optional character vector of species to include when `fits` is
  omitted. Defaults to all species in `ppmr_data` and is otherwise
  ignored.

## Value

A fit data frame with one row per selected predator when **Return** is
clicked.

## Details

The sidebar switches among species and the `"normal"`, `"trunc_exp"`,
and two-component `"gauss_mix"` families. Parameter sliders immediately
update the diagnostic produced by
[`plot_log_ppmr_fit()`](https://gustavdelius.github.io/mizerStomach/reference/plot_log_ppmr_fit.md).
On first switching to a family not yet cached for that species, the
gadget attempts a fit with
[`fit_log_ppmr()`](https://gustavdelius.github.io/mizerStomach/reference/fit_log_ppmr.md);
the **Fit** button repeats that fit. For a truncated exponential, the
currently displayed parameters are reused as optimizer starting values.

**Undo** restores the preceding cached state and **Reset** restores all
input fits. **Download** saves the currently selected fit for every
species as `fits.rds`. **Return** closes the gadget and returns the same
collection to the calling R session. Fits for families visited but not
currently selected are cached only for the duration of the gadget.

The gadget uses an external browser viewer and blocks the calling R
session until it is closed. It is intended for interactive use, not
scripts or non-interactive package builds.

## See also

[`fit_log_ppmr()`](https://gustavdelius.github.io/mizerStomach/reference/fit_log_ppmr.md)
for non-interactive fitting and
[`plot_log_ppmr_fit()`](https://gustavdelius.github.io/mizerStomach/reference/plot_log_ppmr_fit.md)
for the diagnostic shown by the gadget. See
[`vignette("PPMR_distributions", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/PPMR_distributions.md)
for fit-selection and reporting guidance.

## Examples

``` r
if (FALSE) { # \dontrun{
initial <- fit_log_ppmr(barnes_data, "Atlantic cod", "normal")
tuned <- fit_shiny(barnes_data, fits = initial)
} # }
```
