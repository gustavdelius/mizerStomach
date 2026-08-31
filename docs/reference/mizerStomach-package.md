# mizerStomach: Using stomach content data in mizer

Fits distributions to predator/prey body-mass ratios observed in stomach
contents and converts supported distributions to feeding-kernel
parameters for mizer size-spectrum models.

## Details

The package works with \\l=\log(w\_{pred}/w\_{prey})\\. A typical
workflow is to:

1.  standardize observations with
    [`validate_ppmr_data()`](https://gustavdelius.github.io/mizerStomach/reference/validate_ppmr_data.md);

2.  inspect their dependence on predator size with
    [`plot_ppmr_violins()`](https://gustavdelius.github.io/mizerStomach/reference/plot_ppmr_violins.md);

3.  fit normal, smoothly truncated-exponential, or Gaussian-mixture
    densities with
    [`fit_log_ppmr()`](https://gustavdelius.github.io/mizerStomach/reference/fit_log_ppmr.md);

4.  diagnose number- and biomass-weighted fits with
    [`plot_log_ppmr_fit()`](https://gustavdelius.github.io/mizerStomach/reference/plot_log_ppmr_fit.md);
    and

5.  transfer a fitted distribution to a mizer model with
    [`set_kernel_params()`](https://gustavdelius.github.io/mizerStomach/reference/set_kernel_params.md).

Fit objects retain the exponent of prey mass used as an observation
weight.
[`transform_fit()`](https://gustavdelius.github.io/mizerStomach/reference/transform_fit.md)
uses family-specific parameter updates to express a fit at another
weighting without fitting the raw observations again.

## Vignettes

Read the package articles in this order:

- [`vignette("mizerStomach", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/mizerStomach.md)
  gives a complete introductory workflow.

- [`vignette("density_functions", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/density_functions.md)
  explains weighting and density transformations.

- [`vignette("PPMR_distributions", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/PPMR_distributions.md)
  covers data limitations, family choice, and fit diagnostics.

- [`vignette("feeding_kernels", package = "mizerStomach")`](https://gustavdelius.github.io/mizerStomach/articles/feeding_kernels.md)
  derives and demonstrates the mizer feeding-kernel conversion.

## See also

Useful links:

- <https://gustavdelius.github.io/mizerStomach/>

- <https://github.com/gustavdelius/mizerStomach>

- Report bugs at <https://github.com/gustavdelius/mizerStomach/issues>
