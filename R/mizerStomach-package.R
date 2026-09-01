#' mizerStomach: Using stomach content data in mizer
#'
#' Fits distributions to predator/prey body-mass ratios observed in stomach
#' contents and converts supported distributions to feeding-kernel parameters
#' for mizer size-spectrum models.
#'
#' @details
#' The package works with
#' \eqn{l=\log(w_{pred}/w_{prey})}. A typical workflow is to:
#'
#' 1. standardize observations with [validate_ppmr_data()];
#' 2. inspect their dependence on predator size with [plot_ppmr_violins()];
#' 3. fit normal, smoothly truncated-exponential, or Gaussian-mixture densities
#'    with [fit_log_ppmr()];
#' 4. diagnose number- and biomass-weighted fits with
#'    [plot_log_ppmr_fit()]; and
#' 5. transfer a fitted distribution to a mizer model with
#'    [set_kernel_params()].
#'
#' Fit objects retain the exponent of prey mass used as an observation weight.
#' [transform_fit()] uses family-specific parameter updates to express a fit at
#' another weighting without fitting the raw observations again.
#'
#' @section Vignettes:
#' Read the package articles in this order:
#'
#' * `vignette("mizerStomach", package = "mizerStomach")` gives a complete
#'   introductory workflow.
#' * `vignette("density_functions", package = "mizerStomach")` explains
#'   weighting and density transformations.
#' * `vignette("PPMR_distributions", package = "mizerStomach")` covers data
#'   limitations, family choice, and fit diagnostics.
#' * `vignette("feeding_kernels", package = "mizerStomach")` derives and
#'   demonstrates the mizer feeding-kernel conversion.
#'
#' @import dplyr ggplot2
#' @importFrom stats weighted.mean dnorm integrate rnorm sd setNames
#' @importFrom graphics text
#' @importFrom utils hasName
#' @importFrom bbmle mle2
#' @importFrom shiny browserViewer
#' @keywords internal
"_PACKAGE"

# Column names used in non-standard evaluation (dplyr/ggplot2)
globalVariables(c(
    "Biomass", "Density", "Number", "Type",
    "bin", "biomass", "log_ppmr", "species",
    "w_pred", "weight"
))
