#' mizerStomach: Using stomach content data in mizer
#'
#' This package provides functions to fit distributions to stomach content data
#' and use these distributions to choose parameters for the predation kernel in
#' a mizer model.
#'
#' @import mizer mizerExperimental dplyr ggplot2
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
