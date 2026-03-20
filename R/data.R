#' Predator-prey body size data from Barnes et al.
#'
#' A dataset of predator-prey body mass observations compiled by Barnes et al.
#' (2008), filtered to include only predator species with more than 1000
#' stomach content observations. Each row represents a single prey item found
#' in a predator's stomach.
#'
#' @format A tibble with 23,164 rows and 5 columns:
#' \describe{
#'   \item{species}{Common name of the predator species (character).}
#'   \item{w_pred}{Body mass of the predator in grams.}
#'   \item{w_prey}{Body mass of the prey item in grams.}
#'   \item{n_prey}{Number of prey items (always 1 in this dataset, since each
#'     row represents a single observation).}
#'   \item{log_ppmr}{Logarithm of the predator/prey mass ratio,
#'     i.e., \eqn{\log(w_{pred} / w_{prey})}.}
#' }
#'
#' The dataset contains observations for the following 12 predator species:
#' Albacore, Antarctic fish, Atlantic bluefin tuna, Atlantic cod, Bigeye tuna,
#' Bluefish, European sea bass, Silver hake, Smooth dogfish,
#' Spurdog / spiny dogfish, Winter skate, and Yellowfin tuna.
#'
#' @source Barnes, C., Bethea, D.M., Brodeur, R.D., et al. (2008).
#'   Predator and prey body sizes in marine food webs.
#'   *Ecology*, 89(3), 881. \doi{10.1890/07-1551.1}
#'
#' @seealso [validate_ppmr_data()] for validating similar datasets,
#'   \code{vignette("PPMR_distributions")} for analysis using this data.
#'
#' @examples
#' head(barnes_data)
#' unique(barnes_data$species)
"barnes_data"
