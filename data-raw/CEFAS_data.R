library(mizerStomach)
library(dplyr)
load("../stomach_data/data/stomach_dataset.Rdata")

ppmr_data <- stom_df |>
    transmute(species = pred_species,
              w_pred = pred_weight_g,
              w_prey = prey_ind_weight_g,
              n_prey = nprey_perpred,
              log_ppmr = log(ppmr)) |>
    filter(w_pred > 0, w_prey > 0, n_prey > 0) |>
    na.omit()

ppmr_data <- validate_ppmr_data(ppmr_data)

species <- "Gadus morhua"

plot_ppmr_violins(ppmr_data, species)

fit0 <- fit_log_ppmr(ppmr_data, species, distribution = "normal")
plot_log_ppmr_fit(ppmr_data, fit0)

fit1 <- fit_log_ppmr(ppmr_data, species, distribution = "normal",
                     power = 1)
plot_log_ppmr_fit(ppmr_data, fit1)

fit0 <- fit_log_ppmr(ppmr_data, species , distribution = "truncated exponential")
plot_log_ppmr_fit(ppmr_data, fit0)

# Fit to biomass
fit1 <- fit_log_ppmr(ppmr_data, species,
                     distribution = "truncated exponential",
                     power = 1)
plot_log_ppmr_fit(ppmr_data, fit1)

species <- "Merluccius merluccius"

fit <- fit_log_ppmr(ppmr_data, species, distribution = "normal")
plot_log_ppmr_fit(ppmr_data, fit)

fit <- fit_log_ppmr(ppmr_data, species, distribution = "normal", power = 1)
plot_log_ppmr_fit(ppmr_data, fit)

fit <- fit_log_ppmr(ppmr_data, species, distribution = "truncated exponential")
plot_log_ppmr_fit(ppmr_data, fit)

fit <- fit_log_ppmr(ppmr_data, species, distribution = "truncated exponential", power = 1)
plot_log_ppmr_fit(ppmr_data, fit)

fit <- fit_log_ppmr(ppmr_data, species, distribution = "gaussian mixture")
plot_log_ppmr_fit(ppmr_data, fit)

fit <- fit_log_ppmr(ppmr_data, species, distribution = "gaussian mixture",
                    power = 1)
plot_log_ppmr_fit(ppmr_data, fit)
