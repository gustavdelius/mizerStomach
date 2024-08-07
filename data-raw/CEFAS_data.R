library(mizerStomach)
library(dplyr)
load("../stomach_data/data/stomach_dataset.Rdata")

species <- "Gadus morhua"

df <- stom_df |>
  filter(pred_species == species) |>
  transmute(species = pred_species,
            w_pred = pred_weight_g,
            w_prey = prey_ind_weight_g,
            n_prey = nprey_perpred,
            log_ppmr = log(ppmr)) |>
  filter(w_pred > 0, w_prey > 0, n_prey > 0)

df <- validate_ppmr_data(df)

fit <- fit_log_ppmr(df, species , distribution = "truncated_exponential")

plot_log_ppmr_fit(df, fit) +
  ggtitle("Fit to number density")

# Fit to biomass
fit1 <- fit_log_ppmr(df, species,
                    distribution = "truncated_exponential",
                    power = 1)

plot_log_ppmr_fit(df, fit1) +
  ggtitle("Fit to biomass density")
