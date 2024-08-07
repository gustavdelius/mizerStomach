library(mizerStomach)
library(dplyr)
load("../stomach_data/data/stomach_dataset.Rdata")

df <- stom_df |>
    transmute(species = pred_species,
              w_pred = pred_weight_g,
              w_prey = prey_ind_weight_g,
              n_prey = nprey_perpred,
              log_ppmr = log(ppmr)) |>
    filter(w_pred > 0, w_prey > 0, n_prey > 0) |>
    na.omit()

df <- validate_ppmr_data(df)

species <- "Gadus morhua"

fit0 <- fit_log_ppmr(df, species, distribution = "normal")
plot_log_ppmr_fit(df, fit0) +
    ggtitle("Fit normal distribution to number density")

fit1 <- fit_log_ppmr(df, species, distribution = "normal", power = 1)
plot_log_ppmr_fit(df, fit1) +
    ggtitle("Fit normal distribution to biomass density")

fit0 <- fit_log_ppmr(df, species , distribution = "truncated_exponential")

plot_log_ppmr_fit(df, fit0) +
    ggtitle("Fit truncated exponential to number density")

# Fit to biomass
fit1 <- fit_log_ppmr(df, species,
                     distribution = "truncated_exponential",
                     power = 1)

plot_log_ppmr_fit(df, fit1) + ggtitle("Fit to biomass density")

species <- "Merluccius merluccius"

fit <- fit_log_ppmr(df, species, distribution = "gaussian_mixture")
plot_log_ppmr_fit(df, fit) + ggtitle("Fit to number density")


fit <- fit_log_ppmr(df, species, distribution = "gaussian_mixture",
                    power = 1)
plot_log_ppmr_fit(df, fit) + ggtitle("Fit to biomass density")
