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
    na.omit() |>
    validate_ppmr_data()

predators <- c(
    Herring = "Clupea harengus",
    Cod = "Gadus morhua",
    Haddock = "Melanogrammus aeglefinus",
    Whiting = "Merlangius merlangus",
    "Blue whiting" = "Micromesistius poutassou",
    Hake = "Merluccius merluccius",
    Monkfish = "Lophius piscatorius",
    Mackerel = "Scomber scombrus",
    Plaice = "Pleuronectes platessa",
    Megrim = "Lepidorhombus whiffiagonis",
    Sole = "Solea solea"
)

fits <- data.frame()
for (predator in predators) {
    tryCatch({
        fit <- fit_log_ppmr(ppmr_data, species = predator,
                                          distribution = "trunc_exp")
    }, error = function(e) {
        warning("The fit did no succeed for ", predator, ": ", e)
    })
    fits <- rbind(fits, fit)
}
rownames(fits) <- fits$species

species <- predators["Herring"]

plot_ppmr_violins(ppmr_data, species)

fit0 <- fit_log_ppmr(ppmr_data, species, distribution = "normal")
plot_log_ppmr_fit(ppmr_data, fit0)

fit1 <- fit_log_ppmr(ppmr_data, species, distribution = "normal",
                     power = 1)
plot_log_ppmr_fit(ppmr_data, fit1)

fit0 <- fit_log_ppmr(ppmr_data, species , distribution = "trunc_exp")
plot_log_ppmr_fit(ppmr_data, fit0)

fit1 <- fit_log_ppmr(ppmr_data, species, distribution = "trunc_exp",
                     power = 1)
plot_log_ppmr_fit(ppmr_data, fit1)

species <- "Merluccius merluccius"

plot_ppmr_violins(ppmr_data, species)

fit <- fit_log_ppmr(ppmr_data, species, distribution = "normal")
plot_log_ppmr_fit(ppmr_data, fit)

fit <- fit_log_ppmr(ppmr_data, species, distribution = "normal", power = 1)
plot_log_ppmr_fit(ppmr_data, fit)

fit <- fit_log_ppmr(ppmr_data, species, distribution = "trunc_exp")
plot_log_ppmr_fit(ppmr_data, fit)

fit <- fit_log_ppmr(ppmr_data, species, distribution = "trunc_exp", power = 1)
plot_log_ppmr_fit(ppmr_data, fit)

fit <- fit_log_ppmr(ppmr_data, species, distribution = "gauss_mix")
plot_log_ppmr_fit(ppmr_data, fit)

fit <- fit_log_ppmr(ppmr_data, species, distribution = "gauss_mix",
                    power = 1)
plot_log_ppmr_fit(ppmr_data, fit)
