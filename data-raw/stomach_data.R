library(mizerStomach)
library(dplyr)

load("../../stomach_data/data/stomach_dataset.Rdata")
stom_df <- filter(stom_df, data == "ICES_YOTS")

ppmr_data <- stom_df |>
    transmute(species = pred_species,
              w_pred = pred_weight_g,
              w_prey = prey_ind_weight_g,
              n_prey = nprey_perpred,
              log_ppmr = log(ppmr)) |>
    filter(w_pred > 0, w_prey > 0, n_prey > 0) |>
    na.omit() |>
    mizerStomach::validate_ppmr_data()


