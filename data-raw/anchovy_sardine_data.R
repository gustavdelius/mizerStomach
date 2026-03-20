library(mizerStomach)
library(dplyr)
anchovy_sardine <- read.csv("../stomach_data/data/AnchovyAndSardine.csv")

ppmr_data_a <- anchovy_sardine %>%
    rename(species = Species,
           w_prey = wprey,
           w_pred = wpredator,
           n_prey = Nprey) %>%
    mutate(log_ppmr = log(w_pred / w_prey)) |>
    mizerStomach::validate_ppmr_data()
