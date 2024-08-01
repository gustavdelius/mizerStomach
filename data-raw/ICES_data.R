# We downloaded the data from the ICES stomach data website at
# https://stomachdata.ices.dk/inventory using the "Download data" button
# on 01/09/2024.
# We then extracted the files PredatorInformation.csv and PreyInformation.csv.
# There is information about the columns in these files at
# https://datsu.ices.dk/web/selRep.aspx?Dataset=157

library(readr)
library(dplyr)

selected_species <- c(
  "sprat" = 126425,
  "cod" = 126436,
  "haddock" = 126437,
  "blue whiting" = 126439
)

PredatorInformation <- read_csv("PredatorInformation.csv") |>
  select(tblPredatorInformationID, AphiaIDPredator, IndWgt) |>
  filter(!is.na(IndWgt))

PreyInformation <- read_csv("PreyInformation.csv") |>
  select(tblPredatorInformationID, Count, Weight, UnitWgt) |>
  filter(!is.na(Weight),
         !is.na(UnitWgt),
         !is.na(Count))

PreyInformation$Count[is.na(PreyInformation$Count)] <- 1

# The description of the `Weight` column is "Weight, individual or grouped".
# We interpreted that as meaning that if `Count` is specified then `Weight`
# is the individual weight.

ICES_data <- inner_join(PredatorInformation, PreyInformation,
                        by = "tblPredatorInformationID") |>
  mutate(n_prey = Count,
         w_prey = Weight,
         w_pred = IndWgt,
         log_ppmr = log(w_pred) - log(w_prey)) |>
  select(AphiaIDPredator, w_pred, n_prey, w_prey, log_ppmr)

# We used Worrms to look up common names for the aphia IDs
species_dictionary <- data.frame(
  AphiaID = unique(ICES_data$AphiaIDPredator),
  species = c("cod", "whiting", "grey gurnard", "angler", "thornback ray",
              "cuckoo ray", "horse mackerel", "turbot", "tub gurnard", "kite",
              "halibut", "pollack", "hake", "ling", "plaice", "mackerel")
)

ICES_data <- left_join(ICES_data, species_dictionary,
                       by = c("AphiaIDPredator" = "AphiaID"))

# Unfortunately this data set can not be trusted because there are many
# records where the prey size is larger than the predator size:
sum(ICES_data$w_prey > ICES_data$w_pred)
ICES_data <- ICES_data |> arrange(log_ppmr)
head(ICES_data)
