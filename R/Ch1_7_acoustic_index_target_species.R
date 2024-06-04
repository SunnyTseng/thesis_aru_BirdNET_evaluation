###
### Author: Sunny, 2024 Jun 03
### Question: Calculate the acoustic indices for each target species
###



# library -----------------------------------------------------------------

library(tidyverse)
library(here)
library(soundecology)


# functions ---------------------------------------------------------------

aci_values <- function(data = bird_data_target,
                       dir) { # path to the hard drive where the Audio was stored
  aci_values <- c()
  for (i in 1:nrow(data)) {
    # get necessary parameters to find the associated recording
    year <- data$year[i]
    site <- data$site[i]
    recording <- data$recording[i]
    start_s <- data$start_s[i]
    end_s <- data$end_s[i]
    
    # read in the recording as a Wave object, provided by tuneR package
    sound <- readWave(paste0(dir, "/", year, "_passerine", "/", site, "/", recording, ".wav"), 
                      from = start_s - 1, 
                      to = end_s + 1, 
                      units = "seconds")
    
    # calculate of the acoustic index
    soundfile_aci <- acoustic_complexity(sound)
    
    # save the values as a vector
    aci_values <- c(aci_values, soundfile_aci)
  }
  return(aci_values) 
}

# prepare acoustic data ---------------------------------------------------

# import data
data_2020 <- read_csv(here("data", "processed", paste0(2020, "_passerine_BirdNET_updated.csv")))
data_2021 <- read_csv(here("data", "processed", paste0(2021, "_passerine_BirdNET.csv")))
bird_data <- bind_rows(data_2020, data_2021)

# prepare single species data, 50 recordings per species
target_species <- c("American Robin",
                    "Hammond's Flycatcher",
                    "Northern Waterthrush",
                    "Olive-sided Flycatcher",
                    "Orange-crowned Warbler",
                    "Pacific Wren",
                    "Red-breasted Nuthatch",
                    "Ruby-crowned Kinglet",
                    "Swainson's Thrush",
                    "Tennessee Warbler",
                    "Varied Thrush",
                    "Western Wood-Pewee",
                    "White-throated Sparrow",
                    "Brown Creeper",
                    "MacGillivray's Warbler",
                    "Northern Flicker",
                    "Pacific-slope Flycatcher",
                    "Wilson's Warbler",
                    "Yellow-rumped Warbler")

bird_data_target <- bird_data %>%
  filter(common_name %in% target_species) %>%
  slice_max(order_by = confidence, 
            n = 50,
            by = common_name,
            with_ties = FALSE)


# calculation of the index ------------------------------------------------

aci_values_target <- bird_data_target %>%
  group_nest(common_name, scientific_name) %>%
  map(.x = data, .f =~ aci_values(data = .x,
                                  dir = "E:/Audio"))









