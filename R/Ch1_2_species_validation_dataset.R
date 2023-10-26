###
### Name: species_validation_dataset
### 
### Author: Sunny Tseng
### Date: 2022-10-18
### Input: target_species, N
### Output: a tidy dataframe with stratified sampling for BirdNET validation
###

# Library -----------------------------------------------------------------

library(tidyverse)
library(here)


# Function ----------------------------------------------------------------

species_validation_dataset <- function(target_species, N){
  set.seed(100)
  
  # import data
  data_2020 <- read_csv(here("data", "processed", paste0(2020, "_passerine_BirdNET_updated.csv")))
  data_2021 <- read_csv(here("data", "processed", paste0(2021, "_passerine_BirdNET.csv")))
  
  data <- bind_rows(data_2020, data_2021)
  # prepare single species data
  data_species <- data %>%
    filter(common_name == target_species) %>%
    mutate(category = cut(x = confidence, breaks = seq(0.1, 1, 0.05)))
  
  # plot of the predicted confidence
  g_histogram <- data_species %>%
    ggplot(aes(x = confidence)) +
    geom_histogram(bins = 20) +
    theme_bw() +
    labs(title = paste0(target_species), 
         y = "# of segments") +
    theme(plot.title = element_text(size = 15),
          axis.title = element_text(size = 15))
  
  g_histogram %>% print()
  
  # create a list of sapling file (10 out of each 10 bins)
  data_species_sampling <- data_species %>%
    group_by(category) %>%
    sample_n(N, replace = T)
  
return(data_species_sampling)
}
 


# Example code ------------------------------------------------------------


final <- species_validation_dataset(target_species = "Varied Thrush", N = 50)
write_csv(final, here("data", "number_evaluation", "Varied Thrush.csv"))

