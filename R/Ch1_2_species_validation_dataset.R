###
### Name: species_validation_dataset
### 
### Author: Sunny Tseng
### Date: 2022-10-18
### Input: year, target_species, N
### Output: a tidy dataframe with stratified sampling for BirdNET validation
###


###
### Library
###
library(tidyverse)
library(here)


###
### Main function here
###
species_validation_dataset <- function(year, target_species, sample){
  set.seed(100)
  
  # import data
  data <- read_csv(here("data", "processed", paste0(year, "_passerine_BirdNET.csv")))
  
  # prepare single species data
  data_species <- data %>%
    filter(common_name == target_species) %>%
    mutate(category = cut(x = confidence, breaks = seq(0.1, 1, 0.05)))
  
  # plot of the predicted confidence
  g_histogram <- data_species %>%
    ggplot(aes(x = confidence)) +
    geom_histogram(bins = 20) +
    theme_bw() +
    labs(title = paste0(year, " ", target_species), 
         y = "# of segments") +
    theme(plot.title = element_text(size = 15),
          axis.title = element_text(size = 15))
  g_histogram %>% print()
  
  # create a list of sapling file (10 out of each 10 bins)
  data_species_sampling <- data_species %>%
    group_by(category) %>%
    sample_n(sample, replace = T)
  
return(data_species_sampling)
}
 

###
### Example code
###
final <- species_validation_dataset(year = 2021, target_species = "Black-capped Chickadee", sample = 10)
write_csv(final, here("Ch1_BirdNET", "data", "2021_Black-capped Chickadee.csv"))

