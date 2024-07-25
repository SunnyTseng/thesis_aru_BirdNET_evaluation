###
### Name: group_BirdNET_output
### 
### Author: Sunny Tseng
### Date: 2022-10-18
### Input: path
### Output: a tidy dataframe with BirdNET output from single season
###

# Library -----------------------------------------------------------------

library(tidyverse)
library(purrr)
library(here)
library(janitor)


# Function ----------------------------------------------------------------

group_BirdNET_output <- function(path){
  
  # combine all the output tables
  data_full <- list.files(path,
                          pattern = ".csv$", recursive = TRUE,
                          full.names = TRUE) %>%
    map_df(~ read_csv(file = .,
                      col_types = cols(filepath = "c",
                                       start = "d",
                                       end = "d",
                                       scientific_name = "c",
                                       common_name = "c",
                                       confidence = "d",
                                       lat = "d",
                                       lon = "d",
                                       week = "d",
                                       overlap = "d",
                                       sensitivity = "d",
                                       min_conf = "d",
                                       species_list = "c",
                                       model = "c")))
  
return(data_full)
}


# Example usage -----------------------------------------------------------


 # data_full <- group_BirdNET_output(path = "F:/PhD thesis/data/Audio_output/2023_passerine_BirdNET_V2.4_C0.1")
 # write_csv(data_full, paste0("F:/PhD thesis/data/Audio_output_combined/", "2023_passerine_BirdNET_V2.4_C0.1.csv"))

