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
  
  # wrangle the big dataset
  data_full_format <- data_full %>%
    clean_names() %>%
    mutate(site = str_split_i(filepath, pattern = "\\\\", i = 5),
           recording = str_split_i(filepath, pattern = "\\\\", i = 6)) %>%
    mutate(date = str_split_i(recording, pattern = ".WAV", i = 1) %>% as_datetime())

return(data_full_format)
}


# Example usage -----------------------------------------------------------


final <- group_BirdNET_output(path = here("data", "2020_passerine_BirdNET_V2.4_C0.5"))
#write_csv(final, here("data", "processed", "2022_passerine_BirdNET_V2.4_C0.5.csv"))



