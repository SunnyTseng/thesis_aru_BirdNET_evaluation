###
### Name: group_BirdNET_output
### 
### Author: Sunny Tseng
### Date: 2022-10-18
### Input: year, season, and data_folder
### Output: a tidy dataframe with BirdNET output from single season
###

# Library -----------------------------------------------------------------

library(tidyverse)
library(purrr)
library(here)
library(janitor)



# Function ----------------------------------------------------------------

group_BirdNET_output <- function(year, season, data_folder){
  
  # list the files
  year_season = paste0(year, "_", season)
  file_list <- list.files(here(data_folder, year_season, "14_02"), pattern = ".txt$", recursive = TRUE)
  
  # combine all the txt files
  data_full <- tibble()
  for(file in file_list){
    data <- read_delim(file = here(data_folder, year_season, "14_02", file), 
                       delim = ";",
                       col_type = list(`Start (s)` = col_double(),
                                       `End (s)` = col_double(),
                                       `Scientific name` = col_character(),
                                       `Common name` = col_character(),
                                       `Confidence` = col_double())) %>%
      mutate(recording = file)
    data_full <- bind_rows(data_full, data)
  }
  
  # wrangle the big dataset
  data_full_format <- data_full %>%
    clean_names() %>%
    separate(col = recording, into = c("site", "recording"), sep = "/") %>%
    separate(col = recording, into = c("recording", NA), sep = ".txt") %>%
    mutate(year = year, season = season, month = str_sub(recording, 5, 6), day = str_sub(recording, 7, 8)) %>%
    select(year, month, day, season, site, recording, start_s, end_s, scientific_name, common_name, confidence)

return(data_full_format)
}


# Example usage -----------------------------------------------------------


final <- group_BirdNET_output(year = 2022, season = "passerine", data_folder = "data/Audio_output")
write_csv(final, here("data", "processed", "2022_passerine_BirdNET_updated.csv"))



