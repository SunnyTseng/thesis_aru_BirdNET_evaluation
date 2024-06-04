###
### Name: play sound for validation
### 
### Author: Sunny Tseng
### Last update: 2023-10-26
### Input: sound file directory
### Output: play the sound segment for validating TRUE or FALSE
###


# Library -----------------------------------------------------------------


library(seewave)
library(tuneR)
library(tidyverse)
library(here)


# Play sound --------------------------------------------------------------

dir <- "E:/Audio"
dataset <- read_csv(here("data", "number_evaluation", "Yellow_rumped Warbler_finished.csv"))

range <- 801:900
for (i in range) {
  year <- dataset$year[i]
  site <- dataset$site[i]
  recording <- dataset$recording[i]
  start_s <- dataset$start_s[i]
  end_s <- dataset$end_s[i]
  
  
  song <- readWave(paste0(dir, "/", year, "_passerine", "/", site, "/", recording, ".wav"), 
                   from = start_s - 1, 
                   to = end_s + 1, 
                   units = "seconds")
  
  print(paste0("This is recording ", recording, " from ", start_s, " to ", end_s))
  play(song, ... = "/play /close")
}


