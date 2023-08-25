library(seewave)
library(tuneR)
library(tidyverse)
library(here)

dir <- "E:/Audio/2021_passerine"
dataset <- read_csv(here("Ch1_BirdNET", "data", "2021_MacGillivray's Warbler_working.csv"))


for (i in 101:180) {
  site <- dataset$site[i]
  recording <- dataset$recording[i]
  start_s <- dataset$start_s[i]
  end_s <- dataset$end_s[i]
  
  
  song <- readWave(paste0(dir, "/", site, "/", recording, ".wav"), 
                   from = start_s - 1, 
                   to = end_s + 1, 
                   units = "seconds")
  
  print(paste0("This is recording ", recording, " from ", start_s, " to ", end_s))
  play(song, ... = "/play /close")
}


