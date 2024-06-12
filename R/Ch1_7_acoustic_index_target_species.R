###
### Author: Sunny, 2024 Jun 03
### Question: Calculate the acoustic indices for each target species
###

# library -----------------------------------------------------------------

library(tidyverse)
library(here)

library(tuneR)
library(seewave)

library(ohun)
library(warbleR)
library(soundecology)


# functions ---------------------------------------------------------------

# calculates the aci values for single species (n values)
aci_values <- function(data, dir) { # path to the hard drive where the Audio was stored
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
                      from = start_s - 10, 
                      to = end_s + 10, 
                      units = "seconds")
    
    # calculate of the acoustic index
    soundfile_aci <- acoustic_complexity(sound)
    
    # save the values as a vector
    aci_values <- c(aci_values, soundfile_aci$AciTotAll_left)
  }
  return(aci_values) 
}

# calculate the metrics for single species
metric_values <- function(data, dir) {
  
  set.seed(12)
  
  detection_full <- tibble()
  for (i in 1:nrow(data)) {
    year <- data$year[i]
    site <- data$site[i]
    recording <- data$recording[i]
    start_s <- data$start_s[i]
    end_s <- data$end_s[i]
    
    sound <- readWave(paste0(dir, "/", year, "_passerine", "/", site, "/", recording, ".wav"), 
                      from = start_s - 3, 
                      to = end_s + 3, 
                      units = "seconds")
    
    writeWave(sound, file.path(tempdir(), "sound.wav"))
    
    
    # create the selection table for the target sound
    detection <- energy_detector(
      files = "sound.wav",
      path = tempdir(),
      bp = c(0.5, 9),
      threshold = 25,
      smooth = 150,
      hold.time = 200,
      min.duration = 100)
    
    # plot spectrogram and envelope
    # png(file = here("docs", "figures", "detection_plot.png"),
    #     width = 20, height = 14, units = "cm", res = 300)
    # 
    # label_spectro(wave = sound,
    #               envelope = TRUE,
    #               detection = detection,
    #               threshold = 25,
    #               fastdisp = TRUE)
    # 
    # dev.off()
    
    # get number of inflections
    inflections <- freq_ts(X = detection, 
                           path = tempdir(),
                           pb = TRUE,
                           length.out = 20,
                           flim = c(0.5, 9)) %>%
      inflections()
    
    # get frequency range
    bandwidth <- freq_range(X = detection, 
                            path = tempdir(), 
                            flim = c(0.5, 9)) %>%
      mutate(bandwidth = top.freq - bottom.freq) 
    
    # cleaning up the results
    detection_recording <- detection %>% 
      as_tibble() %>%
      left_join(inflections, by = join_by(sound.files == sound.files, selec == selec)) %>%
      left_join(bandwidth, by = join_by(sound.files == sound.files, selec == selec)) %>%
      select(duration.x, inflections, bandwidth) %>%
      summarise(across(1:3, mean))
    
    detection_full <- bind_rows(detection_full, detection_recording)
  }
  return(detection_full)
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

set.seed(12)

bird_data_target <- bird_data %>%
  filter(common_name %in% target_species) %>%
  slice_max(order_by = confidence, 
            n = 50,
            by = common_name,
            with_ties = FALSE)



# calculation of the ACI --------------------------------------------------

# aci_values_target <- bird_data_target %>%
#   group_nest(common_name, scientific_name) %>%
#   mutate(aci = map(.x = data,
#                    .f =~ aci_values(data = .x, dir = "E:/Audio")))
#
# save(aci_values_target, file = here("R", "aci_values_target.rda"))

load(here("R", "aci_values_target.rda"))

aci_stats_target <- aci_values_target %>%
  mutate(mean_aci = map_dbl(.x = aci, .f =~ mean(.x)),
         se_aci = map_dbl(.x = aci, .f =~ sd(.x)/50)) %>%
  select(-data, -aci) %>%
  arrange(mean_aci)



# calculation of the metrics for the sound --------------------------------

# metrics_values_target <- bird_data_target %>%
#   group_nest(common_name, scientific_name) %>%
#   mutate(metrics = map(.x = data, .f =~ metric_values(data = .x, dir = "E:/Audio")))
#   
# 
# complexity_metrics <- metrics_values_target %>%
#   select(-data) %>%
#   unnest(metrics)  
# 
# save(complexity_metrics, file = here("R", "complexity_metrics.rda"))

load(here("R", "complexity_metrics.rda"))

test <- ggbetweenstats(
  data = complexity_metrics,
  x = common_name,
  y = duration.x
)


metrics_plot <- complexity_metrics %>%
  pivot_longer(names_to = "metrics", 
               cols = c(duration.x, inflections, bandwidth)) %>%
  ggplot(aes(x = common_name, y = value)) +
  geom_boxplot() +
  geom_jitter() +
  
  
metrics_plot











