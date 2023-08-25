###
### Name: main_analysis
### 
### Author: Sunny Tseng
### Date: 2022-10-18
###


###
### Library
###
library(tidyverse)
library(here)


###
### Q: What's the min recordings one should select to achieve similar threshold result? 
### A1: Seems like there is no much difference between n = 50 and n =10. Selecting 10 is enough for future evaluation
### A2: Set the threshold as 0.625 can achieve Precision > 90% and Recall > 50%
###
data <- read_csv(here("Ch1_BirdNET", "data", "2020_Olive-sided Flycatcher_finished.csv"))

# calculate precision and recall for different datasets
evaluation_full <- NULL
for (num_recordings in c(10, 20, 30, 40, 50)){
  
  evaluation <- NULL
  for (threshold in seq(0.125, 0.975, length.out = 18)){
    
    data_sub <- data %>%
      group_by(category) %>%
      sample_n(num_recordings) %>%
      ungroup()
    
    true_positive <- (data_sub$confidence > threshold & data_sub$observation == "Y") %>% sum()
    false_positive <- (data_sub$confidence > threshold & data_sub$observation == "N") %>% sum()
    false_negative <- (data_sub$confidence < threshold & data_sub$observation == "Y") %>% sum()
    
    precision <- true_positive/(false_positive + true_positive)
    recall <- true_positive/(false_negative + true_positive)
    
    evaluation <- bind_rows(evaluation, tibble(threshold, precision, recall))
  }
  evaluation <- evaluation %>%
    mutate(num_recordings = num_recordings * 18)
  evaluation_full <- rbind(evaluation_full, evaluation)
}

# plot all the precision and recall curves together - see no change
threshold_curve <- evaluation_full %>%
  pivot_longer(cols = c(precision, recall)) %>%
  ggplot(aes(x = threshold, 
             y = value, 
             colour = name,
             alpha = num_recordings,
             group = interaction(name, num_recordings))) +
  stat_smooth(aes(alpha = num_recordings), 
              geom = "line", se = FALSE, size = 1.2) +
  geom_point(size = 2) +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text  = element_text(size = 12)) +
  labs(x = "Threshold",
       y = "Precision / Recall",
       alpha = "# of recordings",
       colour = "Type")
threshold_curve


###
### Q: What's the best threshold for all species? 
### A1: Most of them are 0.125! Only OSFL is 0.475
###

species <- "Orange-crowned Warbler"

data <- read_csv(here("Ch1_BirdNET", "data", paste0("2020_", species, "_finished.csv")))














species_list <- c("White-throated Sparrow",
                  "Olive-sided Flycatcher",
                  "Orange-crowned Warbler",
                  "Pacific Wren",
                  "Ruby-crowned Kinglet",
                  "Swainson's Thrush",
                  "Western Wood-Pewee")

# combine all the precision, recall values together from all species
evaluation_full <- NULL
for (species in species_list){
  data <- read_csv(here("Ch1_BirdNET", "data", paste0("2020_", species, "_finished.csv")))
  
  evaluation <- NULL#tibble(threshold = 0.01, precision = 0, recall = 1)
  for (threshold in seq(0.125, 0.975, length.out = 18)){
    true_positive <- (data$confidence > threshold & data$observation == "Y") %>% sum()
    false_positive <- (data$confidence > threshold & data$observation == "N") %>% sum()
    false_negative <- (data$confidence < threshold & data$observation == "Y") %>% sum()
    
    precision <- true_positive/(false_positive + true_positive)
    recall <- true_positive/(false_negative + true_positive)
    
    evaluation <- bind_rows(evaluation, tibble(threshold, precision, recall))
  }
  evaluation <- evaluation %>%
    mutate(species = species)
  evaluation_full <- rbind(evaluation_full, evaluation)
}

# plot the precision, recall, threshold values together from all species
ROC_curve <- evaluation_full %>%
  ggplot(aes(x = recall, 
             y = precision, 
             alpha = threshold)) +
    geom_line(size = 1.5) +
    geom_hline(yintercept = 0.5, linetype = 2) +
    facet_wrap(~ species) +
    ylim(0, 1) +
    theme_bw() +
    theme(axis.title = element_text(size = 16),
          axis.text  = element_text(size = 8)) +
    labs(x = "Recall",
         y = "Precision",
         alpha = "Threshold")
ROC_curve


###
### Q: What's the trend of these target species?
###
data_2020 <- read_csv(here("data", "processed", "2020_passerine_BirdNET.csv"))
data_2021 <- read_csv(here("data", "processed", "2021_passerine_BirdNET.csv"))

data <- rbind(data_2020, data_2021)
base <- tibble(year = c(rep(2020, 100), rep(2021, 100)), julian = rep(seq(from = 121, to = 220), 2))

data_OSFL <- data %>%
  filter(common_name == "Olive-sided Flycatcher",
         confidence >= 0.475) %>%
  unite(julian, year:day, remove = FALSE, sep = "") %>%
  mutate(julian = ymd(julian) %>% yday()) %>%
  group_by(year, julian) %>%
  summarise(count = n(),
            sites = n_distinct(site)) %>%
  mutate(count_effective = count/sites) %>%
  right_join(base, by = c("year", "julian")) %>%
  ggplot(aes(x = julian, y = count_effective, colour = year, group = year)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, formula = y ~ x) +
    theme_bw()
data_OSFL



###
### Others
###
calibration <- data %>%
  select(start_s, end_s, category, confidence, observation) %>%
  mutate(score = if_else(observation == "Y", 0.02, 0)) %>%
  group_by(category) %>%
  summarize(observed = sum(score)) %>%
  mutate(predicted = seq(0.125, 0.975, length.out = 18)) %>%
  ggplot(aes(x = predicted, y = observed)) +
  geom_point(size = 5) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_bw() +
  labs(title="OSFL predicted versus observed value", x = "Predicted", y = "Observed propotion (%)") +
  theme(plot.title = element_text(size = 15),
        axis.title = element_text(size = 15))



