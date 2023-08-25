
#################################################################
### Goal: Summarize the audio files that collected from JPRF 2020
###       and create a dataframe for metadata
### Author: Sunny Tseng
### Date: 2021/09/20
################################################################

###
### Library
###
library(tidyverse)
library(here)


###
### Recording info
###
recordings <- list.files(path = here("data", "JPRF_audio_moth_2020_Lauren"), 
                         pattern = "\\.WAV$", all.files = FALSE, recursive = TRUE, full.names = FALSE) %>%
  as_tibble() %>%
  rename(fullPath = value) %>%
  separate(col = fullPath, into = c("site", "recorder", "time"), sep = "\\/", remove = FALSE) %>%
  mutate(recorder = str_extract(recorder, pattern = "[:graph:]+"),
         year = str_sub(time, 1, 4) %>% as.numeric(),
         month = str_sub(time, 5, 6) %>% as.numeric(),
         day = str_sub(time, 7, 8) %>% as.numeric(),
         hour = str_sub(time, 10, 11) %>% as.numeric(),
         min = str_sub(time, 12, 13) %>% as.numeric(),
         sec = str_sub(time, 14, 15) %>% as.numeric()) %>%
  select(site, recorder, year, month, day, hour, min, sec, fullPath)
  

###
### Site info
###
sites <- list.dirs(here("data"), recursive = TRUE, full.names = FALSE) %>%
  as_tibble() %>%
  rename(original = value) %>%
  separate(col = original, into = c("site", "recorder"), sep = "\\/", remove = FALSE) %>%
  drop_na(recorder) %>%
  mutate(recorder = str_extract(recorder, pattern = "[:graph:]+"))

sites_add <- recordings %>%
  group_nest(site, recorder) %>%
  mutate(no = map_dbl(.x = data, .f =~ .x %>% nrow()),
         start_year = map_dbl(.x = data, .f =~ .x$year[1]),
         start_month = map_dbl(.x = data, .f =~ .x$month[1]),
         start_day = map_dbl(.x = data, .f =~ .x$day[1]),
         end_year = map_dbl(.x = data, .f =~ .x$year %>% tail(1)),
         end_month = map_dbl(.x = data, .f =~ .x$month %>% tail(1)),
         end_day = map_dbl(.x = data, .f =~ .x$day %>% tail(1))) %>%
  select(-data)

sites_1 <- sites %>%
  left_join(sites_add, by = c("site", "recorder")) %>%
  mutate(no = if_else(is.na(no), 0, no)) %>%
  select(site, recorder, no, start_year, start_month, start_day, end_year, end_month, end_day)
      


###
### Summary of the data
###

recordings <- read_csv(here("data", "processed", "2022_passerine_BirdNET.csv"))

ARUs <- recordings %>%
  group_by(site, recording) %>%
  summarize(n = n())

species <- recordings %>%
  filter(confidence >= 0.85) %>%
  group_by(common_name) %>%
  summarize(mean_confidence = mean(confidence))

write_csv(species, here("species_list_above85_2022.csv"))




