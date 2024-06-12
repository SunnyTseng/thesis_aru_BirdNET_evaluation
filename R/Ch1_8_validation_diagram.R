

# library -----------------------------------------------------------------

library(tidyverse)
library(here)


# data import -------------------------------------------------------------

data_2020 <- read_csv(here("data", "processed", paste0(2020, "_passerine_BirdNET_updated.csv")))
data_2021 <- read_csv(here("data", "processed", paste0(2021, "_passerine_BirdNET.csv")))

data_full <- bind_rows(data_2020, data_2021) %>%
  filter(common_name == "American Robin") %>%
  mutate(category = cut(x = confidence, breaks = seq(0.1, 1, 0.05)))

data_selected <- read_csv(here("data", "2020_American Robin_finished.csv"))


# figures -----------------------------------------------------------------

### full dataset

g_full <- data_full 
  
  
### selected dataset 
  



