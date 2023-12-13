



# library -----------------------------------------------------------------
library(tidyverse)
library(terra)
library(sf)

library(terra)
library(ggmap)
library(leaflet)
library(tidyterra)
library(ggspatial)
library(cowplot)



# load files --------------------------------------------------------------
grid_JPRF <- vect(here("data", "map", "JPRF Grid"))
grid_North <- vect(here("data", "map", "North Grid Jprf"))

test <- read_csv(here("data", "map", "JPRF_all_sites.csv")) %>%
  st_as_sf(coords = c(X, Y), crs = )
