

# library -----------------------------------------------------------------
library(tidyverse)
library(here)
library(terra) ## to read in data
library(sf) ## to read in csv and transfer to terra object
library(ggmap) ## for mapping

library(leaflet)
library(tidyterra)
library(ggspatial)
library(cowplot)

# load files --------------------------------------------------------------

## site location, a total of 66 sites, in WGS84
sites <- read_csv(here("data", "map", "JPRF_all_sites.csv")) %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(4326)) %>%
  vect() 

## JPRF grid, reprojected to WGS84
grid_JPRF <- vect(here("data", "map", "JPRF Grid")) %>%
  project(sites)

## North grid, reprojected to WGS84
grid_North <- vect(here("data", "map", "North Grid Jprf")) %>%
  project(sites)


# base R ------------------------------------------------------------------

plot(sites)
plot(grid_JPRF, col = "red", alpha = 0.2, add = TRUE)
plot(grid_North, col = "green", alpha = 0.2, add = TRUE)  
  

# using ggmap package -----------------------------------------------------

## get base map
aoi <- sites %>% 
  # add a buffer around the actual camera points
  # so the the base map extends beyond them (in meters)
  buffer(6000) %>% 
  # get the extent of this area (xmin, xmax, ymin, ymax)
  ext()

extent <- as.vector(aoi)

register_stadiamaps("483bb38f-7ba0-4c89-b2f5-fc8310057ab4", write = FALSE)

basemap <- get_stadiamap(location = c(extent[[1]], extent[[3]],
                                      extent[[2]], extent[[4]]), 
                         maptype = "stamen_terrain")












  
  
  
