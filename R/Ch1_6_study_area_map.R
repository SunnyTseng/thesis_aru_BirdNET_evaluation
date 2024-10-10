

# library -----------------------------------------------------------------
library(tidyverse)
library(here)
library(terra) ## to read in data
library(sf) ## to read in csv and transfer to terra object
library(ggmap) ## for getting basemap
library(tidyterra) # to add terra object to ggplot
library(ggspatial) ## to add features on the map
library(bcmaps) ## to get the outline of the BC maps
library(cowplot) ## to combine plots

library(png) ## to read in legend image
library(patchwork) ## to combine plots

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
  project(sites) %>%
  drop_na(site)


# base R ------------------------------------------------------------------

plot(sites)
plot(grid_JPRF, col = "red", alpha = 0.2, add = TRUE)
plot(grid_North, col = "green", alpha = 0.2, add = TRUE)  
  

# using ggmap package to get the main map ---------------------------------

## get base map
aoi <- sites %>% 
  # add a buffer around the actual camera points so the the base map extends beyond them (in meters)
  buffer(3000) %>% 
  ext()
extent <- as.vector(aoi)

register_stadiamaps("483bb38f-7ba0-4c89-b2f5-fc8310057ab4", write = FALSE)

basemap <- get_stadiamap(bbox = c(left = extent[[1]] - 0.1, 
                                  right = extent[[2]],
                                  bottom = extent[[3]], 
                                  top = extent[[4]]), 
                         maptype = "alidade_smooth")

main <- ggmap(basemap) + 
  geom_spatvector(data = grid_JPRF, inherit.aes = F, alpha = 0.1, fill = "lightgoldenrod3") +
  geom_spatvector(data = grid_North, inherit.aes = F, alpha = 0.1, fill = "lightgoldenrod3") +
  geom_spatvector(data = sites, inherit.aes = F) +
  theme_void() +
  # scale bar:
  annotation_scale(location = "bl", 
                   width_hint = 0.4) +
  # north arrow:
  annotation_north_arrow(style = north_arrow_fancy_orienteering, 
                         location = "bl",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))


# make inset map --------------------------------------------------------

# outline of BC
bc <- vect(bcmaps::bc_bound()) %>% 
  project("EPSG:4326")

# aoi as polygon
aoi <- as.polygons(aoi)  
crs(aoi) <- "EPSG:4326"

plot(bc)
plot(as.polygons(aoi), add = T)

inset <- ggplot() +
  geom_spatvector(data = bc, inherit.aes = F,
                  fill = "lightblue4", colour = "black") +
  geom_spatvector(data = aoi, fill = "lightgoldenrod1",
                  colour = "lightgoldenrod1", 
                  lwd = 1) + 
  theme_void() +
  theme(panel.background=element_rect(fill = "transparent", colour="antiquewhite4"))

# combine main map with inset map -----------------------------------------

p_legend <- readPNG(here("docs", "figures", "study_site_map_legend_2.PNG"),
                    native = TRUE)

final_map <- ggdraw() +
  draw_plot(main, x = 0, y = 0) +
  draw_plot(inset, x = 0, y = 0.63, width = 0.35, height = 0.35) +
  inset_element(p = p_legend,
                left = 0.02,
                right = 0.21,
                top = 0.37,
                bottom = 0.18) +
  theme(rect = element_rect(fill = "transparent", colour = "transparent"))

final_map

ggsave(final_map,
       filename = here("docs", "figures", "study_site_map_1.png"),
       width = 24,
       height = 16,
       units = "cm",
       dpi = 300)
  
  
  
