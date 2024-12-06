

# library -----------------------------------------------------------------

library(tidyverse)
library(here)
library(scales)


# import data -------------------------------------------------------------

data_2020 <- read_csv(here("data", "processed", "2020_passerine_BirdNET_updated.csv"))
data_2021 <- read_csv(here("data", "processed", "2021_passerine_BirdNET.csv"))
detection <- bind_rows(data_2020, data_2021) %>%
  mutate(category = cut(x = confidence, breaks = seq(0.1, 1, 0.05))) 

bc_bird <- read_csv(here("data", "bird_list", "species_bc_breeding_bird_atlas.csv")) %>%
  mutate(common_name = case_when(common_name == "Gray Jay" ~ "Canada Jay",
                                 common_name == "Pacific/Winter Wren" ~ "Pacific Wren",
                                 common_name == "Yellow-rumped Warbler (Audubon's)" ~ "Yellow-rumped Warbler",
                                 .default = common_name))
  


# species within the bc list ----------------------------------------------

bc_bird %>% 
  filter(!bc_bird$common_name %in% detection$common_name)

detection_bc_bird <- detection %>%
  filter(common_name %in% bc_bird$common_name)


final <- c()
for(threshold in seq(from = 0.1, to = 1, by = 0.02)){
  for(replicate in 1:10){
    richness <- slice_sample(detection_bc_bird, prop = 0.1) %>%
      filter(confidence >= threshold) %>%
      pull(common_name) %>%
      n_distinct()
    
    temp <- c(threshold, richness)
    final <- rbind(final, temp)
  }
}
richness_trend <- final %>%
  as_tibble() %>%
  rename(threshold = V1, richness = V2)

# plot the richness_trend as bubble plot

plot <- richness_trend %>%
  ggplot(aes(x = threshold, y = richness)) +
  geom_point(size = 5, alpha = 0.1, colour = "lavenderblush4") +
  geom_vline(xintercept = 0.7, linetype = "dashed", colour = "red", linewidth = 1.5) +
  geom_smooth(se = FALSE, colour = "black", linewidth = 1.2) +
  labs(x = "Confidence threshold",
       y = "Richness") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.3)) +
  theme_bw() +
  theme(legend.position = "none", 
        legend.justification = NULL,
        legend.box.just = NULL,
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  labs(x = "Confidence threshold", 
       y = "No. of detected species",
       colour = NULL,
       size = NULL) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))


ggsave(plot = plot,
       filename = here("docs", "figures", "richness_threshold.png"),
       width = 20,
       height = 12, 
       units = "cm",
       dpi = 300)



# stratified versus simple random sampling --------------------------------

NOFL_full <- detection %>%
  filter(common_name == "Northern Flicker") %>%
  mutate(id = row_number())

coul <- brewer.pal(10, "Set3") 

# stratified sampling
stratified <- NOFL_full %>%
  slice_sample(n = 10, by = category) %>%
  mutate(sampled = "A") %>%
  right_join(NOFL_full) %>%
  select(id, category, confidence, sampled) %>%
  mutate(sampled = if_else(is.na(sampled), "B", sampled)) %>%
  
  ggplot() +
  geom_bar(aes(fill = sampled, x = category), 
           position = "stack") +
  theme_bw() +
  scale_fill_manual(values = coul[c(6, 1)], 
                    labels = c("Sampled", "Not sampled")) +
  scale_x_discrete(labels = seq(0.1, 1, 0.05)) +
  labs(x = "Confidence value", 
       y = "No. of BirdNET observations") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = c(0.80, 0.80),
        title = element_text(size = 14))

stratified

# simple random sampling
simple <- NOFL_full %>%
  slice_sample(n = 180) %>%
  mutate(sampled = "A") %>%
  right_join(NOFL_full) %>%
  select(id, category, confidence, sampled) %>%
  mutate(sampled = if_else(is.na(sampled), "B", sampled)) %>%
  
  ggplot() +
  geom_bar(aes(fill = sampled, x = category), 
           position = "stack") +
  theme_bw() +
  scale_fill_manual(values = coul[c(6, 1)], 
                    labels = c("Sampled", "Not sampled")) +
  scale_x_discrete(labels = seq(0.1, 1, 0.05)) +
  labs(x = "Confidence value", 
       y = "No. of BirdNET observations") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = c(0.80, 0.80),
        title = element_text(size = 14))

simple
  

# comparing the method of deciding the threshold --------------------------





