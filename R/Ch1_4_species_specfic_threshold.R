


# Library -----------------------------------------------------------------

library(tidyverse)
library(data.table)
library(here)
library(spatstat.utils)
library(patchwork)
library(RColorBrewer)

g1_plot <- function(data, species){
  coul <- brewer.pal(10, "Set3") 
  g1 <- data %>%
    pivot_longer(cols = c(TP_cum, FP_cum), names_to = "type") %>%
    ggplot() +
    geom_bar(aes(fill = type, y = value, x = category_dbl), 
             position = "stack", 
             stat = "identity") +
    ggtitle(species) +
    scale_fill_manual(values = coul[c(6, 1)], 
                      labels = c("False Positive", "True Positive")) +
    theme_bw() +
    labs(x = "Confidence threshold", 
         y = "Proportion of BirdNET detections (%)") +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 11),
          legend.position = c(0.80, 0.80),
          title = element_text(size = 14))
  
  return(g1)
}

# Import data -------------------------------------------------------------

data_validation <- list.files(here("data"), pattern = "*finished.csv") %>%
  map_df(~read_csv(paste0("./data/", .)))

data_2020 <- read_csv(here("data", "processed", "2020_passerine_BirdNET.csv"))
data_2021 <- read_csv(here("data", "processed", "2021_passerine_BirdNET.csv"))
data_all <- bind_rows(data_2020, data_2021) %>%
  mutate(category = cut(x = confidence, breaks = seq(0.1, 1, 0.05))) 



# Visualization: all species curves ---------------------------------------

rate <- data_validation %>%
  group_nest(common_name, scientific_name, category) %>%
  mutate(rate = map_dbl(.x = data, .f =~ sum(.x$observed == "Y")/nrow(.x))) %>%
  separate(col = category, into = c("from", "to"), sep = ",") %>%
  mutate(from = str_extract(from, pattern = "[^\\(]+") %>% as.numeric(),
         to = str_extract(to, pattern = "[^\\]]+") %>% as.numeric()) %>%
  mutate(category_dbl = (from + to)/2) %>%
  select(common_name, scientific_name, category_dbl, rate)

coul <- brewer.pal(12, "Paired") 
coul <- colorRampPalette(coul)(19)
g <- ggplot(rate, aes(x = category_dbl, 
                           y = rate, 
                           group = common_name, 
                           colour = common_name)) +
  geom_point(size = 3, 
             alpha = 0.2) +
  geom_line(stat = "smooth",
            method = "loess", # span is 0.75 by default
            se = FALSE, 
            size = 1.5,
            alpha = 0.7) + 
  scale_colour_manual(values = coul) +
  ylim(0, 1) +
  xlim(0, 1) +
  theme_bw() +
  labs(x = "BirdNET confidence", 
       y = "Proportion of true positive",
       colour = "Species") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = "bottom")
g


# Visualization: single species  ------------------------------------------

rate_loess <- data_validation %>%
  group_nest(common_name, scientific_name, category) %>%
  mutate(rate = map_dbl(.x = data, .f =~ sum(.x$observed == "Y")/nrow(.x))) %>%
  separate(col = category, into = c("from", "to"), sep = ",") %>%
  mutate(from = str_extract(from, pattern = "[^\\(]+") %>% as.numeric(),
         to = str_extract(to, pattern = "[^\\]]+") %>% as.numeric()) %>%
  mutate(category_dbl = (from + to)/2) %>%
  group_nest(common_name) %>%
  mutate(rate_loess = map(.x = data, .f =~ loess(rate ~ category_dbl, data = .x) %>%
                            predict() %>%
                            ifelse(. > 1, 1, .))) %>%
  unnest(cols = c(data, rate_loess)) %>%
  select(common_name, scientific_name, category_dbl, rate_loess)


rate_loess_count <- data_all %>%
  count(common_name, scientific_name, category) %>%
  separate(col = category, into = c("from", "to"), sep = ",") %>%
  mutate(from = str_extract(from, pattern = "[^\\(]+") %>% as.numeric(),
         to = str_extract(to, pattern = "[^\\]]+") %>% as.numeric()) %>%
  mutate(category_dbl = (from + to)/2) %>%
  left_join(rate_loess) %>%
  drop_na(rate_loess) %>% 
  mutate(TP = n * rate_loess,
         FP = n * (1 - rate_loess)) %>%
  group_by(common_name) %>%
  mutate(TP_cum = revcumsum(TP)/sum(n)*100,
         FP_cum = revcumsum(FP)/sum(n)*100)


g1_list <- rate_loess_count %>%
  group_nest(common_name, scientific_name) %>%
  mutate(g1 = map2(.x = data, .y = common_name, .f =~ g1_plot(data = .x, species = .y)))


(g1_list$g1[[1]] | g1_list$g1[[2]] | g1_list$g1[[3]]) / 
  (g1_list$g1[[4]] | g1_list$g1[[5]] | g1_list$g1[[6]])

(g1_list$g1[[7]] | g1_list$g1[[8]] | g1_list$g1[[9]]) / 
  (g1_list$g1[[10]] | g1_list$g1[[11]] | g1_list$g1[[12]])

(g1_list$g1[[13]] | g1_list$g1[[14]] | g1_list$g1[[15]]) / 
  (g1_list$g1[[16]] | g1_list$g1[[17]] | g1_list$g1[[18]])

g1_list$g1[[19]]



level_patch <- g1_list$g1[[19]] / g1_list$g1[[7]] / g1_list$g1[[15]] &
  # plot_annotation(tag_levels = 'A') & 
  theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5)) &
  ylab(NULL)

wrap_elements(level_patch) +
  labs(tag = "Proportion of BirdNET detections (%)") +
  theme(plot.tag = element_text(size = 16, angle = 90),
        plot.tag.position = "left")


level_patch <- (g1_list$g1[[19]] | g1_list$g1[[7]]) / (g1_list$g1[[2]] | g1_list$g1[[15]]) &
  plot_annotation(tag_levels = 'A') & 
  theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5)) &
  ylab(NULL)

wrap_elements(level_patch) +
  labs(tag = "Proportion of BirdNET detections (%)") +
  theme(plot.tag = element_text(size = 16, angle = 90),
        plot.tag.position = "left")

# Others ------------------------------------------------------------------
species_vector <- test1 %>% 
  pull(common_name) %>%
  unique()

for (species in species_vector) {
  g <- test1 %>%
    filter(common_name == species) %>%
    pivot_longer(cols = c(true_positive, false_positive), names_to = "type") %>%
    ggplot() +
    geom_bar(aes(fill = type, y = value, x = category_dbl), position = "fill", stat = "identity") +
    geom_smooth(aes(x = category_dbl, y = rate_loess), method = "loess", se = FALSE, size = 1.2) +
    # geom_bar(aes(y = n, x = category_dbl)) +
    ggtitle(species) +
    theme_bw()
  
  print(g)
}





