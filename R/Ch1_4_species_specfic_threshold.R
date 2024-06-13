


# Library -----------------------------------------------------------------

library(tidyverse)
library(data.table)
library(here)
library(spatstat.utils)
library(patchwork)
library(grid)
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
         y = "Remaining BirdNET detections (%)") +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 11),
          legend.position = c(0.80, 0.80),
          title = element_text(size = 14))
  
  return(g1)
}

g1_plot_1 <- function(data, species){
  coul <- brewer.pal(10, "Set3") 
  
  threshold <- data %>%
    filter(rate_cum > 0.90) %>%
    slice_min(category_dbl) %>%
    pull(category_dbl)
  
  
  g1 <- data %>%
    pivot_longer(cols = c(TP_cum, FP_cum), names_to = "type") %>%
    ggplot() +
    geom_bar(aes(fill = type, y = value, x = category_dbl), 
             position = "stack", 
             stat = "identity") +
    #geom_point(aes(y = rate_cum*100, x = category_dbl)) +
    #geom_line(aes(y = rate_cum*100, x = category_dbl)) +
    geom_segment(x = threshold, y = 30, xend = threshold, yend = 10, 
                 colour = "red", linewidth = 1.2, size = 1,
                 arrow = arrow(length = unit(0.1, "inches"))) + 
    geom_segment(x = 0.725, y = 30, xend = 0.725, yend = 10, 
                 colour = "slateblue4", linewidth = 1.2, size = 1,
                 arrow = arrow(length = unit(0.1, "inches"))) + 
    # geom_vline(xintercept = threshold, colour = "red", linetype = "dashed", linewidth = 1.2) +
    # geom_vline(xintercept = 0.75, colour = "grey", linetype = "dashed", linewidth = 1.2) +
    ggtitle(species) +
    scale_fill_manual(values = coul[c(6, 1)], 
                      labels = c("False Positive", "True Positive")) +
    theme_bw() +
    labs(x = "Confidence threshold", 
         y = "Remaining BirdNET detections (%)") +
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
  map_df(~ read_csv(paste0("./data/", .)))

data_2020 <- read_csv(here("data", "processed", "2020_passerine_BirdNET_updated.csv"))
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
  select(-data)

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
            linewidth = 1.5,
            alpha = 0.7) + 
  scale_colour_manual(values = coul) +
  scale_x_continuous(limits = c(0.1, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme_bw() +
  labs(x = "BirdNET confidence", 
       y = "True positive rate",
       colour = "Species") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  guides(colour = guide_legend(ncol = 4)) 


ggsave(plot = g,
       filename = here("docs", "figures", "calibration_curves.PNG"),
       width = 28,
       height = 20,
       units = "cm", 
       dpi = 300)



# Visualization: single species  ------------------------------------------

rate_loess <- rate %>%
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
  drop_na(rate_loess) %>% # to keep only target species
  mutate(TP = n * rate_loess,
         FP = n * (1 - rate_loess)) %>%
  group_by(common_name) %>%
  mutate(TP_cum = revcumsum(TP)/sum(n)*100, # total number of true positives after applying threshold
         FP_cum = revcumsum(FP)/sum(n)*100, # total number of false positives after applying threshold
         rate_cum = TP_cum/(TP_cum + FP_cum))


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


g1_list_1 <- rate_loess_count %>%
  group_nest(common_name, scientific_name) %>%
  mutate(g1 = map2(.x = data, 
                   .y = common_name, 
                   .f =~ g1_plot_1(data = .x, species = .y)))


level_patch <- (g1_list_1$g1[[19]] | g1_list_1$g1[[7]]) / (g1_list_1$g1[[2]] | g1_list_1$g1[[15]]) &
  plot_annotation(tag_levels = 'A') & 
  theme(plot.margin = margin(5.5, 5.5, 5.5, 8),
        plot.tag.position = c(0, 0.98)) &
  ylab(NULL) &
  xlab(NULL) 


level_patch_1 <- patchwork::patchworkGrob(level_patch) %>%
  gridExtra::grid.arrange(., 
                          #right = grid.text("Proportion of true positives (%)", rot = -90, gp = gpar(fontsize=18)),
                          left = grid.text("Remaining BirdNET detections (%)", rot = 90, gp = gpar(fontsize=18)), 
                          bottom = grid.text("Confidence threshold", gp = gpar(fontsize=18)))

ggsave(plot = level_patch_1,
       filename = here("docs", "figures", "threshold_setting.png"),
       width = 24,
       height = 18,
       units = "cm",
       dpi = 300)




# threshold setting table for fixing precision ----------------------------

thresholds_table_s2 <- rate_loess_count %>%
  filter(rate_cum > 0.90) %>%
  group_by(common_name, scientific_name) %>%
  slice_min(category_dbl) %>%
  mutate(proportion_detection = TP_cum + FP_cum) %>%
  select(common_name, scientific_name, from, rate_cum, proportion_detection) %>%
  mutate(proportion_detection = round(proportion_detection, digits = 0),
         rate_cum = round(rate_cum, digits = 2)) %>%
  arrange(from, rate_cum, proportion_detection)
  
# write_csv(thresholds_table_s2, 
#           file = here("docs", "tables", "thresholds_table_s2.csv"))



# precision evaluation for fixed threshold --------------------------------
thresholds_table_s1 <- rate_loess_count %>%
  filter(from == 0.7) %>%
  mutate(proportion_detection = TP_cum + FP_cum) %>%
  select(common_name, scientific_name, from, rate_cum, proportion_detection) %>%
  mutate(proportion_detection = round(proportion_detection, digits = 0),
         rate_cum = round(rate_cum, digits = 2)) %>%
  arrange(from, desc(rate_cum), proportion_detection)

# write_csv(thresholds_table_s1, 
#           file = here("docs", "tables", "thresholds_table_s1.csv"))


