###
### Author: Sunny, 2023 Aug 24
### Question: What's the min recordings one should select to achieve similar 
### threshold result? 
###

# Library -----------------------------------------------------------------
library(tidyverse)
library(here)
library(patchwork)
library(png)

pr <- function(data = data, n_vector = c(10, 20, 30, 40, 50)){
  pr_full <- NULL
  for (num_recordings in n_vector){
    
    evaluation <- NULL
    for (threshold in seq(0.125, 0.975, length.out = 18)){
      
      data_sub <- data %>%
        group_by(category) %>%
        sample_n(num_recordings) %>%
        ungroup()
      
      true_positive <- (data_sub$confidence > threshold & data_sub$observed == "Y") %>% sum()
      false_positive <- (data_sub$confidence > threshold & data_sub$observed == "N") %>% sum()
      false_negative <- (data_sub$confidence < threshold & data_sub$observed == "Y") %>% sum()
      
      precision <- true_positive/(false_positive + true_positive)
      recall <- true_positive/(false_negative + true_positive)
      
      evaluation <- bind_rows(evaluation, tibble(threshold, precision, recall))
    }
    evaluation <- evaluation %>%
      mutate(num_recordings = num_recordings * 18)
    pr_full <- rbind(pr_full, evaluation)
  }
  
  return(pr_full)
}


proportion <- function(data = data, n_vector = c(5, 10, 25, 50), bootstrap = 100){
  set.seed(70)
  data_proportion_2 <- NULL
  for (num_recordings in n_vector) {
    
    data_proportion_1 <- NULL
    for (i in 1:bootstrap) {
      data_proportion <- data %>%
        group_by(category) %>%
        sample_n(num_recordings) %>%
        ungroup() %>%
        group_nest(category) %>%
        mutate(rate = map_dbl(.x = data, .f =~ sum(.x$observed == "Y")/nrow(.x))) %>%
        separate(col = category, into = c("from", "to"), sep = ",") %>%
        mutate(from = str_extract(from, pattern = "[^\\(]+") %>% as.numeric(),
               to = str_extract(to, pattern = "[^\\]]+") %>% as.numeric()) %>%
        mutate(category_dbl = (from + to)/2) %>%
        mutate(num_recordings = num_recordings) %>%
        select(num_recordings, category_dbl, rate)
      
      data_proportion_1 <- bind_rows(data_proportion_1, data_proportion)
    }
    
    data_proportion_1_sum <- data_proportion_1 %>%
      group_nest(num_recordings, category_dbl) %>%
      mutate(rate_mean = map_dbl(.x = data, .f =~ .x$rate %>% mean()),
             rate_low = map_dbl(.x = data, .f =~ .x$rate %>% min()),
             rate_high = map_dbl(.x = data, .f =~ .x$rate %>% max()),
             rate_se = map_dbl(.x = data, .f =~ sd(.x$rate)/sqrt(length(.x$rate)))) %>%
      select(-data)
    
    data_proportion_2 <- bind_rows(data_proportion_2, data_proportion_1_sum)
  }
  return(data_proportion_2)
}


# Import data -------------------------------------------------------------
data <- read_csv(here("data", 
                      "number_evaluation",
                      "Yellow_rumped Warbler_finished.csv"))


# V1: Precision recall curve ----------------------------------------------
pr <- pr(data)

g1 <- pr %>%
  pivot_longer(cols = c(precision, recall)) %>%
  ggplot(aes(x = threshold, 
             y = value, 
             colour = name,
             alpha = num_recordings,
             group = name
             )) +
  stat_smooth(aes(alpha = num_recordings), 
              geom = "line", se = FALSE, size = 1.2) +
  geom_point(size = 2) +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text  = element_text(size = 12)) +
  labs(x = "Threshold",
       y = "Precision / Recall",
       alpha = "# of recordings",
       colour = "Type") +
  facet_grid(num_recordings~1)
g1


# V2: Calibration curve with confidence interval --------------------------
pro_100 <- proportion(data = data, 
                      n_vector = c(5, 10, 25, 50), 
                      bootstrap = 100)

g3 <- pro_100 %>%
  mutate(upper = rate_mean + 10*rate_se,
         lower = rate_mean - 10*rate_se) %>%
  mutate(num_recordings_f = as_factor(num_recordings)) %>%
  ggplot(aes(x = category_dbl, group = num_recordings_f)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  #geom_point(aes(size = num_recordings, colour = num_recordings_f), 
  #           alpha = 0.3,
  #           shape = 20)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#00AFFB")) +
  #scale_size(range = c(5, 25)) +
  theme_bw()
g3  
  


# V3: Calibration curve with 1 random sampling ----------------------------

### OSFL 
data <- read_csv(here("data", 
                      #"number_evaluation",
                      "2020_Olive-sided Flycatcher_finished.csv"))
pro_1 <- proportion(data = data, 
                    n_vector = c(5, 10, 25, 50), 
                    bootstrap = 1)
g_OSFL <- pro_1 %>%
  mutate(num_recordings_f = as_factor(num_recordings)) %>%
  ggplot(aes(x = category_dbl, 
             y = rate_mean, 
             colour = num_recordings_f,
             group = num_recordings_f)) +
  geom_point(aes(size = num_recordings), 
             alpha = 0.3,
             shape = 20) +
  geom_line(stat = "smooth",
            method = "glm", 
            se = FALSE, 
            method.args = list(family = binomial),
            linewidth = 1.5,
            alpha = 0.5) +
  # geom_line(stat = "smooth", method = "loess", 
  #           se = FALSE, alpha = 0.5, size = 1.5) +
  scale_color_manual(values = c("#999999", "#E7B800","#FC4E07", "#00AFFB")) +
  scale_size(range = c(5, 25)) +
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.3)) +
  theme_bw() +
  theme(legend.position = "none", 
        legend.justification = NULL,
        legend.box.just = NULL,
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  labs(title = "Olive-sided Flycatcher",
       x = NULL, 
       y = NULL,
       colour = NULL,
       size = NULL) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        #legend.position = c(0.8, 0.5),
        legend.justification = c("left", "top"))


### YRWA
data <- read_csv(here("data", 
                      "number_evaluation",
                      "Yellow_rumped Warbler_finished.csv"))

pro_1 <- proportion(data = data, 
                    n_vector = c(5, 10, 25, 50), 
                    bootstrap = 1)

g_YRWA <- pro_1 %>%
  mutate(num_recordings_f = as_factor(num_recordings)) %>%
  ggplot(aes(x = category_dbl, 
             y = rate_mean, 
             colour = num_recordings_f,
             group = num_recordings_f)) +
  geom_point(aes(size = num_recordings), 
             alpha = 0.3,
             shape = 20) +
  # geom_line(stat = "smooth", method = "loess", 
  #           se = FALSE, alpha = 0.5, size = 1.5) +
  geom_line(stat = "smooth",
            method = "glm", 
            se = FALSE, 
            method.args = list(family = binomial),
            linewidth = 1.5,
            alpha = 0.5) +
  scale_color_manual(values = c("#999999", "#E7B800","#FC4E07", "#00AFFB")) +
  scale_size(range = c(5, 25)) +
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.3)) +
  theme_bw() +
  theme(legend.position = "none", 
        legend.justification = NULL,
        legend.box.just = NULL,
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  labs(title = "Myrtle Warbler",
       x = NULL, 
       y = NULL,
       colour = NULL,
       size = NULL) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        #legend.position = c(0.8, 0.5),
        legend.justification = c("left", "top"))


### VATH
data <- read_csv(here("data", 
                      "number_evaluation",
                      "Varied Thrush_finished.csv"))

pro_1 <- proportion(data = data, 
                    n_vector = c(5, 10, 25, 50), 
                    bootstrap = 1)

g_VATH <- pro_1 %>%
  mutate(num_recordings_f = as_factor(num_recordings)) %>%
  ggplot(aes(x = category_dbl, 
             y = rate_mean, 
             colour = num_recordings_f,
             group = num_recordings_f)) +
  geom_point(aes(size = num_recordings), 
             alpha = 0.3,
             shape = 20) +
  # geom_line(stat = "smooth", method = "loess", 
  #           se = FALSE, alpha = 0.5, size = 1.5) +
  geom_line(stat = "smooth",
            method = "glm", 
            se = FALSE, 
            method.args = list(family = binomial),
            linewidth = 1.5,
            alpha = 0.5) +
  scale_color_manual(values = c("#999999", "#E7B800","#FC4E07", "#00AFFB")) +
  scale_size(range = c(5, 25)) +
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.3)) +
  theme_bw() +
  theme(legend.position = "none", 
        legend.justification = NULL,
        legend.box.just = NULL,
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  labs(title = "Varied Thrush",
       x = "BirdNET confidence", 
       y = NULL,
       colour = NULL,
       size = NULL) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        #legend.position = c(0.8, 0.5),
        legend.justification = c("left", "top"))

### legend
p_legend <- readPNG(here("docs", "figures", "number_evaluation_legend_2.PNG"),
                    native = TRUE)

# combine plot and add annotation
patch_plot <- (g_YRWA / g_OSFL / g_VATH) +
  plot_annotation(tag_levels = "A") &
  theme(plot.margin = margin(2, 2, 2, 5),
        plot.tag = element_text(size = 14))

# add shared y axis
patch_plot_1 <- wrap_elements(patch_plot) +
  labs(tag = "True positive rate") +
  theme(plot.tag = element_text(size = rel(1.5), angle = 90),
        plot.tag.position = "left") 

# add a legend, saved as as an image
patch_plot_2 <- patch_plot_1 + 
  inset_element(p = p_legend,
                left = 0.65,
                right = 0.95,
                top = 0.88,
                bottom = 0.73)

patch_plot_2

# save the final plot
ggsave(plot = patch_plot_2,
       filename = here("docs", "figures", "number_evaluation_1.png"),
       width = 20,
       height = 26, 
       units = "cm",
       dpi = 300)




