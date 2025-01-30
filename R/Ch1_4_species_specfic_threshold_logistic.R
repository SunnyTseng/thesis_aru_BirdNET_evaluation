# Library -----------------------------------------------------------------

library(tidyverse)
library(data.table)
library(here)
library(spatstat.utils)
library(patchwork)
library(grid)
library(RColorBrewer)

g1_plot_1 <- function(data, species){
  coul <- brewer.pal(10, "Set3") 
  
  ## create outlier threshold if non of rate_cum is larger than 0.9
  
  if(all(data$rate_cum < 0.90)){
    threshold <- 0.975
  } else {
    threshold <- data %>%
      filter(rate_cum > 0.90) %>%
      slice_min(category_dbl) %>%
      pull(category_dbl)
  }
  
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
  map_df(~ read_csv(paste0("./data/", .))) %>%
  mutate(common_name = if_else(common_name == "Yellow-rumped Warbler", "Myrtle Warbler", common_name),
         common_name = if_else(common_name == "Pacific-slope Flycatcher", "Western Flycatcher", common_name)) 

data_2020 <- read_csv(here("data", "processed", "2020_passerine_BirdNET_updated.csv"))
data_2021 <- read_csv(here("data", "processed", "2021_passerine_BirdNET.csv"))
data_all <- bind_rows(data_2020, data_2021) %>%
  mutate(category = cut(x = confidence, breaks = seq(0.1, 1, 0.05))) %>%
  mutate(common_name = if_else(common_name == "Yellow-rumped Warbler", "Myrtle Warbler", common_name),
         common_name = if_else(common_name == "Pacific-slope Flycatcher", "Western Flycatcher", common_name))



# calibration curves ------------------------------------------------------

rate_1 <- data_validation %>%
  mutate(observed = ifelse(observed == "Y", 1, 0)) 

coul <- brewer.pal(12, "Paired") 
coul <- colorRampPalette(coul)(19)

g <- ggplot(rate_1, aes(x = confidence, 
                        y = observed, 
                        group = common_name,
                        colour = common_name)) + 
  geom_point(size = 5, 
             alpha = 0.1) +
  geom_line(stat = "smooth",
            method = "glm", 
            se = FALSE, 
            method.args = list(family = binomial),
            linewidth = 1.5,
            alpha = 0.7) +
  scale_colour_manual(values = coul) +
  scale_x_continuous(limits = c(0.1, 1), expand = c(0, 0), breaks = seq(0.1, 1, by = 0.3)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme_bw() +
  labs(x = "BirdNET confidence", 
       y = "True positive rate",
       colour = "Species") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  guides(colour = guide_legend(ncol = 4)) 


ggsave(plot = g,
       filename = here("docs", "figures", "calibration_curves_logistic.PNG"),
       width = 24,
       height = 19,
       units = "cm",
       dpi = 300)


# finding species-specific thresholds (logistic) --------------------------

logistic_models <- rate_1 %>%
  group_nest(common_name, scientific_name) %>%
  mutate(model = map(.x = data, .f =~ glm(observed ~ confidence, data = .x, family = binomial))) %>%
  select(common_name, scientific_name, model)

rate_logistic_count <- data_all %>%
  group_nest(common_name, scientific_name, category) %>%
  separate(col = category, into = c("from", "to"), sep = ",") %>%
  mutate(from = str_extract(from, pattern = "[^\\(]+") %>% as.numeric(),
         to = str_extract(to, pattern = "[^\\]]+") %>% as.numeric()) %>%
  mutate(category_dbl = (from + to)/2) %>%
  filter(common_name %in% logistic_models$common_name) %>% # to keep only target species
  left_join(logistic_models) %>%
  mutate(rate_logistic = map2(.x = data, .y = model, 
                              .f =~ predict(.y, newdata = .x, type = "response"))) %>%
  mutate(TP = map_dbl(.x = rate_logistic, .f =~ sum(.x)),
         FP = map_dbl(.x = rate_logistic, .f =~ length(.x) - sum(.x)),
         n = map_dbl(.x = rate_logistic, .f =~ length(.x))) %>%
  group_by(common_name) %>%
  mutate(TP_cum = revcumsum(TP)/sum(n)*100, # total number of true positives after applying threshold
         FP_cum = revcumsum(FP)/sum(n)*100, # total number of false positives after applying threshold
         rate_cum = TP_cum/(TP_cum + FP_cum)) %>%
  ungroup() %>%
  select(-data, -model, -rate_logistic)


# bar plot visualization to view thresholds and FP, TP, and total  --------

g1_list <- rate_logistic_count %>%
  group_nest(common_name, scientific_name) %>%
  mutate(g1 = map2(.x = data, 
                   .y = common_name, 
                   .f =~ g1_plot_1(data = .x, species = .y)))


S1 <- (g1_list$g1[[1]] | g1_list$g1[[2]] | g1_list$g1[[3]]) / 
  (g1_list$g1[[4]] | g1_list$g1[[5]] | g1_list$g1[[6]])

S2 <- (g1_list$g1[[7]] | g1_list$g1[[8]] | g1_list$g1[[9]]) / 
  (g1_list$g1[[10]] | g1_list$g1[[11]] | g1_list$g1[[12]])

S3 <- (g1_list$g1[[13]] | g1_list$g1[[14]] | g1_list$g1[[15]]) / 
  (g1_list$g1[[16]] | g1_list$g1[[17]] | g1_list$g1[[18]])

S4 <- g1_list$g1[[19]]


## adjust the plot
level_patch <- S3 & # rotating between S1, S2 and S3
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

## save the plot
ggsave(plot = level_patch_1,
       ## entering between S1, S2 and S3
       filename = here("docs", "figures", "threshold_setting_S3.png"),
       width = 34,
       height = 18,
       units = "cm",
       dpi = 300)


ggsave(plot = S4,
       filename = here("docs", "figures", "threshold_setting_S4.png"),
       width = 18,
       height = 10,
       units = "cm",
       dpi = 300)




# comparison between four species -----------------------------------------

g1_list_1 <- rate_logistic_count %>%
  group_nest(common_name, scientific_name) %>%
  mutate(g1 = map2(.x = data, 
                   .y = common_name, 
                   .f =~ g1_plot_1(data = .x, species = .y)))

level_patch <- (g1_list_1$g1[[5]] | g1_list_1$g1[[8]]) / (g1_list_1$g1[[2]] | g1_list_1$g1[[15]]) &
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








