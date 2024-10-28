
# library -----------------------------------------------------------------
library(tidyverse)
library(here)
library(gt)
library(RColorBrewer)


# load data ---------------------------------------------------------------
load(here("R", "rate_loess_count.rda"))
load(here("R", "rate_logistic_count.rda"))

data_2020 <- read_csv(here("data", "processed", "2020_passerine_BirdNET_updated.csv"))
data_2021 <- read_csv(here("data", "processed", "2021_passerine_BirdNET.csv"))
data_all <- bind_rows(data_2020, data_2021) %>%
  mutate(category = cut(x = confidence, breaks = seq(0.1, 1, 0.05))) %>%
  filter(common_name %in% rate_logistic_count$common_name)

# universal threshold of 0.7 ---------------------------------------------
thresholds_table_s1 <- rate_logistic_count %>%
  filter(from == 0.7) %>%
  mutate(proportion_detection = TP_cum + FP_cum) %>%
  select(common_name, scientific_name, from, rate_cum, proportion_detection) %>%
  mutate(proportion_detection = round(proportion_detection, digits = 0),
         rate_cum = round(rate_cum, digits = 2)) %>%
  arrange(from, desc(rate_cum), proportion_detection) 


# species-specific threshld achieving 0.9 precision ----------------------
thresholds_table_out <- rate_logistic_count %>%
  filter(common_name %in% c("Brown Creeper", "Varied Thrush")) %>%
  group_by(common_name, scientific_name) %>%
  slice_max(category_dbl) %>%
  mutate(proportion_detection = TP_cum + FP_cum) %>%
  select(common_name, scientific_name, from, rate_cum, proportion_detection) %>%
  mutate(proportion_detection = round(proportion_detection, digits = 0),
         rate_cum = round(rate_cum, digits = 2))

thresholds_table_s2 <- rate_logistic_count %>%
  filter(rate_cum > 0.90) %>%
  group_by(common_name, scientific_name) %>%
  slice_min(category_dbl) %>%
  mutate(proportion_detection = TP_cum + FP_cum) %>%
  select(common_name, scientific_name, from, rate_cum, proportion_detection) %>%
  mutate(proportion_detection = round(proportion_detection, digits = 0),
         rate_cum = round(rate_cum, digits = 2)) %>%
  rbind(thresholds_table_out) 


# total number of detections ----------------------------------------------
n_detections <- data_all %>%
  group_by(common_name) %>%
  summarise(n_detections = n())


# combine tables ----------------------------------------------------------
table_final <- thresholds_table_s1 %>%
  left_join(thresholds_table_s2, 
            by = c("common_name", "scientific_name"), 
            suffix = c("_u", "_s")) %>%
  ungroup() %>%
  left_join(n_detections, by = "common_name") %>%
  relocate(n_detections, .after = scientific_name) %>%
  arrange(from_s, desc(rate_cum_s)) %>%
  gt() %>%
  tab_spanner(label = "Universal",
              columns = c(from_u, rate_cum_u, proportion_detection_u)) %>%
  tab_spanner(label = "Species-specific",
              columns = c(from_s, rate_cum_s, proportion_detection_s))

table_final

#gtsave(table_final, filename = here("docs", "tables", "thresholds_table_logistic.docx"))


# threshold tables visualization ------------------------------------------

light_coul <- brewer.pal(n = 3, "Pastel2") 
dark_coul <- brewer.pal(n = 3, "Dark2")

threshold_plot <- thresholds_table_s2 %>%
  mutate(proportion_detection = proportion_detection / 100,
         common_name = factor(common_name, levels = rev(thresholds_table_s2$common_name))) %>%
  pivot_longer(cols = c(from, rate_cum, proportion_detection)) %>%
  group_by(name) %>%
  mutate(mean_value = mean(value)) %>%
  ggplot(aes(x = common_name, y = value, group = name, colour = name)) +
  geom_point(size = 8, alpha = 0.5) +
  geom_hline(aes(yintercept = mean_value, colour = name), 
             linewidth = 2, alpha = 0.5) +
  scale_colour_manual(values = c(dark_coul[1], light_coul[2], light_coul[3])) +
  theme_bw() +
  labs(x = NULL, y = NULL, legend = NULL) +
  theme(axis.text = element_text(size = 16),
        axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5),
        legend.position = "none",
        plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"))

threshold_plot
