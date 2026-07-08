set.seed(NULL)
set.seed(123)

movement_factors <- readRDS("./Output/Model_data/movement_factors.rds")
M1_slope <- readRDS("./Output/Models/M1_slope.rds")

###################################
####### COEFFICIENT SUMMARIES #####
###################################

pred_data <- expand.grid(
  route_ID = 1:66,
  mathematical_slope_abs = 1)

slope_samples <- generate(
  M1_slope,
  newdata = pred_data,
  formula = ~ (Slope + Slope2),
  n.samples = 1000)

samples_long <- as.data.frame(slope_samples) %>%
  mutate(route_ID = factor(1:n())) %>%
  pivot_longer(
    cols = starts_with("V"), 
    names_to = "sample_index", 
    values_to = "value")

samples_long_summary <- samples_long %>%
  group_by(route_ID) %>%
  summarise(mean = mean(value),
            median = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975)) %>%
  ungroup() %>%  
  mutate(group = case_when(
    lower > 0 & upper > 0 ~ "positive",
    lower < 0 & upper < 0 ~ "negative",
    TRUE ~ "uncertain"))

samples_long <- samples_long %>%
  left_join(samples_long_summary) %>%
  mutate(group = factor(group, levels = c("positive", "negative", "uncertain")))

re_samples_plot <- ggplot() + 
  tidybayes::stat_interval(data = samples_long[samples_long$group == "positive",], aes(x = route_ID, y = value), .width = c(0.5, 0.8, 0.95)) +
  tidybayes::stat_pointinterval(data = samples_long[samples_long$group == "positive",], aes(x = route_ID, y = value), .width = c(0)) +
  scale_colour_brewer(palette = "Reds", name = "Credible interval") + 
  ggnewscale::new_scale_color() + 
  tidybayes::stat_interval(data = samples_long[samples_long$group == "negative",], aes(x = route_ID, y = value), .width = c(0.5, 0.8, 0.95)) +
  tidybayes::stat_pointinterval(data = samples_long[samples_long$group == "negative",], aes(x = route_ID, y = value), .width = c(0)) +
  scale_colour_brewer(palette = "Blues", name = "Credible interval") +   
  ggnewscale::new_scale_color() + 
  tidybayes::stat_interval(data = samples_long[samples_long$group == "uncertain",], aes(x = route_ID, y = value), .width = c(0.5, 0.8, 0.95)) +
  tidybayes::stat_pointinterval(data = samples_long[samples_long$group == "uncertain",], aes(x = route_ID, y = value), .width = c(0)) +
  scale_colour_brewer(palette = "Greys", name = "Credible interval") +   
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_y_continuous(breaks = seq(-15, 10, 1)) + 
  labs(x = "Route ID", y = "Log-relative selection strength") +
  labs(colour = NULL) + 
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.background = element_blank(), plot.background = element_rect(color = "white"), strip.background = element_blank(), panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))

ggsave(plot = re_samples_plot, "./Output/Figures/re_samples_plot.png", dpi = 300, width = 12, height = 6)
ggsave(plot = re_samples_plot, "./Output/Figures/re_samples_plot.svg", dpi = 300, width = 12, height = 6)

#########################################
####### COEFFICIENT SLOPE SUMMARIES #####
#########################################

slope_grid <- expand.grid(
  mathematical_slope_abs = seq(0, 1, 0.01),
  route_ID = unique(movement_factors$route_ID))

pred_data2 <- generate(
  M1_slope,
  newdata = slope_grid,
  formula = ~ exp(Slope + Slope2),
  n.samples = 1000)

samples2_long <- cbind(slope_grid, as.data.frame(pred_data2)) %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "sample_index",
    values_to = "value")

samples2_long$route_ID2 <- paste("Road ", samples2_long$route_ID)

slope_grid2 <- data.frame(mathematical_slope_abs = seq(0, 1, 0.01))

pred_data3 <- generate(
  M1_slope,
  newdata = slope_grid2,
  formula = ~ exp(Slope),
  n.samples = 1000)

samples3_long <- cbind(slope_grid2, as.data.frame(pred_data3)) %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "sample_index",
    values_to = "value") %>%
  mutate(route_ID = "Typical road",
         route_ID2 = "Typical road")

fixed_samples_plot <- ggplot() + 
  tidybayes::stat_lineribbon(data = samples3_long, aes(x = mathematical_slope_abs, y = value, group = route_ID)) +
  scale_fill_brewer(palette = "Oranges", name = "Credible interval") + 
  scale_x_continuous(breaks = seq(0, 1, 0.1), labels = paste0(seq(0, 1, 0.1), " (", round(slope2deg(seq(0, 1, 0.1)), 1), "°)")) +
  scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0, 1)) + 
  labs(x = "Mathematical slope gradient (Degrees, °)", y = "Relative selection strength") +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.background = element_blank(), plot.background = element_rect(color = "white"), strip.background = element_blank(), panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))

ggsave(plot = fixed_samples_plot, "./Output/Figures/fixed_samples_plot.png", dpi = 300, width = 10, height = 5)
ggsave(plot = fixed_samples_plot, "./Output/Figures/fixed_samples_plot.svg", dpi = 300, width = 10, height = 5)

pred_data_45 <- generate(
  M1_slope,
  newdata = data.frame(mathematical_slope_abs = 1),
  formula = ~ exp(Slope),
  n.samples = 1000)
100 - round(quantile(pred_data_45, c(0.025, 0.5, 0.975))*100, 0)

pred_data_6 <- generate(
  M1_slope,
  newdata = data.frame(mathematical_slope_abs = deg2slope(6)),
  formula = ~ exp(Slope),
  n.samples = 1000)
100 - round(quantile(pred_data_6, c(0.025, 0.5, 0.975))*100, 0)

samples4_long <- rbind(samples2_long, samples3_long)
samples4_long$route_ID2 <- factor(samples4_long$route_ID2, c(unique(samples3_long$route_ID2), unique(samples2_long$route_ID2)))

re_samples_plot2 <- ggplot() + 
  tidybayes::stat_lineribbon(data = samples4_long[samples4_long$route_ID2 %in% c("Typical road"),], aes(x = mathematical_slope_abs, y = value, group = route_ID)) +
  scale_fill_brewer(palette = "Oranges", name = "Credible interval") + 
  ggnewscale::new_scale_fill() + 
  tidybayes::stat_lineribbon(data = samples4_long[samples4_long$route_ID %in% samples_long[samples_long$group == "positive",]$route_ID,], aes(x = mathematical_slope_abs, y = value, group = route_ID)) + 
  scale_fill_brewer(palette = "Reds", name = "Credible interval") + 
  ggnewscale::new_scale_fill() + 
  tidybayes::stat_lineribbon(data = samples4_long[samples4_long$route_ID %in% samples_long[samples_long$group == "negative",]$route_ID,], aes(x = mathematical_slope_abs, y = value, group = route_ID)) + 
  scale_fill_brewer(palette = "Blues", name = "Credible interval") + 
  ggnewscale::new_scale_fill() + 
  tidybayes::stat_lineribbon(data = samples4_long[samples4_long$route_ID %in% samples_long[samples_long$group == "uncertain",]$route_ID,], aes(x = mathematical_slope_abs, y = value, group = route_ID)) + 
  scale_fill_brewer(palette = "Greys", name = "Credible interval") + 
  facet_wrap(~route_ID2, scales = "free_y") + 
  scale_x_continuous(breaks = seq(0, 1, 0.2), labels = paste0(seq(0, 1, 0.2), " (", round(slope2deg(seq(0, 1, 0.2)), 1), "°)")) +
  labs(x = "Mathematical slope gradient (Degrees, °)", y = "Relative selection strength") +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.background = element_blank(), plot.background = element_rect(color = "white"), strip.background = element_blank(), panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))

ggsave(plot = re_samples_plot2, "./Output/Figures/re_samples_plot2.png", dpi = 300, width = 14, height = 14)
ggsave(plot = re_samples_plot2, "./Output/Figures/re_samples_plot2.svg", dpi = 300, width = 14, height = 14)

##################################
####### ROMAN ROAD SUMMARIES #####
##################################

# to append information on overall RW2 selection for plotting
RR <- readRDS("./Data/Sardinia_roads/RR.rds")

RR2 <- RR
RR2$route_ID <- RR2$ID
RR2$route_ID <- factor(RR2$route_ID)
RR2 <- RR2 %>%
  left_join(samples_long_summary)


saveRDS(RR2, "./Output/Predictions/RR2.rds")
sf::st_write(RR2, "./Output/Predictions/RR2.gpkg", append = FALSE)
