INLA::inla.setOption(num.threads = 1)

set.seed(NULL)
set.seed(123)

M2_slope <- readRDS("./Output/Models/M2_slope.rds")

# to append information on overall RW2 selection for plotting
RR <- readRDS("./Data/Sardinia_roads/RR.rds")

re_samples <- generate(
  M2_slope, 
  newdata = expand.grid(mathematical_slope_abs_deg2 = seq(0, 45, 1), route_ID = 1:66), 
  formula = ~ (Slope + Slope2), 
  n.samples = 1000)

re_samples <- as.data.frame(re_samples) %>%
  bind_cols(expand.grid(mathematical_slope_abs_deg2 = seq(0, 45, 1),route_ID = 1:66)) %>%
  pivot_longer(cols = c(1:1000),names_to = ".draw", values_to = ".value")

re_samples <- re_samples %>%
  group_by(.draw, route_ID) %>%
  mutate(log_value = .value - .value[mathematical_slope_abs_deg2 == 0]) %>%
  mutate(.value = exp(log_value)) %>%
  ungroup()

re_samples$route_ID <- paste0("Road ", re_samples$route_ID)
re_samples$mathematical_slope_abs <- deg2slope(re_samples$mathematical_slope_abs_deg2)
re_samples$route_ID <- factor(re_samples$route_ID, levels = unique(re_samples$route_ID))

re_samples <- re_samples %>%
  group_by(route_ID, mathematical_slope_abs_deg2) %>%
  mutate(
    lower = quantile(.value, 0.025),
    upper = quantile(.value, 0.975),
    type = case_when(lower > 1 ~ "above", upper < 1 ~ "below", TRUE ~ "crosses")) %>%
  select(-lower, -upper) %>%
  ungroup()

re_samples_avg <- re_samples %>%
  group_by(route_ID, .draw) %>%
  summarise(draw_mean = mean(.value), .groups = "drop_last") %>%
  summarise(
    median_val = median(draw_mean),
    lower_95 = quantile(draw_mean, 0.025),
    upper_95 = quantile(draw_mean, 0.975),
    .groups = "drop") %>%
  mutate(
    type_overall = case_when(
      lower_95 > 1 ~ "above",
      upper_95 < 1 ~ "below",
      TRUE ~ "crosses"))

RR2 <- RR
RR2$route_ID <- paste0("Road ", RR2$ID)
RR2 <- RR2 %>%
  left_join(distinct(re_samples_avg[c("route_ID", "type_overall")]))

saveRDS(RR2, "./Output/Predictions/RR2.rds")
sf::st_write(RR2, "./Output/Predictions/RR2.gpkg", append = FALSE)

segment_lookup <- re_samples %>%
  distinct(route_ID, mathematical_slope_abs_deg2, type) %>%
  arrange(route_ID, mathematical_slope_abs_deg2) %>%
  group_by(route_ID) %>%
  mutate(segment_id = consecutive_id(type)) %>% 
  ungroup()

re_samples <- re_samples %>%
  left_join(segment_lookup, by = c("route_ID", "mathematical_slope_abs_deg2", "type"))

re_samples_rw2_plot <- ggplot() +
  stat_lineribbon(data = re_samples, aes(x = mathematical_slope_abs, y = .value), .width = c(.50, .80, .95), alpha = 1) +
  scale_fill_brewer(palette = "Greys", name = "Credible Interval") +
  ggnewscale::new_scale_fill() +
  stat_lineribbon(data = filter(re_samples, type == "above"), aes(x = mathematical_slope_abs, y = .value, group = segment_id), .width = c(.50, .80, .95), alpha = 1) +
  scale_fill_brewer(palette = "Blues", name = "Credible Interval") +
  ggnewscale::new_scale_fill() +
  stat_lineribbon(
    data = filter(re_samples, type == "below"), aes(x = mathematical_slope_abs, y = .value, group = segment_id), .width = c(.50, .80, .95), alpha = 1) +
  scale_fill_brewer(palette = "Reds", name = "Credible Interval") +
  scale_x_continuous(breaks = seq(0, 1, 0.2), labels = paste0(seq(0, 1, 0.2), " (", round(slope2deg(seq(0, 1, 0.2)), 1), "°)")) +
  labs(x = "Mathematical slope gradient (Degrees, °)", y = "Relative selection weight") +
  geom_hline(yintercept = 1, linetype = "dashed") + 
  facet_wrap(~route_ID, scales = "free_y") + 
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.background = element_blank(), plot.background = element_rect(color = "white"), strip.background = element_blank(), panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))

ggsave(plot = re_samples_rw2_plot, "./Output/Figures/re_samples_rw2_plot.png", dpi = 300, width = 14, height = 14)
ggsave(plot = re_samples_rw2_plot, "./Output/Figures/re_samples_rw2_plot.svg", dpi = 300, width = 14, height = 14)

pop_samples <- generate(
  M2_slope, 
  newdata = data.frame(mathematical_slope_abs_deg2 = seq(0, 45, 1)), 
  formula = ~ (Slope), 
  n.samples = 1000)

pop_samples <- as.data.frame(pop_samples) %>%
  bind_cols(data.frame(mathematical_slope_abs_deg2 = seq(0, 45, 1))) %>%
  pivot_longer(cols = c(1:1000), names_to = ".draw", values_to = ".value")

pop_samples$route_ID <- "Typical Road"
pop_samples$mathematical_slope_abs <- deg2slope(pop_samples$mathematical_slope_abs_deg2)
pop_samples$route_ID <- factor(pop_samples$route_ID, levels = unique(pop_samples$route_ID))

pop_samples <- pop_samples %>%
  group_by(.draw) %>%
  mutate(log_value = .value - .value[mathematical_slope_abs_deg2 == 0]) %>%
  mutate(.value = exp(log_value)) %>%
  ungroup()

pop_samples_rw2_plot <- ggplot() +
  stat_lineribbon(data = pop_samples, aes(x = mathematical_slope_abs, y = .value), .width = c(.50, .80, .95), alpha = 1) +
  scale_fill_brewer(palette = "Reds") +
  scale_x_continuous(breaks = seq(0, 1, 0.1), labels = paste0(seq(0, 1, 0.1), " (", round(slope2deg(seq(0, 1, 0.1)), 1), "°)")) +
  scale_y_continuous(breaks = seq(-20, 10, 0.1), limits = c(0,1.1)) +
  labs(x = "Mathematical slope gradient (Degrees, °)", y = "Relative selection weight", fill = "Credible Interval") +
  geom_hline(yintercept = 1, linetype = "dashed") + 
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.background = element_blank(), plot.background = element_rect(color = "white"), strip.background = element_blank(), 
        panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))

print(round(quantile(pop_samples[pop_samples$mathematical_slope_abs_deg2 == 0,]$.value - pop_samples[pop_samples$mathematical_slope_abs_deg2 == 45,]$.value, c(0.025, 0.5, 0.975)), 2))

print(round(quantile(pop_samples[pop_samples$mathematical_slope_abs_deg2 == 0,]$.value - pop_samples[pop_samples$mathematical_slope_abs_deg2 == 6,]$.value, c(0.025, 0.5, 0.975)), 2))

ggsave(plot = pop_samples_rw2_plot, "./Output/Figures/pop_samples_rw2_plot.png", dpi = 300, width = 14, height = 7)
ggsave(plot = pop_samples_rw2_plot, "./Output/Figures/pop_samples_rw2_plot.svg", dpi = 300, width = 14, height = 7)

pop_samples$type <- "below"
pop_samples$segment_id <- 1

pop_re_samples <- rbind(pop_samples, re_samples)
pop_re_samples$mathematical_slope_abs <- deg2slope(pop_re_samples$mathematical_slope_abs_deg2)
pop_re_samples$route_ID <- factor(pop_re_samples$route_ID, levels = unique(pop_re_samples$route_ID))

pop_re_samples_rw2_plot <- ggplot() +
  stat_lineribbon(data = pop_re_samples[pop_re_samples$route_ID == "Typical Road",], aes(x = mathematical_slope_abs, y = .value), .width = c(.50, .80, .95), alpha = 1, show.legend = FALSE) +
  scale_fill_brewer(palette = "Blues", name = "Credible Interval") +
  ggnewscale::new_scale_fill() +
  stat_lineribbon(data = pop_re_samples[pop_re_samples$route_ID != "Typical Road",], aes(x = mathematical_slope_abs, y = .value), .width = c(.50, .80, .95), alpha = 1, show.legend = TRUE) +
  scale_fill_brewer(palette = "Reds", name = "Credible Interval") +
  scale_x_continuous(breaks = seq(0, 1, 0.2), labels = paste0(seq(0, 1, 0.2), " (", round(slope2deg(seq(0, 1, 0.2)), 1), "°)")) +
  labs(x = "Mathematical slope gradient (Degrees, °)", y = "Relative selection weight", fill = "Credible interval") +
  geom_hline(yintercept = 1, linetype = "dashed") + 
  facet_wrap(~route_ID, scales = "free_y") + 
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.background = element_blank(), plot.background = element_rect(color = "white"), strip.background = element_blank(), 
        panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))

ggsave(plot = pop_re_samples_rw2_plot, "./Output/Figures/pop_re_samples_rw2_plot.png", dpi = 300, width = 14, height = 14)
ggsave(plot = pop_re_samples_rw2_plot, "./Output/Figures/pop_re_samples_rw2_plot.svg", dpi = 300, width = 14, height = 14)
