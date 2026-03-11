library(data.table)
library(ggplot2)

INLA::inla.setOption(num.threads = 1)

set.seed(NULL)
set.seed(1)

movement_factors <- readRDS("./Output/Model_data/movement_factors.rds")
M2_slope <- readRDS("./Output/Models/M2_slope.rds")

mean_curve <- M2_slope$summary.random$Slope[, c("ID", "mean")]

# Map the mean curve to your slope data using approx()
dt <- as.data.table(movement_factors)

# Ensure slope_vals is the vector of actual slopes in data
slope_vals <- dt$mathematical_slope_abs_deg2

# Map the curve IDs to your actual slope values
eff_slope_mean <- approx(x = mean_curve$ID, y = mean_curve$mean, xout = slope_vals, rule = 2)$y

dt[, mean_score := eff_slope_mean]

# Rank 1 = highest score (most likely step)
dt[, mean_rank := frank(-mean_score, ties.method = "min"), by = from_unique_id]

observed_ranks <- dt[in_adj == 1, mean_rank]

rank_counts_mean <- as.data.frame(table(observed_ranks))
colnames(rank_counts_mean) <- c("Rank", "Count")
rank_counts_mean$Rank <- as.numeric(as.character(rank_counts_mean$Rank))
rank_counts_mean$Percent <- (rank_counts_mean$Count / sum(rank_counts_mean$Count)) * 100

saveRDS(rank_counts_mean, "./Output/Predictions/pop_rw2_mean_rank_data.rds")

pop_rw2_mean_rank_plot <- ggplot(rank_counts_mean, aes(x = factor(Rank), y = Percent)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Percent)), vjust = -0.5, size = 3) +
  geom_hline(yintercept = 12.5, linetype = "dashed", color = "black") +
  labs(x = "Predicted rank of observed step", y = "Frequency (%)", title = "A") + 
  scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0,45)) +
  theme_clean() + 
  theme(strip.text = element_text(hjust = 0, size = 14, face = "bold"), strip.background = element_blank())

ggsave(plot = pop_rw2_mean_rank_plot, "./Output/Figures/pop_rw2_mean_rank_plot.png", dpi = 300, width = 8, height = 4)
ggsave(plot = pop_rw2_mean_rank_plot, "./Output/Figures/pop_rw2_mean_rank_plot.svg", dpi = 300, width = 8, height = 4)

##########################################
##########################################
########MEAN RW2 CURVE COMPARISON ########
##########################################
##########################################

slope_vals <- seq(-45, 45, 0.1)

# 3. Map your Mean RW2 (Symmetrical Mirroring)
# Use the absolute value of x_range to pull from your mean_curve
# calculate selection weights for slope values (absolute values since rw2 curve is symmetrical but do pos/neg slope for plot purposes)
rw2_vals <- approx(x = mean_curve$ID, y = mean_curve$mean, xout = abs(slope_vals), rule = 2)$y

pop_curve_comparison <- data.frame(
  slope = slope_vals,
  `Roman` = exp(rw2_vals) / max(exp(rw2_vals)),
  `Tobler` = tobler_cf(deg2slope(slope_vals)) / max(tobler_cf(deg2slope(slope_vals))),
  `Herzog` = herzog_cf(deg2slope(slope_vals)) / max(herzog_cf(deg2slope(slope_vals))),
  `Wheel` = wt_cf(deg2slope(slope_vals), crit_slope = 12) / max(wt_cf(deg2slope(slope_vals), crit_slope = 12))
) %>%
  pivot_longer(-slope, names_to = "Function", values_to = "Value")

pop_curve_comparison$Function <- factor(pop_curve_comparison$Function, levels = c("Roman", "Tobler", "Herzog", "Wheel"),
                           labels = c("Roman roads", "Tobler's Hiking Function", "Herzog's Energy", "Wheel-based"))

pop_curve_comparison$mathematical_slope_abs <- deg2slope(pop_curve_comparison$slope)

pop_curve_comparison_plot <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey75", alpha = 1) +
  geom_line(data = pop_curve_comparison, aes(x = mathematical_slope_abs, y = Value, group = Function, linetype = Function, colour = Function), size = 1) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid")) +
  scale_color_manual(values = c("black", "#e41a1c", "#377eb8", "#4daf4a")) + 
  scale_x_continuous(breaks = seq(-1, 1, 0.2), labels = paste0(seq(-1, 1, 0.2), " (", round(slope2deg(seq(-1, 1, 0.2)), 1), "°)")) +
  scale_y_continuous(breaks = seq(0,1, 0.1)) + 
  labs(x = "Mathematical slope gradient (Degrees, °)", y = "Relative selection weight / Conductance", colour = NULL, linetype = NULL) +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.background = element_blank(), plot.background = element_rect(color = "white"), strip.background = element_blank(), panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))

ggsave(plot = pop_curve_comparison_plot, "./Output/Figures/pop_curve_comparison_plot.png", dpi = 300, width = 14, height = 7)
ggsave(plot = pop_curve_comparison_plot, "./Output/Figures/pop_curve_comparison_plot.svg", dpi = 300, width = 14, height = 7)



