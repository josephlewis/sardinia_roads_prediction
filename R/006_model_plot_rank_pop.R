INLA::inla.setOption(num.threads = 1)

set.seed(NULL)
set.seed(1)

movement_factors <- readRDS("./Output/Model_data/movement_factors.rds")
M2_slope <- readRDS("./Output/Models/M2_slope.rds")

pop_rank_nsims <- 1000

# get unique rw2 values for mapping values to rw2 curve
slope_map_x <- sort(unique(movement_factors$mathematical_slope_abs_deg2))

# generate posterior samples from M2 slope model
M2_samples <- inla.posterior.sample(n = pop_rank_nsims, result = M2_slope)

# extract rw2 curves for all simulations
sim_curves <- list()
for(i in 1:pop_rank_nsims) {
  sim_curves[[i]] <- get_rw2_curve_pop(M2_samples[[i]], "Slope", slope_map_x)
}

dt <- as.data.table(movement_factors)

slope_vals <- dt$mathematical_slope_abs_deg2

chosen_indices <- which(dt$in_adj == 1)

# rows = observed steps; cols = no. of simulations
rank_matrix <- matrix(NA, nrow = length(chosen_indices), ncol = pop_rank_nsims)

for(j in 1:pop_rank_nsims) {
  
  print(j)

  # get rw2 curve for specific simulation
  curve <- sim_curves[[j]]
  
  # approx() maps the binned slope values to the RW2 curve
  eff_slope <- approx(x = curve$x, y = curve$y, 
                      xout = slope_vals, rule = 2)$y
  
  dt[, temp_score := eff_slope]
  
  # calculate rank of predicted vs. observed. rank 1 is the highest; rank 8 is the lowest
  dt[, temp_rank := frank(-temp_score, ties.method = "min"), by = from_unique_id]
  
  rank_matrix[, j] <- dt$temp_rank[chosen_indices]
}

rank_counts <- as.data.frame(table(as.vector(rank_matrix)))
colnames(rank_counts) <- c("Rank", "Count")

rank_counts$Rank <- as.numeric(as.character(rank_counts$Rank))
rank_counts$Percent <- rank_counts$Count / sum(rank_counts$Count) * 100

saveRDS(rank_counts, "./Output/Predictions/pop_rw2_rank_data.rds")

pop_rw2_rank_plot <- ggplot(rank_counts, aes(x = factor(Rank), y = Percent)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Percent)), vjust = -0.5, size = 3) +
  geom_hline(yintercept = 12.5, linetype = "dashed", color = "black") +
  annotate("text", x = 7.5, y = 12.5 + 1.15, 
           label = paste0("Random Chance (", round(12.5,1), "%)"), 
           color = "black", size = 3) +
  labs(x = "Predicted rank of observed step", y = "Frequency (%)", title = "A") + 
  scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0,45)) +
  theme_clean() + 
  theme(strip.text = element_text(hjust = 0, size = 14, face = "bold"), strip.background = element_blank())

ggsave(plot = pop_rw2_rank_plot, "./Output/Figures/pop_rw2_rank_plot.png", dpi = 300, width = 8, height = 4)
ggsave(plot = pop_rw2_rank_plot, "./Output/Figures/pop_rw2_rank_plot.svg", dpi = 300, width = 8, height = 4)
