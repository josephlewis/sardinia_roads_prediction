INLA::inla.setOption(num.threads = 1)

set.seed(NULL)
set.seed(123)

movement_factors <- readRDS("./Output/Model_data/movement_factors.rds")

pop_rank_nsims <- 1000

M2_slope_components <- ~
  -1 +
  Slope(mathematical_slope_abs_deg2, model = "rw2", scale.model = TRUE, constr = TRUE) +
  Slope2(route_ID, mathematical_slope_abs_deg2, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.05)))) +
  
  Strata(from_unique_id, model = "iid", hyper = list(theta = list(initial = log(1e-6), fixed = TRUE)))

route_ID <- 1

loocv_rank_counts <- list()

for(route_ID in unique(movement_factors$route_ID)) { 
  
  print(route_ID)

M2_slope_loocv <- bru(
  components = M2_slope_components,
  formula = in_adj ~ .,
  family = "poisson",
  data = sf::st_drop_geometry(movement_factors[!movement_factors$route_ID == route_ID,]),
  options = list(control.compute = list(dic = FALSE, waic = FALSE, cpo = FALSE), verbose = TRUE))

movement_factors_pred <- movement_factors[movement_factors$route_ID == route_ID,]

# get unique rw2 values for mapping values to rw2 curve
slope_map_x <- sort(unique(movement_factors$mathematical_slope_abs_deg2))

# generate posterior samples from M2 slope model
M2_samples <- inla.posterior.sample(n = pop_rank_nsims, result = M2_slope_loocv)

rm(M2_slope_loocv)

sim_curves <- list()
for(i in 1:pop_rank_nsims) {
  sim_curves[[i]] <- get_rw2_curve_pop(M2_samples[[i]], "Slope", slope_map_x)
}

rm(M2_samples)

dt <- as.data.table(movement_factors_pred)

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

rm(sim_curves, dt, slope_vals)

rank_counts <- as.data.frame(table(as.vector(rank_matrix)))
colnames(rank_counts) <- c("Rank", "Count")

rank_counts$Rank <- as.numeric(as.character(rank_counts$Rank))
rank_counts$Percent <- rank_counts$Count / sum(rank_counts$Count) * 100

loocv_rank_counts[[route_ID]] <- rank_counts
loocv_rank_counts[[route_ID]]$loocv_route_ID <- route_ID

saveRDS(rank_counts, paste0("./Output/Predictions/loocv/pop_rw2_rank_loocv_data_", route_ID, ".rds"))

rm(rank_matrix, rank_counts, movement_factors_pred)
gc(full = TRUE)

}

loocv_rank_counts <- do.call(rbind, loocv_rank_counts)

loocv_rank_counts$loocv_route_ID <- as.numeric(loocv_rank_counts$loocv_route_ID)
loocv_rank_counts$loocv_route_ID2 <- paste0("Road ", loocv_rank_counts$loocv_route_ID)
loocv_rank_counts$loocv_route_ID3 <- factor(loocv_rank_counts$loocv_route_ID2, unique(paste0("Road ", loocv_rank_counts$loocv_route_ID)))

loocv_overall_rank_counts <- loocv_rank_counts %>%
  group_by(loocv_route_ID) %>%
  complete(Rank = 1:8, fill = list(Count = 0, Percent = 0)) %>%
  group_by(Rank) %>%
  summarise(total_count = sum(Count)) %>%
  mutate(percent_overall = (total_count / sum(total_count)) * 100)

loocv_rank_counts_plot <- ggplot(loocv_overall_rank_counts, aes(x = factor(Rank), y = percent_overall)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", percent_overall)), vjust = -0.5, size = 3) +
  geom_hline(yintercept = 12.5, linetype = "dashed", color = "black") +
  # annotate("text", x = 7.5, y = 12.5 + 1.15, 
  #          label = paste0("Random Chance (", round(12.5,1), "%)"), 
  #          color = "black", size = 3) +
  labs(x = "Predicted rank of observed step", y = "Frequency (%)", title = "B") + 
  scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0,45)) +
  theme_clean() + 
  theme(strip.text = element_text(hjust = 0, size = 14, face = "bold"), strip.background = element_blank())

ggsave(plot = loocv_rank_counts_plot, "./Output/Figures/loocv_rank_counts_plot.png", dpi = 300, width = 8, height = 4)
ggsave(plot = loocv_rank_counts_plot, "./Output/Figures/loocv_rank_counts_plot.svg", dpi = 300, width = 8, height = 4)

loocv_rank_counts_plot2 <- ggplot(loocv_rank_counts, aes(x = factor(Rank), y = Percent)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_hline(yintercept = 12.5, linetype = "dashed", color = "black") +
  facet_wrap(~loocv_route_ID3, ncol = 9) + 
  labs(x = "Predicted rank of observed step", y = "Frequency (%)") + 
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
  theme_clean() + 
  theme(strip.text = element_text(), strip.background = element_blank())

ggsave(plot = loocv_rank_counts_plot2, "./Output/Figures/loocv_rank_counts_plot2.png", width = 14, height = 14, dpi = 300)
ggsave(plot = loocv_rank_counts_plot2, "./Output/Figures/loocv_rank_counts_plot2.svg", width = 14, height = 14, dpi = 300)
