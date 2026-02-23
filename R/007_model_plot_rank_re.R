INLA::inla.setOption(num.threads = 1)

set.seed(NULL)
set.seed(1)

movement_factors <- readRDS("./Output/Model_data/movement_factors.rds")
M2_slope <- readRDS("./Output/Models/M2_slope.rds")

re_rank_nsims <- 1000

# get unique rw2 values for mapping values to rw2 curve
slope_map_x <- sort(unique(movement_factors$mathematical_slope_abs_deg2))

# create index column to match model indexing
unique_routes <- sort(unique(movement_factors$route_ID))
dt <- as.data.table(movement_factors)

dt[, route_idx := match(route_ID, unique_routes)] 

# generate posterior samples from M2 slope model
M2_samples <- inla.posterior.sample(n = re_rank_nsims, result = M2_slope)

sim_data <- list()
n_routes <- length(unique_routes)

# extract rw2 curves for all simulations
for(i in 1:re_rank_nsims) {
  sim_data[[i]] <- list(
    global = get_rw2_curve_re(M2_samples[[i]], "Slope", slope_map_x),
    local  = get_iid_coefs(M2_samples[[i]], "Slope2", n_routes)
  )
}

slope_vals <- dt$mathematical_slope_abs_deg2
route_indices <- dt$route_idx 
chosen_indices <- which(dt$in_adj == 1)

# rows = observed steps; cols = no. of simulations
rank_matrix <- matrix(NA, nrow = length(chosen_indices), ncol = re_rank_nsims)

for(j in 1:re_rank_nsims) {
  
  print(j)
  
  curve <- sim_data[[j]]$global
  betas <- sim_data[[j]]$local
  
  score_global <- approx(x = curve$x, y = curve$y, xout = slope_vals, rule = 2)$y
  
  score_local <- betas[route_indices] * slope_vals
  
  dt[, temp_score := score_global + score_local]

  dt[, temp_rank := frank(-temp_score, ties.method = "min"), by = from_unique_id]
  
  rank_matrix[, j] <- dt$temp_rank[chosen_indices]
}

observed_routes <- dt$route_ID[chosen_indices]

results_long <- data.frame(
  route_ID = rep(observed_routes, times = re_rank_nsims),
  Rank = as.vector(rank_matrix)
)

plot_data <- results_long %>%
  group_by(route_ID, Rank) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(route_ID) %>%
  mutate(Percent = Count / sum(Count) * 100) %>%
  ungroup()

full_grid <- expand.grid(route_ID = unique(unique_routes), Rank = 1:8)
plot_data <- full_join(plot_data, full_grid, by = c("route_ID", "Rank"))
plot_data$Percent[is.na(plot_data$Percent)] <- 0

plot_data$route_ID2 <- paste0("Road ", plot_data$route_ID)
plot_data$route_ID2 <- factor(plot_data$route_ID2, levels = unique(plot_data$route_ID2))

saveRDS(plot_data, "./Output/Predictions/re_rw2_rank_data.rds")

re_rw2_rank_plot <- ggplot(plot_data, aes(x = factor(Rank), y = Percent)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_hline(yintercept = 12.5, linetype = "dashed", color = "black") +
  facet_wrap(~route_ID2, ncol = 9) + 
  labs(x = "Predicted rank of observed step", y = "Frequency (%)") + 
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
  theme_clean() + 
  theme(strip.text = element_text(), strip.background = element_blank())

ggsave(plot = re_rw2_rank_plot, "./Output/Figures/re_rw2_rank_plot.png", width = 14, height = 14, dpi = 300)
ggsave(plot = re_rw2_rank_plot, "./Output/Figures/re_rw2_rank_plot.svg", width = 14, height = 14, dpi = 300)