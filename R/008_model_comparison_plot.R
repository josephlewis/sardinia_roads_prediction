movement_factors <- readRDS("./Output/Model_data/movement_factors.rds")

tobler_ranks <- movement_factors %>%
  sf::st_drop_geometry() %>%
  group_by(from_unique_id) %>%
  mutate(rank = min_rank(desc(math_slope_tobler))) %>%
  filter(in_adj == 1) %>%
  ungroup() %>%
  select(from_unique_id, rank) %>%
  group_by(rank) %>%
  count() %>%
  ungroup() %>%
  mutate(n2 = n/sum(n)*100)

herzog_ranks <- movement_factors %>%
  sf::st_drop_geometry() %>%
  group_by(from_unique_id) %>%
  mutate(rank = min_rank(desc(math_slope_herzog))) %>%
  filter(in_adj == 1) %>%
  ungroup() %>%
  select(from_unique_id, rank) %>%
  group_by(rank) %>%
  count() %>%
  ungroup() %>%
  mutate(n2 = n/sum(n)*100)

wheeled_ranks <- movement_factors %>%
  sf::st_drop_geometry() %>%
  group_by(from_unique_id) %>%
  mutate(rank = min_rank(desc(math_slope_wheeled))) %>%
  filter(in_adj == 1) %>%
  ungroup() %>%
  select(from_unique_id, rank) %>%
  group_by(rank) %>%
  count() %>%
  ungroup() %>%
  mutate(n2 = n/sum(n)*100)

cf_rank <- rbind(tobler_ranks, herzog_ranks, wheeled_ranks)
cf_rank$type <- rep(c("C", "D", "E"), each = 8)

cf_rank_plot_1 <- ggplot(cf_rank[cf_rank$type == "C",], aes(x = factor(rank), y =n2)) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", n2)), vjust = -0.5, size = 4) +
    geom_hline(yintercept = 12.5, linetype = "dashed", color = "black") +
  facet_wrap(~type, ncol = 1) + 
    labs(x = "Predicted rank of observed step", y = "Frequency (%)") + 
    scale_y_continuous(breaks = seq(0, 100, 2), limits = c(0, 16)) +
    theme_clean() + 
  theme(strip.text = element_text(hjust = 0, size = 14, face = "bold"), strip.background = element_blank())

cf_rank_plot_2 <- ggplot(cf_rank[cf_rank$type == "D",], aes(x = factor(rank), y =n2)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", n2)), vjust = -0.5, size = 4) +
  geom_hline(yintercept = 12.5, linetype = "dashed", color = "black") +
  facet_wrap(~type, ncol = 1) + 
  labs(x = "Predicted rank of observed step", y = "Frequency (%)") + 
  scale_y_continuous(breaks = seq(0, 100, 2), limits = c(0, 16)) +
  theme_clean() + 
  theme(strip.text = element_text(hjust = 0, size = 14, face = "bold"), strip.background = element_blank())

cf_rank_plot_3 <- ggplot(cf_rank[cf_rank$type == "E",], aes(x = factor(rank), y =n2)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", n2)), vjust = -0.5, size = 4) +
  geom_hline(yintercept = 12.5, linetype = "dashed", color = "black") +
  facet_wrap(~type, ncol = 1) + 
  labs(x = "Predicted rank of observed step", y = "Frequency (%)") + 
  scale_y_continuous(breaks = seq(0, 100, 2), limits = c(0,16)) +
  theme_clean() + 
  theme(strip.text = element_text(hjust = 0, size = 14, face = "bold"), strip.background = element_blank())

ggsave(plot = cf_rank_plot_1, "./Output/Figures/cf_rank_plot_1.png", dpi = 300, width = 8, height = 4)
ggsave(plot = cf_rank_plot_1, "./Output/Figures/cf_rank_plot_1.svg", dpi = 300, width = 8, height = 4)

ggsave(plot = cf_rank_plot_2, "./Output/Figures/cf_rank_plot_2.png", dpi = 300, width = 8, height = 4)
ggsave(plot = cf_rank_plot_2, "./Output/Figures/cf_rank_plot_2.svg", dpi = 300, width = 8, height = 4)

ggsave(plot = cf_rank_plot_3, "./Output/Figures/cf_rank_plot_3.png", dpi = 300, width = 8, height = 4)
ggsave(plot = cf_rank_plot_3, "./Output/Figures/cf_rank_plot_3.svg", dpi = 300, width = 8, height = 4)