dem <- terra::rast("./Data/Sardinia_DEM/DEM_modifed.tif")
sardinia_border <- sf::st_read("./Data/gadm41_ITA_1.json/sardinia_border.gpkg")
sardinia_border_line <- sf::st_read("./Data/gadm41_ITA_1.json/sardinia_border_line.gpkg")

RR <- readRDS("./Data/Sardinia_roads/RR.rds")

movement_factors <- calculate_slope(x = dem, y = RR)

movement_factors <- sf::st_as_sf(movement_factors, coords = c("to_x", "to_y"), crs = sf::st_crs(dem))
movement_factors$to_x <- sf::st_coordinates(movement_factors)[,1]
movement_factors$to_y <- sf::st_coordinates(movement_factors)[,2]

movement_factors <- movement_factors %>%
  group_by(from_unique_id) %>%
  filter(all(abs(mathematical_slope) < 1)) %>%
  ungroup() %>%
  group_by(from_unique_id) %>%
  filter(sum(in_adj, na.rm = TRUE) == 1) %>%
  ungroup() %>%
  group_by(from_unique_id) %>%
  filter(
    all(!is.na(mathematical_slope))) %>%
  ungroup()

# ensure that each 'strata' (from_unique_id) have 7 possible alternatives
movement_factors <- movement_factors %>%
  group_by(from_unique_id) %>%
  filter(n() == 7) %>%
  ungroup()

movement_factors$mathematical_slope_abs <- abs(movement_factors$mathematical_slope)
movement_factors$math_slope_tobler <-  tobler_cf(movement_factors$mathematical_slope)
movement_factors$math_slope_herzog <- herzog_cf(movement_factors$mathematical_slope)
movement_factors$math_slope_wheeled <- wt_cf(x = movement_factors$mathematical_slope, crit_slope  = 12)

movement_factors$route_ID <- factor(movement_factors$route_ID)

movement_factors$mathematical_slope_abs_deg <- slope2deg(movement_factors$mathematical_slope_abs)

length(unique(movement_factors$mathematical_slope_abs_deg))
length(unique(round(movement_factors$mathematical_slope_abs_deg, 1)))
length(unique(round(movement_factors$mathematical_slope_abs_deg, 0)))

# Aggregate to every 2 degrees
movement_factors$mathematical_slope_abs_deg2 <- round(movement_factors$mathematical_slope_abs_deg / 2) * 2

movement_factors$elevation <- terra::extract(x = dem, y = sf::st_coordinates(movement_factors))[,1]/1000
movement_factors$elevation2 <- as.numeric(scale(movement_factors$elevation))

saveRDS(movement_factors, "./Output/Model_data/movement_factors.rds")

math_slope_selection_plot <- ggplot(data = sf::st_drop_geometry(movement_factors)) +
                                       geom_density(aes(x = mathematical_slope_abs, colour = as.factor(in_adj)), show.legend = TRUE) + 
                                       scale_colour_manual(values = rev(c("#2166AC", "#B2182B")), labels = rev(c("Not Selected", "Selected"))) + 
                                       scale_x_continuous(breaks = seq(-1, 1, 0.1), labels = paste0(seq(-1, 1, 0.1), "\n(", round(slope2deg(seq(-1, 1, 0.1)), 1), "°)")) +
                                       labs(x = "Mathematical slope gradient (Degrees, °)", y = "Density", colour = NULL, title = "B") + 
                                       theme_clean() + 
                                       theme(legend.position = "bottom", legend.justification = "right", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), legend.background = element_blank())

ggsave(plot = math_slope_selection_plot, filename = "./Output/Figures/math_slope_selection_plot.png", dpi = 300, width = 6, height = 4)
ggsave(plot = math_slope_selection_plot, filename = "./Output/Figures/math_slope_selection_plot.svg", dpi = 300, width = 6, height = 4)

movement_factors$route_ID2 <- paste0("Road ", movement_factors$route_ID)
movement_factors$route_ID2 <- factor(movement_factors$route_ID2, levels = unique(movement_factors$route_ID2))

math_slope_selection_plot2 <- ggplot(data = sf::st_drop_geometry(movement_factors)) +
  geom_density(aes(x = mathematical_slope_abs, colour = as.factor(in_adj)), show.legend = TRUE) + 
  scale_colour_manual(values = rev(c("#2166AC", "#B2182B")), labels = rev(c("Not Selected", "Selected"))) + 
  facet_wrap(~route_ID2, scales = "free_y") + 
  scale_x_continuous(breaks = seq(0, 1, 0.2), labels = paste0(seq(0, 1, 0.2), " (", round(slope2deg(seq(0, 1, 0.2)), 1), "°)")) +
  labs(x = "Mathematical slope gradient (Degrees, °)", y = "Density", colour = NULL, title = NULL) +
  theme_clean() + 
  theme(legend.position = "bottom", legend.justification = "right", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.background = element_blank(), plot.background = element_rect(color = "white"), strip.background = element_blank(), panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))

ggsave(plot = math_slope_selection_plot2, filename = "./Output/Figures/math_slope_selection_plot2.png", dpi = 300, width = 14, height = 14)
ggsave(plot = math_slope_selection_plot2, filename = "./Output/Figures/math_slope_selection_plot2.svg", dpi = 300, width = 14, height = 14)
