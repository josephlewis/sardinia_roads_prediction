INLA::inla.setOption(num.threads = 1)

set.seed(NULL)
set.seed(123)

movement_factors <- readRDS("./Output/Model_data/movement_factors.rds")

movement_factors_strata_count <- movement_factors %>%
  sf::st_drop_geometry() %>%
  group_by(route_ID) %>%
  summarise(n = length(unique(from_unique_id)))

saveRDS(movement_factors_strata_count, "./Output/Model_data/movement_factors_strata_count.rds")

print(min(movement_factors_strata_count$n))
print(max(movement_factors_strata_count$n))

print(round(mean(movement_factors_strata_count$n), 0))

#########################################################
###### MODEL ONE. SLOPE ONLY. COMPLETE POOLING ##########
#########################################################

M1_slope_components <- ~
  -1 +
  
  Slope(mathematical_slope_abs_deg2, model = "rw2") +
  Strata(from_unique_id, model = "iid", hyper = list(theta = list(initial = log(1e-6), fixed = TRUE)))

M1_slope <- bru(
  components = M1_slope_components,
  formula = in_adj ~ .,
  family = "poisson",
  data = sf::st_drop_geometry(movement_factors),
  options = list(control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE), verbose = FALSE))

saveRDS(M1_slope, "./Output/Models/M1_slope.rds")

########################################################
###### MODEL TWO. SLOPE ONLY. PARTIAL POOLING ##########
########################################################

M2_slope_components <- ~
  -1 +
  Slope(mathematical_slope_abs_deg2, model = "rw2", scale.model = TRUE, constr = TRUE) +
  Slope2(route_ID, mathematical_slope_abs_deg2, model = "iid", hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.05)))) +
  
  Strata(from_unique_id, model = "iid", hyper = list(theta = list(initial = log(1e-6), fixed = TRUE)))

M2_slope <- bru(
  components = M2_slope_components,
  formula = in_adj ~ .,
  family = "poisson",
  data = sf::st_drop_geometry(movement_factors),
  options = list(control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE), verbose = FALSE))

saveRDS(M2_slope, "./Output/Models/M2_slope.rds")

waic_values <- data.frame(model = c(1,2), waic = c(M1_slope$waic$waic, M2_slope$waic$waic))
write.csv(waic_values, "./Output/Models/waic_values.csv", row.names = FALSE)