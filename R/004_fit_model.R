INLA::inla.setOption(num.threads = 1)

set.seed(NULL)
set.seed(123)

movement_factors <- readRDS("./Output/Model_data/movement_factors.rds")

movement_factors_strata_count <- movement_factors %>%
  sf::st_drop_geometry() %>%
  group_by(route_ID) %>%
  summarise(n = length(unique(from_unique_id)))

saveRDS(movement_factors_strata_count, "./Output/Model_data/movement_factors_strata_count.rds")

nrow(movement_factors)
length(unique(movement_factors$from_unique_id))

print(min(movement_factors_strata_count$n))
print(max(movement_factors_strata_count$n))

print(round(mean(movement_factors_strata_count$n), 0))

M1_slope_components <- ~
  -1 +
  Slope(main = mathematical_slope_abs, model = "linear") +
  Slope2(main = route_ID, model = "iid", weights = mathematical_slope_abs, hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.05)))) +
  
  Strata(from_unique_id, model = "iid", hyper = list(theta = list(initial = log(1e-6), fixed = TRUE)))

M1_slope <- bru(
  components = M1_slope_components,
  formula = in_adj ~ .,
  family = "poisson",
  data = sf::st_drop_geometry(movement_factors),
  options = list(control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE), verbose = FALSE))

summary(M1_slope)

saveRDS(M1_slope, "./Output/Models/M1_slope.rds")