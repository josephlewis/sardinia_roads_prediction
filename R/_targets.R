library(targets)

tar_option_set(
  packages = c(
    "terra", "sf", "geosphere", "sfnetworks", "tidygraph",
    "dplyr", "tidyr", "stringr", "data.table",
    "INLA", "inlabru",
    "ggplot2", "tidyterra", "ggsflabel", "ggdist", "ggthemes", "patchwork"),
  memory = "transient", 
  garbage_collection = TRUE)

tar_source("./R/000_Functions.R")

list(
  tar_target(run_001, {
    source("./R/001_Preprocess_dem.R")
    "./R/001_Preprocess_dem.R" 
  }, format = "file"),
  
  tar_target(run_002, {
    run_001
    source("./R/002_preprocess_routes.R")
    "./R/002_preprocess_routes.R"
  }, format = "file"),
  
  tar_target(run_003, {
    run_002
    source("./R/003_preprocess_movement_factors.R")
    "./R/003_preprocess_movement_factors.R"
  }, format = "file"),
  
  tar_target(run_004, {
    run_003 
    source("./R/004_fit_model.R")
    "./R/004_fit_model.R"
  }, format = "file"),
  
  tar_target(run_005, {
    run_004
    source("./R/005_model_plots.R")
    "./R/005_model_plots.R"
  }, format = "file"))