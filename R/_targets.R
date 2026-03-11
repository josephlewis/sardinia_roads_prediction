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
  tar_target(run_004, {
    source("./R/004_fit_models.R")
    "./R/004_fit_models.R" 
  }, format = "file"),
  
  tar_target(run_005, {
    run_005
    source("./R/005_model_plot_rw2.R")
    "./R/005_model_plot_rw2.R"
  }, format = "file"),
  
  tar_target(run_006, {
    run_006 
    source("./R/006_model_plot_rank_pop.R")
    "./R/006_model_plot_rank_pop.R"
  }, format = "file"),
  
  tar_target(run_007, {
    run_007
    source("./R/007_model_plot_rank_re.R")
    "./R/007_model_plot_rank_re.R"
  }, format = "file"),
  
  tar_target(run_008, {
    run_008
    source("./R/008_model_comparison_plot.R")
    "./R/008_model_comparison_plot.R"
  }, format = "file"),
  
  tar_target(run_009, {
    run_009
    source("./R/009_model_plot_rank_pop2.R")
    "./R/009_model_plot_rank_pop2.R"
  }, format = "file"),
  
  tar_target(run_010, {
    run_010
    source("./R/010_model_plots_mean_curve.R")
    "./R/010_model_plots_mean_curve.R"
  }, format = "file")
)