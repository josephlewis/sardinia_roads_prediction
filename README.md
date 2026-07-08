# A data-driven framework for inferring selection preferences from archaeological routes

This repository contains all the data and scripts required to fully reproduce all analyses presented in the paper "_A data-driven framework for inferring selection preferences from archaeological routes_" authored by **Lewis**, J

Getting Started
---------------

1. Open project using sardinia_roads_prediction.Rproj to ensure relative paths work.
2. Run the _00_Main.R_ R script in the R folder to reproduce the analysis. This project uses the R package _targets_ so only necessarily code will be re-run.

**Code**
   * _001_Preprocess_dem.R_ preprocesses the Digital Elevation Model (DEM) used in the analysis.
   * _002_preprocess_routes.R_ preprocesses the Roman roads dataset into road segments
   * _003_preprocess_movement_factors.R_ preprocesses Roman road segments into data structure for modelling
   * _004_fit_models.R_ fits the partial pooling model to Roman roads in Sardinia
   * _005_model_plots.R_ generates predictions of relative selection strength at the population- and individual road segment-level
   
**Data**
1. gadm41_ITA_1.json
  * _gadm41_ITA_1.json_ - border outline of Italy
  * _sardinia_border.gpkg_ - polygon border outline of Sardinia
  * _sardinia_border_line.gpkg_ - line border outline of Sardinia
2. Sardinia_DEM
  * _dtm_elev.lowestmode_gedi.eml_mf_30m_0..0cm_2000..2018_eumap_epsg3035_v0.3_subset.tif_ - Digital Elevation Model (DEM) of Sardinia. Derived from Hengl et al. (2020) Continental Europe Digital Terrain Model at 30 m resolution. (https://doi.org/10.5281/ZENODO.4724549).
  * _DEM_modifed.tif_ - modified DEM. See _001_Preprocess_dem.R_
3. Sardinia_ports
  * _ports.csv_ - location of key Roman ports in Sardinia for plotting
4. Sardinia_roads
  * _Roman_roads_MASTINO.gpkg_ - Roman roads in Sardinia based on Mastino (2005)
  * _stations.gpkg_ - Overnight stopping places in Sardinia based on Mastino (2005)
  * _RR.gpkg_ - modified Roman roads to separate Roman_roads_MASTINO in road segments. See _002_preprocess_routes.R_
  * _intersections.gpkg_ - intersections where Roman roads are separated into segments. Intersections include stopping stations, cities, and where two or more Roman roads intersect
  * _RR.rds_ - Segmented Roman roads. See _002_preprocess_routes.R_

**Output**
1. Model_data
  * _movement_factors.rds_ - Roman road segments structured for modelling
  * _movement_factors_strata_count.rds_ - Summary of Roman road segments
2. Models
  * _M1_slope.rds_ - Partial pooling model. See _004_fit_models.R_
3. Predictions
  * _RR2.rds_ - Roman road segments with mean averaged relative selection weight for plotting purposes
  * _RR2.gpkg_ - Roman road segments with mean averaged relative selection weight for plotting purposes

**Analysis**

Please note that running the Bayesian (INLA) models via _004_fit_models.R_ can have high memory requirements (recommended 32GB RAM). Due to the size of the files, these are not included.

License
---------------
CC-BY 3.0 unless otherwise stated (see Licenses.md)
