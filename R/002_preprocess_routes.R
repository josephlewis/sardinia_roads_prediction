dem <- terra::rast("./Data/Sardinia_DEM/DEM_modifed.tif")
sardinia_border <- sf::st_read("./Data/gadm41_ITA_1.json/sardinia_border.gpkg")

RR <- sf::st_read("./Data/Sardinia_roads/Roman_roads_MASTINO.gpkg")
RR <- sf::st_cast(RR, "LINESTRING")
intersections <- sf::st_read("./Data/Sardinia_roads/intersections.gpkg")

RR <- sfnetworks::st_network_blend(sfnetworks::as_sfnetwork(RR), intersections)
RR <- convert(RR, sfnetworks::to_spatial_subdivision)

RR <- RR %>%
  activate("edges") %>%
  sf::st_as_sf()

RR <- sf::st_transform(RR, crs = sf::st_crs(dem))
RR$length_km <- as.numeric(sf::st_length(RR)/1000)
RR <- RR[RR$length_km >= 1,]

RR$ID <- 1:nrow(RR)
sf::st_geometry(RR) <- "geometry"

RR <- RR[c("ID", "length_km")]
RR <- sf::st_transform(RR, crs = sf::st_crs(sardinia_border))

sf::st_write(RR, "./Data/Sardinia_roads/RR.gpkg", append = FALSE)
saveRDS(RR, "./Data/Sardinia_roads/RR.rds")