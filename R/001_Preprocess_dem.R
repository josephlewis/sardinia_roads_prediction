dem <- terra::rast("./Data/Sardinia_DEM/dtm_elev.lowestmode_gedi.eml_mf_30m_0..0cm_2000..2018_eumap_epsg3035_v0.3_subset.tif")

sardinia_border <- sf::st_read("./Data/gadm41_ITA_1.json/gadm41_ITA_1.json")
sardinia_border <- sardinia_border[sardinia_border$NAME_1 == "Sardegna",]
sardinia_border <- sf::st_cast(sardinia_border, "POLYGON")
sardinia_border <- sardinia_border[which.max(sf::st_area(sardinia_border)),]
sardinia_border <- sf::st_transform(sardinia_border, crs = sf::st_crs(dem))
sardinia_border_line <- sf::st_cast(sardinia_border, "LINESTRING")

sf::st_write(sardinia_border, "./Data/gadm41_ITA_1.json/sardinia_border.gpkg", append = FALSE)
sf::st_write(sardinia_border_line, "./Data/gadm41_ITA_1.json/sardinia_border_line.gpkg", append = FALSE)

dem <- terra::crop(dem, sardinia_border)
dem <- terra::mask(dem, sardinia_border)
dem[dem < 0] <- NA
# coerce DEM into metres
dem <- dem/10

terra::writeRaster(dem, "./Data/Sardinia_DEM/DEM_modifed.tif", overwrite = TRUE)