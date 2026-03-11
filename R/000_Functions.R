# calculate euclidean/geographic distance between adjacent cells in the raster
calculate_distance_geosphere <- function(x, adj) {
  if(sf::st_is_longlat(x)) {
    
    xy1 <- data.frame(terra::xyFromCell(x, adj[, 1]))
    xy2 <- data.frame(terra::xyFromCell(x, adj[, 2]))
    
    # if coordinate system is geographic, use geosphere distHaversine to account for curvature of the earth.
    # if coordinate system is projected, distance can be calculated using euclidean distance formula
    
    # calculate distances between cells and their adjacent cells    
    dist <- as.numeric(geosphere::distHaversine(xy1, xy2))
    
  } else {
    
    xy1 <- terra::xyFromCell(x, adj[, 1])
    xy2 <- terra::xyFromCell(x,adj[, 2])
    
    xy3 <- (xy1[,1] - xy2[,1])^2
    xy4 <- (xy1[,2] - xy2[,2])^2
    
    # calculate distances between cells and their adjacent cells
    dist <- sqrt(xy3 + xy4)
    
  }
  
  return(dist)
}

slope2deg <- function(slope) { (atan(slope) * 180/pi)}

deg2slope <- function(degrees) {tan(degrees * pi / 180)}

calculate_slope <- function(x, y) {
  
  dem_slopes <- list()
  
  for(y_indx in 1:nrow(y)) {
    
    print(paste0("Calculating route mathematical slope gradients: ", y_indx, " of ", nrow(y)))
    
    # densify points and identify cells
    if(sf::st_is_longlat(y)) {
      y_dens <- sf::st_segmentize(y[y_indx,], dfMaxLength = units::set_units(max(terra::res(x))/10, degrees))
    } else {
      y_dens <- sf::st_segmentize(y[y_indx,], dfMaxLength = units::set_units(max(terra::res(x))/10, metres))
    }
    
    y_pts <- suppressWarnings(sf::st_cast(y_dens, "POINT"))
    y_pts_xy <- sf::st_coordinates(y_pts)
    
    # unique() preserves the order of appearance, so this keeps the path sequence (Start -> End)
    y_cells <- unique(terra::cellFromXY(object = x, xy = y_pts_xy))
    
    # Remove destination cell for the 'from' list
    y_cells2 <- y_cells[-length(y_cells)]
    
    # Map every 'from' cell to its step number before doing any spatial sorting
    step_map <- data.frame(
      from = y_cells2,
      from_unique_id = 1:length(y_cells2)
    )
    
    # calculate adjacency and slope gradients
    adj <- terra::adjacent(x = x, y_cells2, directions = 8, pairs = TRUE, symmetrical = FALSE)
    adj <- data.frame(adj)
    
    from_xy <- terra::xyFromCell(object = x, cell = adj$from)
    colnames(from_xy) <- c("from_x", "from_y")
    to_xy <- terra::xyFromCell(object = x, cell = adj$to)
    colnames(to_xy) <- c("to_x", "to_y")
    
    rise <- unlist((x[adj[,2]] - x[adj[,1]]))
    run <- calculate_distance_geosphere(x = x, adj = adj)
    mathematical_slope <- rise/run
    
    # identify selected steps
    y_matrix <- data.frame(cbind(head(y_cells, -1), tail(y_cells, -1)))
    colnames(y_matrix) <- c("from", "to")
    y_matrix$in_adj <- 1
    
      adj <- adj %>%
      left_join(y_matrix, by = c("from", "to")) %>%
      mutate(in_adj = ifelse(is.na(in_adj), 0, 1)) %>%
      left_join(step_map, by = "from")
    
    dem_slopes[[y_indx]] <- data.frame(
      route_ID = y_indx, 
      from = adj$from, 
      to = adj$to, 
      from_unique_id = adj$from_unique_id, 
      in_adj = adj$in_adj, 
      from_xy, 
      to_xy, 
      rise = rise, 
      run = run, 
      mathematical_slope = mathematical_slope
    ) %>%
      filter(!is.na(mathematical_slope))
  
    if(nrow(dem_slopes[[y_indx]]) == 0) { 
      dem_slopes[[y_indx]] <- data.frame(
        route_ID = y_indx, from = NA, from_unique_id = NA, to = NA, in_adj = NA, 
        from_x = NA, from_y = NA, to_x = NA, to_y = NA, 
        rise = NA, run = NA, mathematical_slope = NA
      )
    }
    
    # attach extra columns from the original sf object if present
    if(length(colnames(y)) > 1) { 
      dem_slopes[[y_indx]] <- cbind(dem_slopes[[y_indx]], as.vector(sf::st_drop_geometry(y[y_indx,])))
    }
  }

  dem_slopes2 <- do.call(rbind, dem_slopes)

  dem_slopes2 <- dem_slopes2 %>%
    arrange(route_ID, from_unique_id) %>% 
    mutate(
      # Create a unique string key for every step (e.g. "1_1", "1_2", "2_1")
      combined_id = paste(route_ID, from_unique_id, sep = "_"),
      
      #    match() returns the index of the first match, so "1_1" becomes 1, "2_1" becomes 50, etc.
      from_unique_id = match(combined_id, unique(combined_id))
    ) %>%
    select(-combined_id)
  
  row.names(dem_slopes2) <- 1:nrow(dem_slopes2)
  
  return(dem_slopes2)
}

tobler_cf <- function(x) {
  (1 * exp(-3.5 * abs(x + 0.05))) / 3.6
}

herzog_cf <- function(x) {
  (1/((1337.8 * (x)^6) + (278.19 * (x)^5) - (517.39 * (x)^4) - (78.199 * (x)^3) + (93.419 * (x)^2) + (19.825 * (x)) + 1.64))
}

wt_cf <- function(x, crit_slope) {
  (1/(1 + ((abs(x) * 100)/crit_slope)^2))
}

get_rw2_curve_pop <- function(sample, random_effect_name, x_values_map) {
  
  # Find indices for this effect (e.g., "Slope:1", "Slope:2")
  idx <- which(startsWith(rownames(sample$latent), paste0(random_effect_name, ":")))
  
  y_values <- sample$latent[idx, 1]
  
  if(length(y_values) != length(x_values_map)) {
    stop(paste("Mismatch! Model has", length(y_values), "nodes but map has", length(x_values_map), "values."))
  }

  return(data.frame(x = x_values_map, y = y_values))
}

get_rw2_curve_re <- function(sample, random_effect_name, x_values_map) {
  idx <- which(startsWith(rownames(sample$latent), paste0(random_effect_name, ":")))
  y_values <- sample$latent[idx, 1]
  if(length(y_values) != length(x_values_map)) stop("RW2 Mismatch")
  return(data.frame(x = x_values_map, y = y_values))
}

get_iid_coefs <- function(sample, random_effect_name, n_groups) {
  # Find indices like "Slope2:1", "Slope2:2"
  idx <- which(startsWith(rownames(sample$latent), paste0(random_effect_name, ":")))
  coefs <- sample$latent[idx, 1]
  
  if(length(coefs) != n_groups) stop(paste("Expected", n_groups, "coeffs, got", length(coefs)))
  
  return(coefs)
}

calculate_distance <- function(x, adj) { 
  
  if(sf::st_is_longlat(x)) { 
    
    xy1 <- data.frame(terra::xyFromCell(x, adj[, 1]))
    xy2 <- data.frame(terra::xyFromCell(x, adj[, 2]))
    
    xy1 <- sf::st_as_sf(xy1, coords = c("x", "y"), crs = terra::crs(x))
    xy2 <- sf::st_as_sf(xy2, coords = c("x", "y"), crs = terra::crs(x))
    
    dist <- as.vector(sf::st_distance(x = xy1, xy2, by_element = TRUE))
    
  } else {
    
    xy1 <- terra::xyFromCell(x, adj[, 1])
    xy2 <- terra::xyFromCell(x,adj[, 2])
    
    xy3 <- (xy1[,1] - xy2[,1])^2
    xy4 <- (xy1[,2] - xy2[,2])^2
    
    dist <- sqrt(xy3 + xy4)  
    
  }
  
  return(dist)
  
}