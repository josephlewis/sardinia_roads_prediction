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
    
    # --- 1. Densify Points & Identify Cells (Existing Logic) ---
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
    
    # --- 2. THE FIX: Create Sequence Map based on Path Order ---
    # We map every 'from' cell to its step number (1, 2, 3...) BEFORE doing any spatial sorting
    step_map <- data.frame(
      from = y_cells2,
      from_unique_id = 1:length(y_cells2)
    )
    
    # --- 3. Calculate Adjacency & Slopes (Existing Logic) ---
    adj <- terra::adjacent(x = x, y_cells2, directions = 8, pairs = TRUE, symmetrical = FALSE)
    adj <- data.frame(adj)
    
    from_xy <- terra::xyFromCell(object = x, cell = adj$from)
    colnames(from_xy) <- c("from_x", "from_y")
    to_xy <- terra::xyFromCell(object = x, cell = adj$to)
    colnames(to_xy) <- c("to_x", "to_y")
    
    rise <- unlist((x[adj[,2]] - x[adj[,1]]))
    run <- calculate_distance_geosphere(x = x, adj = adj)
    mathematical_slope <- rise/run
    
    # --- 4. Identify the "True Path" Choice ---
    y_matrix <- data.frame(cbind(head(y_cells, -1), tail(y_cells, -1)))
    colnames(y_matrix) <- c("from", "to")
    y_matrix$in_adj <- 1
    
    # --- 5. Merge Data & Apply the Sequence ID ---
    adj <- adj %>%
      # A. Mark the true path
      left_join(y_matrix, by = c("from", "to")) %>%
      mutate(in_adj = ifelse(is.na(in_adj), 0, 1)) %>%
      # B. THE FIX: Attach the step sequence ID using the map we made in Step 2
      # This ensures ID 1 is the Origin, ID 2 is the next step, etc.
      left_join(step_map, by = "from")
    
    # --- 6. Construct the Result Dataframe ---
    dem_slopes[[y_indx]] <- data.frame(
      route_ID = y_indx, 
      from = adj$from, 
      to = adj$to, 
      from_unique_id = adj$from_unique_id, # Now included correctly
      in_adj = adj$in_adj, 
      from_xy, 
      to_xy, 
      rise = rise, 
      run = run, 
      mathematical_slope = mathematical_slope
    ) %>%
      filter(!is.na(mathematical_slope))
    
    # Handle empty results (e.g. if route is too short/outside raster)
    if(nrow(dem_slopes[[y_indx]]) == 0) { 
      dem_slopes[[y_indx]] <- data.frame(
        route_ID = y_indx, from = NA, from_unique_id = NA, to = NA, in_adj = NA, 
        from_x = NA, from_y = NA, to_x = NA, to_y = NA, 
        rise = NA, run = NA, mathematical_slope = NA
      )
    }
    
    # Attach extra columns from the original sf object if present
    if(length(colnames(y)) > 1) { 
      dem_slopes[[y_indx]] <- cbind(dem_slopes[[y_indx]], as.vector(sf::st_drop_geometry(y[y_indx,])))
    }
  }
  
  # --- 7. Final Bind (Simplified) ---
  # We removed the group_by/summarise block that was causing the sorting bug.
  dem_slopes2 <- do.call(rbind, dem_slopes)
  
  # B. Create the unique ID using the "Combined String" method
  dem_slopes2 <- dem_slopes2 %>%
    # Ensure it is sorted by Route then Local Step so the IDs increase logically
    arrange(route_ID, from_unique_id) %>% 
    mutate(
      # 1. Create a unique string key for every step (e.g. "1_1", "1_2", "2_1")
      combined_id = paste(route_ID, from_unique_id, sep = "_"),
      
      # 2. Match that string against the unique list of strings to get an integer
      #    match() returns the index of the first match, so "1_1" becomes 1, "2_1" becomes 50, etc.
      from_unique_id = match(combined_id, unique(combined_id))
    ) %>%
    # Remove the temp column
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
  
  # A. Find indices for this effect (e.g., "Slope:1", "Slope:2")
  # "Slope" is the name you defined in the formula: Slope = rw2(...)
  idx <- which(startsWith(rownames(sample$latent), paste0(random_effect_name, ":")))
  
  # B. Extract y-values (the cost/weight)
  y_values <- sample$latent[idx, 1]
  
  # Safety Check: Ensure lengths match
  if(length(y_values) != length(x_values_map)) {
    stop(paste("Mismatch! Model has", length(y_values), "nodes but map has", length(x_values_map), "values."))
  }
  
  # C. Return as dataframe
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
  
  # Safety check
  if(length(coefs) != n_groups) stop(paste("Expected", n_groups, "coeffs, got", length(coefs)))
  
  return(coefs) # Returns a vector of betas
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

slope_calc <- function(x, exaggeration = FALSE, neighbours) {
  
  neighbours <- leastcostpath::neighbourhood(neighbours = neighbours)

  cells <- which(!is.na(terra::values(x)))
  na_cells <- which(is.na(terra::values(x)))
  
  adj <- terra::adjacent(x = x, cells = cells, directions = neighbours, pairs = TRUE)
  adj <- adj[!adj[,2] %in% na_cells,]
  
  elev_values <- terra::values(x)[,1]
  
  message("calculating slope...")
  
  rise <- (elev_values[adj[,2]] - elev_values[adj[,1]])
  run <- calculate_distance(x = x, adj = adj)
  
  mathematical_slope <- rise/run
  
  if(exaggeration) { 
    mathematical_slope <- ifelse(mathematical_slope > 0, mathematical_slope * 1.99, mathematical_slope * 2.31)
  }
  
  ncells <- length(cells) + length(na_cells)
  
  rm(elev_values, rise, run)
  gc()
  
  cs_matrix <- Matrix::Matrix(data = 0, nrow = ncells, ncol = ncells, sparse = TRUE)
  cs_matrix[adj] <- mathematical_slope
  
  cs <- list("conductanceMatrix" = cs_matrix, 
             "costFunction" = NA,
             "maxSlope" = NA, 
             "exaggeration" = exaggeration,
             "criticalSlope" = NA,
             "neighbours" = sum(neighbours, na.rm = TRUE),
             "resolution" = terra::res(x), 
             "nrow" = terra::nrow(x), 
             "ncol" = terra::ncol(x), 
             "extent" = as.vector(terra::ext(x)), 
             "crs" = terra::crs(x, proj = TRUE))
  
  class(cs) <- "conductanceMatrix"
  
  return(cs)
}

# create_nc_surface <- function(cs, cells, dem, ncores = 3) { 
#   
#   RcppParallel::setThreadOptions(numThreads = ncores)
#   
#   conductanceMatrix <- summary(cs$conductanceMatrix)
#   conductanceMatrix$x <- 1 / conductanceMatrix$x
#   directed_graph <- cppRouting::makegraph(conductanceMatrix,directed=TRUE)
#   rm(cs)
#   rm(conductanceMatrix)
#   gc()
#   
#   message("created directed graph")
#   
#   dem_empty <- dem
#   dem_empty[!is.na(dem_empty)] <- 0
#   
#   df_list <- split(cells, ceiling(seq_along(cells)/ncores))
#   
#   for(i in 1:length(df_list)) {
#     
#     message(paste0(i, " of ", length(df_list)))
#     
#     lcps <- cppRouting::get_multi_paths(Graph=directed_graph, from = df_list[[i]], to = cells, long = TRUE)
#     
#     lcps_table <- tabulate(as.numeric(lcps[,3]))
#     lcps_nodes <- which(lcps_table != 0)
#     lcps_vals <- lcps_table[lcps_nodes]
#     
#     rm(lcps, lcps_table)
#     gc()
#     
#     dem_empty[lcps_nodes] <- dem_empty[lcps_nodes] + lcps_vals
#     
#     rm(lcps_nodes, lcps_vals)
#     gc()
#   }
#   
#   return(dem_empty)
#   
# }
# 
# get_fixed_horizon_bearing <- function(full_df, horizon_dist = 1000) {
#   
#   # --- STEP 1: Extract the Observed Backbone ---
#   # We need the "True Path" to measure distances and find the physical coordinates
#   # of the markers (1000m, 2000m, etc.)
#   
#   backbone <- full_df %>%
#     filter(in_adj == 1) %>%  # Only taken steps
#     arrange(route_ID, from_unique_id) %>% # Ensure correct sequence
#     group_by(route_ID) %>%
#     mutate(
#       # Calculate segment length of each observed step
#       # (Use Euclidean distance formula)
#       step_len = sqrt((to_x - from_x)^2 + (to_y - from_y)^2),
#       
#       # Cumulative distance at the START of the step
#       # (e.g. Step 1 starts at 0m, Step 2 starts at 30m...)
#       current_dist = lag(cumsum(step_len), default = 0),
#       
#       # Total length of this route
#       total_route_len = sum(step_len)
#     ) %>%
#     ungroup()
#   
#   # --- STEP 2: Define the Target for Each Step ---
#   # We will create a lookup table: For every 'from_unique_id', where should it look?
#   
#   target_map <- backbone %>%
#     group_by(route_ID) %>%
#     do({
#       sub_df <- .
#       
#       # For every observed step, calculate the desired target distance
#       # Logic: Look for the next multiple of horizon_dist (1000, 2000, etc.)
#       # If at 0m -> look for 1000m. If at 900m -> look for 1000m.
#       # If at 1010m -> look for 2000m.
#       
#       desired_dists <- (floor(sub_df$current_dist / horizon_dist) + 1) * horizon_dist
#       
#       # EDGE CASE: If desired distance is beyond the end of the road,
#       # clamp it to the total length (e.g., if road is 3500m, 4000m becomes 3500m)
#       desired_dists <- pmin(desired_dists, sub_df$total_route_len[1])
#       
#       # FUNCTION: Find the coordinates of the point closest to 'd' on this backbone
#       find_closest_coords <- function(d, df) {
#         # Find index where cumulative distance is closest to 'd'
#         idx <- which.min(abs(df$current_dist - d))
#         return(data.frame(t_x = df$from_x[idx], t_y = df$from_y[idx]))
#       }
#       
#       # Apply this finding logic to every step in the route
#       # (We construct a list of target coordinates matching the rows)
#       targets <- lapply(desired_dists, find_closest_coords, df = sub_df) %>%
#         bind_rows()
#       
#       # Return the mapping: Step ID -> Target Coordinates
#       data.frame(
#         from_unique_id = sub_df$from_unique_id,
#         target_x = targets$t_x,
#         target_y = targets$t_y,
#         desired_dist = desired_dists # Just for debugging
#       )
#     }) %>%
#     ungroup()
#   
#   # --- STEP 3: Join and Calculate Bearings ---
#   # Now we join these target coordinates back to the FULL dataset (including available steps)
#   
#   result_df <- full_df %>%
#     left_join(target_map, by = c("route_ID", "from_unique_id")) %>%
#     mutate(
#       # 1. Calculate the Angle of the specific step (Available or Observed)
#       step_rads = atan2(to_x - from_x, to_y - from_y),
#       
#       # 2. Calculate the Angle to the Fixed Horizon Target
#       # Note: We measure from the START of the step (from_x) to the TARGET
#       horizon_rads = atan2(target_x - from_x, target_y - from_y),
#       
#       # 3. Calculate Alignment (Cosine Similarity)
#       # 1 = Direct approach, -1 = Going away
#       fixed_alignment = cos(step_rads - horizon_rads),
#       
#       # (Optional) Convert to Degrees for readability
#       horizon_bearing_deg = (horizon_rads * 180 / pi) %% 360
#     )
#   
#   return(result_df)
# }
