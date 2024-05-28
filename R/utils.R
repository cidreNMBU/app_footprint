
## GET_SUN_POSITION
get_sun_position <- function(data,
                             date  = "2023-06-01 10:00:00",
                             which = "azimuth") {
  
  sun_mat <- data |> 
    st_centroid() |> 
    st_transform(4326) |> 
    solarpos(
      as.POSIXct(date, tz = "Europe/Oslo"),
      crs = st_crs(4326)
    )
  
  if (which == "azimuth") {
    return(sun_mat[, 1])
  } else if (which == "altitude") {
    return(sun_mat[, 2])
  }
  
}



## CALCULATE_SHADOW
calculate_shadow <- function(elevation_raster, azimuth, altitude) {
  
  # Create slope and aspect rasters
  slope <- terrain(elevation_raster, v = "slope", unit = "radians")
  aspect <- terrain(elevation_raster, v = "aspect", unit = "radians")
  
  # Calculate shadow based on slope and aspect
  shadow <- shade(slope, aspect, direction = azimuth, angle = altitude)
  shadow <- ifel(
    shadow > 0, 1, 0
  )
  
  return(shadow)
}


## GET OVERLAPPING AREA
get_overlapping_area <- function(x, y, z) {
  
  ## Modify shade
  y <- ifel(
    y == 1, 2, 0
  )
  ## Get overlapping area
  overlap_sr <- x + y
  ## Extract only intersecting area
  cropped <- crop(overlap_sr, vect(z))
  
  return(cropped)
  
}


## PLOTS 
plot_lidar_footprint <- function(data, uav_data, boundaries) {
  
  ggplot() +
    geom_spatraster(
      data = as.factor(data)
    ) +
    geom_sf(
      data = boundaries,
      fill = NA,
      size = 2,
      lwd  = 1
    ) +
    geom_point(
      data = uav_data,
      aes(position_x, position_y),
      fill  = "red",
      size  = 3,
      shape = 21
    ) +
    scale_fill_manual(
      values = c("#F4E9CD", "#659B5E"),
      labels = c("Not seen", "Seen"),
      na.translate = FALSE
    ) +
    coord_sf(
      default_crs = st_crs(data),
      datum       = st_crs(data)
    ) +
    labs(
      fill = NULL, x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_text(angle = 90),
      legend.position = "bottom",
      legend.text = element_text(size = 12)
    )
  
}

plot_sun_shadow <- function(data, uav_data, boundaries) {
  
  ggplot() +
    geom_spatraster(
      data = as.factor(data)
    ) +
    geom_sf(
      data = boundaries,
      fill = NA,
      size = 2,
      lwd  = 1
    ) +
    geom_point(
      data = uav_data,
      aes(position_x, position_y),
      fill  = "red",
      size  = 3,
      shape = 21
    ) +
    scale_fill_manual(
      values = c("#F4E9CD", "#659B5E"),
      labels = c("Shadowed", "Not shadowed"),
      na.translate = FALSE
    ) +
    coord_sf(
      default_crs = st_crs(data),
      datum       = st_crs(data)
    ) +
    labs(
      fill = NULL, x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_text(angle = 90),
      legend.position = "bottom",
      legend.text = element_text(size = 12)
    )
  
}

plot_overlap <- function(data, uav_data, boundaries) {
  
  ggplot() +
    geom_spatraster(
      data = as.factor(data)
    ) +
    geom_sf(
      data = boundaries,
      fill = NA,
      size = 2,
      lwd  = 1
    ) +
    geom_point(
      data = uav_data,
      aes(position_x, position_y),
      fill  = "red",
      size  = 3,
      shape = 21
    ) +
    scale_fill_manual(
      values = c("#F4E9CD", "#B7D3F2", "#F58F29", "#005377"),
      labels = c("Not seen", "Seen by UAV", "Sunlit", "UAV + Sunlit"),
      na.translate = FALSE
    ) +
    coord_sf(
      default_crs = st_crs(data),
      datum       = st_crs(data)
    ) +
    labs(
      fill = NULL, x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_text(angle = 90),
      legend.position = "bottom",
      legend.text = element_text(size = 12)
    )
  
}

calculate_area <- function(r, boundaries) {
  
  only_1 <- ifel(r == 0, NA, 1)
  only_1 <- mask(only_1, boundaries)
  
  area_ha <- expanse(only_1, "ha")
  
  area_ha <- round(area_ha$area, 2)
  
  return(area_ha)
  
}


## Create interactive map

get_leaflet_map <- function(uav_r, overlapping_sr, shade_sr,
                            lidar_footprint_sr, boundaries_sf, ortophoto) {
  
  ## Convert UAV tibble to sf
  uav_sf <- uav_r |> 
    st_as_sf(
      coords = c("position_x", "position_y"),
      crs    = crs(overlapping_sr)
    ) |> 
    st_transform(4326)
  
  ## Function for popup
  uav_popup <- function(height, plot_id) {
    
    glue::glue(
      "<b>Plot:</b> {plot_id}
    <br>
    <b>Height:</b> {height} meters"
    )
    
  }
  
  ## Classify layer
  overlapping_class <- ifel(
    overlapping_sr == 0, NA, overlapping_sr
  ) |> as.factor()
  
  ## Create map
  leaflet(options = leafletOptions(maxZoom = 20)) |> 
    addProviderTiles(provider = "Esri.WorldImagery") |> 
    addRasterImage(
      x      = overlapping_class,
      colors = c("#B7D3F2", "#F58F29", "#005377"),
      group  = "Overlapping"
    ) |> 
    addLegend(
      colors   = c("#B7D3F2", "#F58F29", "#005377"),
      labels   = c("Seen by UAV", "Sunlit", "UAV + Sunlit"),
      group    = "Overlapping",
      opacity  = 1,
      position = "bottomright"
    ) |>
    addRasterImage(
      x = ifel(
        shade_sr == 1, 1, NA
      ),
      group = "Not shadowed"
    ) |>
    addRasterImage(
      x = ifel(
        lidar_footprint_sr == 1, 1, NA
      ),
      group = "UAV footprint"
    ) |> 
    addCircles(
      data = uav_sf,
      popup = ~uav_popup(height, plot_id)
    ) |> 
    addPolygons(
      data    = boundaries_sf |> st_transform(4326),
      fill    = NA,
      color   = "red",
      opacity = 1,
      group   = "Convex Hull"
    ) |> 
    addRasterRGB(
      ortophoto,
      maxBytes = 1e10
    ) |> 
    addLayersControl(
      overlayGroups = c("Overlapping", "Not shadowed", 
                        "UAV footprint", "Convex Hull"),
      options = layersControlOptions(collapsed = FALSE)
    ) |> 
    addMeasure(
      primaryLengthUnit = "meters",
      primaryAreaUnit   = "hectares"
    ) |> 
    addHomeButton(group = "Convex Hull")
  
}
