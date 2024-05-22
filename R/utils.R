
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
  
  ## Get overlapping area
  overlap_sr <- x * y
  ## Extract only intersecting area
  cropped <- crop(overlap_sr, vect(z))
  
  return(cropped)
  
}


## PLOTS 
plot_lidar_footprint <- function(data, boundaries) {
  
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
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_text(angle = 90),
      legend.position = "bottom",
      legend.text = element_text(size = 12)
    )
  
}

plot_sun_shadow <- function(data, boundaries) {
  
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
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_text(angle = 90),
      legend.position = "bottom",
      legend.text = element_text(size = 12)
    )
  
}

plot_overlap <- function(data, boundaries) {
  
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
    scale_fill_manual(
      values = c("#F4E9CD", "#659B5E"),
      labels = c("Not overlapped", "Overlapped"),
      na.translate = FALSE
    ) +
    coord_sf(
      default_crs = st_crs(data),
      datum       = st_crs(data)
    ) +
    labs(
      fill = NULL
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



