# Convert E/N to Lat/Lon -------------------------------------------------
convert_coordinates <- function(easting,
                                northing,
                                bng = 27700,
                                wgs84 = 4326) {
  out <- cbind(easting, northing)
  out <- as.data.frame(out)
  mask <- !is.na(easting)
  points_df <- data.frame(
    as.numeric(easting[mask]),
    as.numeric(northing[mask])
  )
  points_sp <- sf::st_as_sf(points_df, coords = c(1, 2), crs = bng)
  sp <- sf::st_transform(points_sp, crs = wgs84)
  out$easting[mask]  <- sf::st_coordinates(sp$geometry)[,1]
  out$northing[mask]  <- sf::st_coordinates(sp$geometry)[,2]
  names(out) <- c("Longitude", "Latitude")
  return(out)
}
