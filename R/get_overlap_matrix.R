#' @title Calculate overlap matrix between pair of stations
#'
#' @description Function used to calculate the ammount of spatial overlap between two stations.
#'
#' @param df Data frame of station names, deployment coordinates, and range
#' @param lon Station longitude
#' @param lat Station latitude
#' @param r Name of the range column 
#' @param crs_in The corresponding CRS
#' 
#' @return Matrix of station spatial overlap
#' 
#' @keywords internal
#'  
get_overlap_matrix <- function(df, 
  lon = "lon", 
  lat = "lat", 
  r = "radius_m", 
  crs_in = 4326) {
  # Make point sf
  pts <- sf::st_as_sf(df, coords = c(lon, lat), crs = crs_in)
  # Transform to a suitable projected CRS (meters)
  pts_m <- sf::st_transform(pts, crs = 3857)
  # Buffer each point by its radius in meters
  buffs <- sf::st_buffer(pts_m, dist = df$radius_m)
  # Compute pairwise intersections
  n <- nrow(df)
  mat <- matrix(0, n, n)
  inters <- sf::st_intersects(buffs, buffs)  
  for (i in 1:n) {
    for (j in inters[[i]]) {
      if (j < i) next
      inter <- sf::st_intersection(buffs[i,], buffs[j,])
      area <- if (nrow(inter) > 0) as.numeric(sf::st_area(inter)) else 0
      mat[i,j] <- mat[j,i] <- area
    }
  }
  # diagonal = full circle areas
  mat[cbind(1:n, 1:n)] <- as.numeric(sf::st_area(buffs))
  return(mat)
}
