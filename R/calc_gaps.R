#' @title Calculate spatial gap between stations 
#'
#' @description Function used to calculate gaps between sequence of stations with expected overlapping range
#'
#' @param type Position of station in the sequence: "first", "middle", "last"
#' @param input from get_daily_ranges
#' @param j Station position
#' @param date Date of interest
#' @param station_order The names of the stations with expected overlapping ML corrected ranges, from first to last.
#' 
#' @return Matrix of station spatial overlap
#' 
#' @export
#' 
calc_gaps <- function(type, 
  input, 
  j,
  date, 
  station_order) {

          if (type == "first") {
            station1 <- subset(input, station_name == station_order[j])
            station2 <- subset(input, station_name == station_order[j+1])
            # Only calculate overlaps if actual ranges were calculated
            if (station1$cor_range > 1 & station2$cor_range > 1) {
              aux.data <- data.frame(
                lon = c(station1$lon, station2$lon),
                lat = c(station1$lat, station2$lat),
                radius_m = c(station1$cor_range, station2$cor_range))
              # Calculate overlap matrix
              overlap_matrix <- suppressWarnings(get_overlap_matrix(aux.data))
              # Calculate gap size when overlap is zero
              if (overlap_matrix[2,1] == 0) {
                pts <- sf::st_as_sf(aux.data, coords = c("lon", "lat"), crs = 4326)
                pts_m <- sf::st_transform(pts, 3857)
                # Create buffers in meters
                buffs <- sf::st_buffer(pts_m, dist = aux.data$radius_m)
                gap_size1 <- round(as.numeric(sf::st_distance(buffs[1,], buffs[2,])), 2)              
                over_per1 <- 0 # No overlap!
              } else {
                gap_size1 <- 0 # No gap, some overlap found!
                over_per1 <- round(overlap_matrix[1,2] / max(c(overlap_matrix[1,1], overlap_matrix[2,2])), 2)
              }
            } else { # No range calculated for some of the stations, return NA
              gap_size1 <- NA 
              over_per1 <- NA
            }
            # Combine values to export         
            over_save <- data.frame(station = station_order[j],
              date = date,
              overlap_before = NA, # No overlap to the before, first station
              gap_before = NA,     # No gap to the before, first station
              overlap_after = over_per1,
              gap_after = gap_size1
            )
          }
          if (type == "middle") {
            station1 <- subset(input, station_name == station_order[j-1])
            station2 <- subset(input, station_name == station_order[j])
            station3 <- subset(input, station_name == station_order[j+1])
            # Only calculate overlaps if actual cor_ranges were calculated
            if (station1$cor_range > 1 & station2$cor_range > 1) {
              aux.data <- data.frame(
                lon = c(station1$lon, station2$lon),
                lat = c(station1$lat, station2$lat),
                radius_m = c(station1$cor_range, station2$cor_range))
              # Calculate overlap matrix
              overlap_matrix <- suppressWarnings(get_overlap_matrix(aux.data))
              # Calculate gap size when overlap is zero
              if (overlap_matrix[2,1] == 0) {
                pts <- sf::st_as_sf(aux.data, coords = c("lon", "lat"), crs = 4326)
                pts_m <- sf::st_transform(pts, 3857)
                # Create buffers in meters
                buffs <- sf::st_buffer(pts_m, dist = aux.data$radius_m)
                gap_size1 <- round(as.numeric(sf::st_distance(buffs[1,], buffs[2,])), 2)              
                over_per1 <- 0 # No overlap!
              } else {
                gap_size1 <- 0 # No gap, some overlap found!
                over_per1 <- round(overlap_matrix[1,2] / max(c(overlap_matrix[1,1], overlap_matrix[2,2])), 2)
              }
            } else { # No cor_range calculated for some of the stations, return NA
              gap_size1 <- NA 
              over_per1 <- NA
            }
            if (station2$cor_range > 1 & station3$cor_range > 1) {
              aux.data <- data.frame(
                lon = c(station2$lon, station3$lon),
                lat = c(station2$lat, station3$lat),
                radius_m = c(station2$cor_range, station3$cor_range))
              # Calculate overlap matrix
              overlap_matrix <- suppressWarnings(get_overlap_matrix(aux.data))
              # Calculate gap size when overlap is zero
              if (overlap_matrix[2,1] == 0) {
                pts <- sf::st_as_sf(aux.data, coords = c("lon", "lat"), crs = 4326)
                pts_m <- sf::st_transform(pts, 3857)
                # Create buffers in meters
                buffs <- sf::st_buffer(pts_m, dist = aux.data$radius_m)
                gap_size2 <- round(as.numeric(sf::st_distance(buffs[1,], buffs[2,])), 2)              
                over_per2 <- 0 # No overlap!
              } else {
                gap_size2 <- 0 # No gap, some overlap found!
                over_per2 <- round(overlap_matrix[1,2] / max(c(overlap_matrix[1,1], overlap_matrix[2,2])), 2)
              }
            } else { # No cor_range calculated for some of the stations, return NA
              gap_size2 <- NA 
              over_per2 <- NA
            }
            # Combine values to export         
            over_save <- data.frame(station = station_order[j],
              date = date,
              overlap_before = over_per1, # No overlap to the before, first station
              gap_before = gap_size1,     # No gap to the before, first station
              overlap_after = over_per2,
              gap_after = gap_size2
            )
          }  
          if (type == "last") {
            station1 <- subset(input, station_name == station_order[j-1])
            station2 <- subset(input, station_name == station_order[j])
            # Only calculate overlaps if actual cor_ranges were calculated
            if (station1$cor_range > 1 & station2$cor_range > 1) {
              aux.data <- data.frame(
                lon = c(station1$lon, station2$lon),
                lat = c(station1$lat, station2$lat),
                radius_m = c(station1$cor_range, station2$cor_range))
              # Calculate overlap matrix
              overlap_matrix <- suppressWarnings(get_overlap_matrix(aux.data))
              # Calculate gap size when overlap is zero
              if (overlap_matrix[2,1] == 0) {
                pts <- sf::st_as_sf(aux.data, coords = c("lon", "lat"), crs = 4326)
                pts_m <- sf::st_transform(pts, 3857)
                # Create buffers in meters
                buffs <- sf::st_buffer(pts_m, dist = aux.data$radius_m)
                gap_size1 <- round(as.numeric(sf::st_distance(buffs[1,], buffs[2,])), 2)              
                over_per1 <- 0 # No overlap!
              } else {
                gap_size1 <- 0 # No gap, some overlap found!
                over_per1 <- round(overlap_matrix[1,2] / max(c(overlap_matrix[1,1], overlap_matrix[2,2])), 2)
              }
            } else { # No cor_range calculated for some of the stations, return NA
              gap_size1 <- NA 
              over_per1 <- NA
            }
            # Combine values to export         
            over_save <- data.frame(station = station_order[j],
              date = date,
              overlap_before = over_per1, 
              gap_before = gap_size1,     
              overlap_after = NA, # No overlap, last station
              gap_after = NA      # No gap, last station
            )
          }
          return(over_save)
        }
