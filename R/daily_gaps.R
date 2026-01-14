#' @title Calculate potential gaps in the station sequence
#'
#' @description This function calculates gaps in the station sequence with daily resolution.
#'
#' @param input File output from get_daily_ranges()
#' @param station_name Column in the metadata file identifying the station names.
#' @param station_order The names of the stations with expected overlapping ranges, from first to last.  
#' 
#' @return Data frame containing the daily gaps and percentage of overlaps for all stations included in the "station_order" argument. 
#' 
#' @importFrom magrittr %>%
#' 
#' @export
#' 
daily_gaps <- function(input, 
  station_name, 
  station_order) {
  
  options(dplyr.summarise.inform = FALSE)
  input <- input$ranges
  # Initial check
  input <- input[which(input[,station_name] %in% station_order),]
  if (nrow(input) == 0)
    stop("station_order stations not present in the data. Please check input file.")
  input[,station_name] <- factor(input[,station_name], levels = station_order)
  names(input)[which(names(input) == station_name)] <- "station_name"  
  # Get total locations
  df_locs <- input %>%
    dplyr::group_by(station_name) %>%
    dplyr::summarise(x = mean(lon),
      y = mean(lat))
  # Calculate gaps
  dates <- unique(input$date)
  df.save <- NULL
  message("Calculating potential gaps")
  pb <- utils::txtProgressBar(min = 0, max = length(dates), initial = 0, style = 3, width = 50)
  for (ii in 1:length(dates)) {
    aux <- subset(input, date == dates[ii])
    # Identify stations with missing range - couldn't be calculated
    index <- which(!station_order %in% aux[,station_name])
    # If only one station with available range - can't calculate actual gaps
    if (length(index) == (length(station_order) - 1)) {
      over_save <- data.frame(station = station_order,
        date = dates[ii],
        overlap_left = NA,
        gap_left = NA,
        overlap_right = NA,
        gap_right = NA)
    } else { # At least two stations with available range
      # Add stations with missing range
      aux.missing <- aux[rep(1, length(index)),]
      aux.missing[,station_name] <- station_order[which(!station_order %in% aux$station_name)]
      aux.missing$lat <- df_locs$y[match(aux.missing$station_name, df_locs$station_name)]
      aux.missing$lon <- df_locs$x[match(aux.missing$station_name, df_locs$station_name)]
      aux.missing$tot_stations_detected <- 0
      aux.missing$daily_efficiency <- 0
      aux.missing$max_distance_detected <- 0
      aux.missing$range <- 1 # Arbitrarily low so overlaps can be calculated 
      aux <- rbind(aux, aux.missing)
      # Reorder to station order
      aux$station_name <- aux[,station_name] # Aux to facilitate indexing
      aux$station_name <- factor(aux$station_name, levels = station_order)
      aux <- aux[order(aux$station_name),]
      # Calculate gaps
      over_save <- NULL
      for (i in 1:length(station_order)) {
        # First point of the line
        if (i == 1) {
          aux_run <- calc_gaps(type = "first", input = aux, j = i, date = dates[ii], station_order = station_order)      
        }
        # Middle points of the line
        if (i > 1 & i < length(station_order)) {
          aux_run <- calc_gaps(type = "middle", input = aux, j = i, date = dates[ii], station_order = station_order)      
        }
        # Last point of the line
        if (i == length(station_order)) {
          aux_run <- calc_gaps(type = "last", input = aux, j = i, date = dates[ii], station_order = station_order)      
        }
        over_save <- rbind(over_save, aux_run)
      }
    }
    df.save <- rbind(df.save, over_save)
    utils::setTxtProgressBar(pb, ii) 
  } 
  close(pb)
  return(df.save)
}
