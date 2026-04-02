#' @title Calculate potential gaps in the station sequence
#'
#' @description This function calculates gaps in the station sequence with daily resolution.
#'
#' @param input File output from get_daily_ranges()
#' @param start_time Initial date and time of interest in Y-m-d format. Default is entire data available.
#' @param stop_time Final date and time of interest in Y-m-d format. Default is entire data available.
#' @param tz When start_time and stop_time are provided, their respective timezone.
#' @param station_order The names of the stations with expected overlapping ranges, from first to last.  
#' @param parallel Should the daily gaps be calculated in parallel? Default is FALSE.
#' @param n_cores How many cores should be used for parallel computing. Default it total available - 1.

#' @return Data frame containing the daily gaps and percentage of overlaps for all stations included in the "station_order" argument. 
#' 
#' @importFrom magrittr %>%
#' 
#' @export
#' 
get_daily_gaps <- function(input, 
  start_time = NULL,
  stop_time = NULL, 
  tz, 
  station_order, 
  parallel = TRUE, 
  n_cores = parallel::detectCores() - 1) {
  
  options(dplyr.summarise.inform = FALSE)
  input <- input$ranges
  
  if (!is.null(start_time) &
      is.null(tz)) {
        stop("start_time provided but no timezone. Please provide tz for your study area.")
      } 
  if (!is.null(stop_time) &
      is.null(tz)) {
        stop("stop_time provided but no timezone. Please provide tz for your study area.")
      }  
  if (!is.null(start_time)) {
    input <- input[input$date >= as.Date(start_time, format = "%Y-%m-%d", tz = tz)]
  }
  if (!is.null(stop_time)) {
    input <- input[input$date <= as.Date(stop_time, format = "%Y-%m-%d", tz = tz)]
  }

  # Initial check
  input <- input[which(input$station_name %in% station_order), ]
  if (nrow(input) == 0)
    stop("station_order stations not present in the data. Please check input file.")
  input$station_name <- factor(input$station_name, levels = station_order)
  # Get total locations
  df_locs <- input %>%
    dplyr::group_by(station_name) %>%
    dplyr::summarise(x = mean(lon, na.rm = TRUE),
              y = mean(lat, na.rm = TRUE),
              .groups = "drop") 
   
  # Remove stations with missing significant range
  missing_idx <- which(is.na(df_locs$x))
  if (length(missing_idx) > 0) {
    message(paste0("No significant ranges available for: ", 
                   paste(unique(df_locs$station_name[missing_idx]), collapse = ", "), 
                   ". Removing stations from gap calculations."))
    input <- input[!input$station_name %in% df_locs$station_name[missing_idx], ]
    df_locs <- df_locs %>% dplyr::filter(!is.na(x))
  }
  dates <- unique(input$date)
  message("Calculating potential gaps...")
  if (parallel) {
    message("Running in parallel on ", n_cores, " cores...progress bar not available in parallel mode")
    # Parallel backend
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    df.save <- foreach::foreach(ii = seq_along(dates), 
      .combine = function(...) dplyr::bind_rows(...), 
      .packages = c("dplyr", "sf", "gapFinder")) %dopar% {
      aux <- subset(input, date == dates[ii]) 
      # Identify stations with missing range
      missing_idx <- which(!station_order %in% aux$station_name)
      if (length(missing_idx) == (length(station_order) - 1)) {
        over_save <- data.frame(
          station = station_order,
          date = dates[ii],
          overlap_before = NA,
          gap_before = NA,
          overlap_after = NA,
          gap_after = NA
        )
      } else {
        # Add missing stations
        if (length(missing_idx) > 0) {
          aux.missing <- aux[rep(1, length(missing_idx)), ]
          aux.missing$station_name <- station_order[missing_idx]
          aux.missing$lat <- df_locs$y[match(aux.missing$station_name, df_locs$station_name)]
          aux.missing$lon <- df_locs$x[match(aux.missing$station_name, df_locs$station_name)]
          aux.missing$tot_stations_detected <- 0
          aux.missing$daily_efficiency <- 0
          aux.missing$max_distance_detected <- 0
          aux.missing$range <- 1
          aux <- rbind(aux, aux.missing)
        }     
        # Reorder to station order
        aux$station_name <- factor(aux$station_name, levels = station_order)
        aux <- aux[order(aux$station_name), ]    
        # Calculate gaps
        over_save <- NULL
        for (i in seq_along(station_order)) {
          type <- if (i == 1) "first" else if (i == length(station_order)) "last" else "middle"
          over_save <- rbind(over_save, 
            calc_gaps(type, aux, i, dates[ii], station_order))
        }
      }    
      over_save
    }  
    parallel::stopCluster(cl)   
  } else {
    # Sequential version with progress bar
    df.save <- NULL
    pb <- utils::txtProgressBar(min = 0, max = length(dates), initial = 0, style = 3, width = 50)
    for (ii in seq_along(dates)) {
      aux <- subset(input, date == dates[ii])
      missing_idx <- which(!station_order %in% aux$station_name)    
      if (length(missing_idx) == (length(station_order) - 1)) {
        over_save <- data.frame(
          station = station_order,
          date = dates[ii],
          overlap_before = NA,
          gap_before = NA,
          overlap_after = NA,
          gap_after = NA
        )
      } else {
        if (length(missing_idx) > 0) {
          aux.missing <- aux[rep(1, length(missing_idx)), ]
          aux.missing$station_name <- station_order[missing_idx]
          aux.missing$lat <- df_locs$y[match(aux.missing$station_name, df_locs$station_name)]
          aux.missing$lon <- df_locs$x[match(aux.missing$station_name, df_locs$station_name)]
          aux.missing$tot_stations_detected <- 0
          aux.missing$daily_efficiency <- 0
          aux.missing$max_distance_detected <- 0
          aux.missing$range <- 1
          aux <- rbind(aux, aux.missing)
        }    
        aux$station_name <- factor(aux$station_name, levels = station_order)
        aux <- aux[order(aux$station_name), ]   
        over_save <- NULL
        for (i in seq_along(station_order)) {
          type <- if (i == 1) "first" else if (i == length(station_order)) "last" else "middle"
          over_save <- rbind(over_save, 
            calc_gaps(type = type, 
              input = aux, j = i, date = dates[ii], station_order = station_order))
        }
      }    
      df.save <- rbind(df.save, over_save)
      utils::setTxtProgressBar(pb, ii)
    }
    close(pb)
  } 

  # Add range and cor_range flags
  df.save$aux <- paste(df.save$station, df.save$date, sep = "_")
  input$aux <- paste(input$station_name, input$date, sep = "_")
  df.save$range_flag <- input$range_flag[match(df.save$aux, input$aux)]
  df.save$cor_flag <- input$cor_flag[match(df.save$aux, input$aux)]
  df.save <- df.save[,-which(names(df.save) == "aux")]
  # NAs are modelled range days with no correction - add flags
  df.save$cor_flag[is.na(df.save$cor_flag)] <- as.factor(2)
  df.save$range_flag[is.na(df.save$range_flag)] <- as.factor(2)
  names(df.save)[1] <- "station_name"

  return(df.save)
}
