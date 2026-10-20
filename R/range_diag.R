#' @title Summary of range performance metrics
#'
#' @description This function calculates the summary statistics of range variation for all stations of interest. 
#'
#' @param input File exported from get_daily_ranges()
#' @param start_time Initial date and time of interest in Y-m-d format. Default is entire data available.
#' @param stop_time Final date and time of interest in Y-m-d format. Default is entire data available.
#' @param tz When start_time and stop_time are provided, their respective timezone.
#' @param station_order The names of the stations with expected overlapping ranges, from first to last.
#' @param range_type Range flag to select (default NULL is both): 1 = Modelled range (default), 2 = Minimum estimated range
#' @param cor_type Correction flag to select (default NULL is both): 1 = ML corrected range (default), 2 = Missing diagnostic data 
#' 
#' @return A dataframe containing the range performance metrics
#' 
#' @export
#' 
range_diag <- function(input, 
  start_time = NULL,
  stop_time = NULL,
  tz = NULL,
  station_order,
  range_type = NULL, 
  cor_type = NULL) {
  options(dplyr.summarise.inform = FALSE)

  if(!all(station_order %in% unique(input$ranges$station_name)))
    stop("Some station names were not found in the input dataset. Please check names provided.")
  input <- subset(input$ranges, station_name %in% station_order)
  input$station_name <- factor(input$station_name, levels = station_order)
  
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
  if (!is.null(range_type)) {
    input <- input[which(input$range_flag == range_type),]
  }
  if (!is.null(cor_type)) {
    input <- input[which(input$cor_flag == cor_flag),]
  }

  deploy_time <- as.numeric(difftime(max(input$date), min(input$date), units = "days"))
  options(dplyr.summarise.inform = FALSE)
  # Calculate metrics at station level
  df_sum <- input %>%
    dplyr::group_by(station_name) %>%
    dplyr::summarise(days_range_prop = round(length(unique(date)) / deploy_time, 2),
      min_transmitters = min(tot_stations_detected),
      max_transmitters = max(tot_stations_detected),
      min_distance = min(max_distance_detected),
      max_distance = max(max_distance_detected),
      min_efficiency = round(min(daily_efficiency), 2),
      mean_efficiency = round(mean(daily_efficiency), 2),
      max_efficiency = round(max(daily_efficiency), 2),
      min_range = if (all(is.na(range))) {
          NA_real_
        } else {
          round(min(range, na.rm = TRUE), 2)
        },
      mean_range = if (all(is.na(range))) {
          NA_real_
        } else {
          round(mean(range, na.rm = TRUE), 2)
        },
      max_range = if (all(is.na(range))) {
          NA_real_
        } else {
          round(max(range, na.rm = TRUE), 2)
        },
      min_cor_range = if (all(is.na(cor_range))) {
          NA_real_
        } else {
          round(min(cor_range, na.rm = TRUE), 2)
        },
      mean_cor_range = if (all(is.na(cor_range))) {
          NA_real_
        } else {
          round(mean(cor_range, na.rm = TRUE), 2)
        },
      max_cor_range = if (all(is.na(cor_range))) {
          NA_real_
        } else {
          round(max(cor_range, na.rm = TRUE), 2)
        },
    .groups = "drop") 

    # Fix values for stations with no data
    index <- which(df_sum$max_efficiency == 0)
    if (length(index) > 0) 
      df_sum$days_range_prop[index] <- 0
      
  return(suppressWarnings(df_sum))
}
