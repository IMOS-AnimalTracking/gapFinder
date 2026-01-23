#' @title Summary of range performance metrics
#'
#' @description This function calculates the summary statistics of range variation for all stations of interest. 
#'
#' @param input File exported from get_daily_ranges()
#' @param station_order The names of the stations with expected overlapping ranges, from first to last.
#' 
#' @return A dataframe containing the range performance metrics
#' 
#' @export
#' 
range_diag <- function(input, station_order) {
  options(dplyr.summarise.inform = FALSE)

  if(!all(station_order %in% unique(input$ranges$station_name)))
    stop("Some station names were not found in the input dataset. Please check names provided.")
  input <- subset(input$ranges, station_name %in% station_order)
  input$station_name <- factor(input$station_name, levels = station_order)
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
    .groups = "drop")
      

  return(suppressWarnings(df_sum))
}