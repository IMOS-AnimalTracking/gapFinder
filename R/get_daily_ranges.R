#' @title Calculate daily ranges
#'
#' @description This function calculates daily acoustic detection ranges for all stations present in the dataset. 
#'
#' @param input File exported from get_daily_dets()
#' 
#' @return A list containing the daily ranges ("rages") and daily logistic regression curves ("curves")
#' 
#' @export
# Calculate daily ranges using logistic regression models
get_daily_ranges <- function(input) {
  data.table::setDT(input)
  # Drop "None" stations
  input <- input[station_detected != "None"]
  max_dist <- max(input$distance_detected, na.rm = TRUE) + 100 
  message("Calculating daily ranges")
  stations <- unique(input$station_detected)
  ranges_list <- vector("list", length(stations))
  curves_list <- vector("list", length(stations))
  pb <- utils::txtProgressBar(0, length(stations), style = 3, width = 50)
  for (i in seq_along(stations)) {
    stat <- stations[i]
    df_stat <- input[station_detected == stat]
    # Order dates once
    dates <- sort(unique(df_stat$date))
    # Compute period index (run-length encoding on gaps)
    period <- cumsum(c(1, diff(dates) > 1))
    ranges_day <- vector("list", length(dates))
    curves_day <- vector("list", length(dates))
    for (j in seq_along(dates)) {
      df_day <- df_stat[date == dates[j]]
      # Add anchored points
      df <- rbind(
        df_day[, .(x = distance_detected, y = efficiency)],
        data.table::data.table(x = c(0.1, max_dist), y = c(0.99, 0.001))
      )
      # Avoid y == 1
      df[y >= 1, y := 0.9999]
      # Logistic fit
      fit <- stats::lm(stats::qlogis(y) ~ x, data = df)
      a <- stats::coef(fit)[1]
      b <- stats::coef(fit)[2]
      day_range <- -a / b
      # Curve grid
      x_grid <- seq(min(df$x) - 50, max(df$x) + 50, length.out = 25)
      curve_df <- data.table::data.table(
        x = x_grid,
        y = stats::plogis(a + b * x_grid),
        type = "curve"
      )
      points_df <- data.table::data.table(
        x = df$x,
        y = df$y,
        type = "points"
      )
      vline_df <- data.table::data.table(
        x = c(day_range, day_range),
        y = c(0, 1),
        type = "vline"
      )
      plot_df <- data.table::rbindlist(list(curve_df, points_df, vline_df))
      plot_df[, `:=`(
        station_name = stat,
        date = dates[j]
      )]
      curves_day[[j]] <- plot_df
      ranges_day[[j]] <- data.table::data.table(
        date = dates[j],
        station_name = stat,
        lat = df_day$lat_detected[1],
        lon = df_day$lon_detected[1],
        tot_stations_detected = data.table::uniqueN(df_day$station_name_transmission),
        daily_efficiency = mean(df_day$efficiency, na.rm = TRUE),
        max_distance_detected = max(df_day$distance_detected),
        range = day_range,
        period = period[j]
      )
    }
    ranges_list[[i]] <- data.table::rbindlist(ranges_day)
    curves_list[[i]] <- data.table::rbindlist(curves_day)
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)
  # Export list
  list(
    ranges = data.table::rbindlist(ranges_list),
    curves = data.table::rbindlist(curves_list)
  )
}

