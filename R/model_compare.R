model_compare <- function(input, parallel = FALSE, max_anchor = 1400, model,
                             n_cores = parallel::detectCores() - 1) {

  data.table::setDT(input)
  input <- input[station_detected != "None"]
  max_dist <- max(input$distance_detected, na.rm = TRUE) + 100
  # if (max_dist < 1000) 
  max_dist <- max_anchor # Discuss with Hugh!
  stations <- unique(input$station_detected)
  message("Calculating daily ranges")
  # Worker for a single station
  station_worker <- function(stat, input, max_dist) {
    df_stat <- input[station_detected == stat]
    dates <- sort(unique(df_stat$date))
    period <- cumsum(c(1, diff(dates) > 1))
    ranges_day <- vector("list", length(dates))
    curves_day <- vector("list", length(dates))
    for (j in seq_along(dates)) {
      df_day <- df_stat[date == dates[j]]
      if (max(df_day$distance_detected) > 0) {
        df <- rbind(
          df_day[, .(x = distance_detected, y = efficiency)],
          data.table::data.table(x = c(0.1, max_dist), y = c(0.99, 0.001))
        )
        df[y >= 1, y := 0.9999]
        fit <- stats::lm(stats::qlogis(y) ~ x, data = df)
        a <- stats::coef(fit)[1]
        b <- stats::coef(fit)[2]
        day_range <- -a / b
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
        plot_df[, `:=`(station_name = stat, date = dates[j])]
        curves_day[[j]] <- plot_df
        ranges_day[[j]] <- data.table::data.table(
          date = dates[j],
          station_name = stat,
          lat = df_day$lat_detected[1],
          lon = df_day$lon_detected[1],
          tot_stations_detected =
            data.table::uniqueN(df_day$station_name_transmission),
          daily_efficiency = mean(df_day$efficiency, na.rm = TRUE),
          max_distance_detected = max(df_day$distance_detected),
          range = day_range,
          period = period[j]
        )
      } else {
        plot_df <- data.table::data.table(
          x = 0,
          y = 0,
          type = c("curve", "points", "vline"),
          station_name = stat,
          date = dates[j]
        )
        curves_day[[j]] <- plot_df
        ranges_day[[j]] <- data.table::data.table(
          date = dates[j],
          station_name = stat,
          lat = NA,
          lon = NA,
          tot_stations_detected = 0,
          daily_efficiency = 0,
          max_distance_detected = 0,
          range = NA,
          period = period[j]
        )
      }
    }
    list(
      ranges = data.table::rbindlist(ranges_day),
      curves = data.table::rbindlist(curves_day)
    )
  }
  # Serial vs parallel execution
  if (!parallel) {
    pb <- utils::txtProgressBar(0, length(stations), style = 3, width = 50)
    out <- vector("list", length(stations))
    for (i in seq_along(stations)) {
      out[[i]] <- station_worker(stations[i], input, max_dist)
      utils::setTxtProgressBar(pb, i)
    }
    close(pb)
  } else {
    message("Running in parallel on ", n_cores, " cores...progress bar not available in parallel mode")
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    out <- foreach::foreach(
      stat = stations,
      .packages = c("data.table", "stats"),
      .errorhandling = "stop"
    ) %dopar% {
      station_worker(stat, input, max_dist)
    }
  }
  # Combine results
  list(
    ranges = data.table::rbindlist(lapply(out, `[[`, "ranges")),
    curves = data.table::rbindlist(lapply(out, `[[`, "curves"))
  )
}
