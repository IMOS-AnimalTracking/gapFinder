#' @title Calculate daily ranges 
#'
#' @description This function calculates daily acoustic detection ranges for all stations present in the dataset .
#'
#' @param input File exported from get_daily_dets()
#' @param diag_data Receiver diagnostics data (loaded with FathomR)
#' @param parallel Should the daily ranges be calculated in parallel? Default is FALSE.
#' @param max_anchor Maximum anchoring distance for near zero detection probability.
#' @param min_slope Baseline minimum slope for logistic regression model.
#' @param max_slope Baseline maximum slope for logistic regression model.
#' @param n_cores How many cores should be used for parallel computing. Default it total available - 1.
#' 
#' @importFrom foreach %dopar%
#' 
#' @return A list containing the daily ranges ("rages") and daily logistic regression curves ("curves"), also the global ("global") model estimates
#' 
#' @export
#' 
get_daily_ranges <- function(input,
                            diag_data,
                            parallel = FALSE,
                            max_anchor = 1500,
                            min_slope = -30,
                            max_slope = -0.01,
                            n_cores = parallel::detectCores() - 1) {

  # Get daily means and scale diagnostic noise to account for scale differences
  df_diag <- diag_data$diag %>%
    dplyr::mutate(noise_num = as.numeric(`Noise Mean (mV)`),
      temp_num = as.numeric(`Ambient Temperature (deg C)`)) %>%
    dplyr::filter(station_name %in% unique(input$station_detected)) %>%
    dplyr::mutate(date = as.Date(Time, tz = "UTC")) %>%
    dplyr::group_by(station_name, date) %>%
    dplyr::summarise(noise = mean(noise_num, na.rm = TRUE),
      temp = mean(temp_num, na.rm = TRUE),)

  # summary(df_diag) # Missing values = receiver lost! Use modelled data for those dates! 
  data.table::setDT(input)
  input <- input[station_detected != "None"]
  stations <- unique(input$station_detected)

  station_worker <- function(stat, input, max_dist, min_slope, max_slope) {
    df_stat <- input[station_detected == stat]
    dates <- seq(min(df_stat$date), max(df_stat$date), 1)
    period <- cumsum(c(1, diff(dates) > 1)) # keeps track of consecutive periods
    
    # GLOBAL MODEL
    global_df <- unique(rbind(
      df_stat[, .(x = distance_detected, p = efficiency)],
      data.table::data.table(x = c(50, max_dist), p = c(0.99, 0.001))
    ))
    global_df[p >= 1, p := 0.9999]
    global_df[p <= 0, p := 1e-6]

    b1_start <- -0.01
    b0_start <- stats::qlogis(mean(global_df$p)) - b1_start * mean(global_df$x)
    
    global_fit <- tryCatch(
      minpack.lm::nlsLM(
        p ~ 1 / (1 + exp(-(b0 + b1 * x))),
        data = global_df,
        start = list(b0 = b0_start, b1 = b1_start),
        lower = c(-Inf, min_slope),
        upper = c(Inf, max_slope),
        control = minpack.lm::nls.lm.control(maxiter = 1000)
      ),
      error = function(e) NULL
    )

    if (is.null(global_fit)) {
      warning(paste("Global fit failed for station:", stat))
      return(NULL)
    }

    b0_global <- stats::coef(global_fit)["b0"]
    b1_global <- stats::coef(global_fit)["b1"]

    # DAILY MODELS
    ranges_day <- vector("list", length(dates))
    curves_day <- vector("list", length(dates))

    for (j in seq_along(dates)) {
      df_day <- df_stat[date == dates[j]]
      
      # Prepare daily fit data with anchors
      fit_df <- unique(rbind(
        df_day[, .(x = distance_detected, p = efficiency)],
        data.table(x = c(50, max_dist), p = c(0.99, 0.001))
      ))
      fit_df[p >= 1, p := 0.9999]
      fit_df[p <= 0, p := 1e-6]

      # Robust starting values
      b1_start_day <- max(min(b1_global, max_slope), min_slope)
      b0_start_day <- stats::qlogis(mean(fit_df$p)) - b1_start_day * mean(fit_df$x)

      # Fit daily model
      daily_fit <- tryCatch(
        minpack.lm::nlsLM(
          p ~ 1 / (1 + exp(-(b0 + b1 * x))),
          data = fit_df,
          start = list(b0 = b0_start_day, b1 = b1_start_day),
          lower = c(-Inf, min_slope),
          upper = c(Inf, max_slope),
          control = minpack.lm::nls.lm.control(maxiter = 1000)
        ),
        error = function(e) NULL
      )

      if (!is.null(daily_fit)) {
        b0_day <- stats::coef(daily_fit)["b0"]
        b1_day <- stats::coef(daily_fit)["b1"]
      } else {
        # fallback to global coefficients if daily fit fails
        b0_day <- b0_global
        b1_day <- b1_global
      }

      ed50_day <- -b0_day / b1_day

      # Stations and distances detected
      aux_det <- subset(df_day, station_name_transmission != station_detected)
      n_stats <- length(unique(aux_det$station_name_transmission))
      max_dist_det <- max(aux_det$distance_detected, na.rm = TRUE)
      mean_eff <- mean(aux_det$efficiency, na.rm = TRUE)

      # Save data
      ranges_day[[j]] <- data.table(
        date = dates[j],
        station_name = stat,
        lon = df_day$lon_detected[1],
        lat = df_day$lat_detected[1],
        b0 = b0_day,
        b1 = b1_day,
        ed50 = ed50_day,
        tot_stations_detected = n_stats,
        max_distance_detected = max_dist_det,
        daily_efficiency = mean_eff,
        period = period[j]
      )

      x_grid <- seq(0, max_dist, length.out = 50)
      curves_day[[j]] <- data.table(
        date = dates[j],
        station_name = stat,
        x = x_grid,
        p = stats::plogis(b0_day + b1_day * x_grid)
      )
    }

    list(
      global = data.table(
        station_name = stat,
        b0 = b0_global,
        b1 = b1_global,
        ed50 = -b0_global / b1_global
      ),
      ranges = data.table::rbindlist(ranges_day),
      curves = data.table::rbindlist(curves_day)
    )
  }


  # SERIAL / PARALLEL EXECUTION
  if (!parallel) {
    message("Calculating daily ranges")
    out <- lapply(stations, station_worker, input = input, max_dist = max_anchor,
      min_slope = min_slope, max_slope = max_slope)
  } else {
    message("Calculating daily ranges in parallel")
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    out <- foreach::foreach(
      stat = stations,
      .packages = c("data.table", "minpack.lm", "gapFinder")
    ) %dopar% {
      station_worker(stat, input, max_anchor,
        min_slope, max_slope)
    }
  }

  # ML correction of modelled ranges
  message("Applying ML correction to modelled ranges")
  df_ranges <- data.table::rbindlist(lapply(out, `[[`, "ranges"))
  df_ranges <- df_ranges %>%
    dplyr::mutate(date = as.Date(date, tz = "UTC")) %>%
    dplyr::left_join(
      df_diag %>% dplyr::select(station_name, date, noise, temp),
      by = c("station_name", "date")
    ) 
    # Apply model correction: for only dates with diagnostic data and efficiency
    wf_fit <- gapFinder::ML_correction
    df_ranges <- df_ranges %>%
      dplyr::mutate(.row = dplyr::row_number())
    df_ranges_pred <- df_ranges %>%
      dplyr::mutate(eff = daily_efficiency) %>%
      dplyr::select(.row, ed50, eff, noise, temp) %>%
      tidyr::drop_na()
    df_ranges_pred <- df_ranges_pred %>%
      dplyr::bind_cols(terra::predict(wf_fit, new_data = df_ranges_pred) %>% rename(pred_range_diff = .pred)) %>%
      dplyr::mutate(cor_ed50 = ed50 - pred_range_diff)    
    df_ranges <- df_ranges %>%
      dplyr::left_join(df_ranges_pred %>% dplyr::select(.row, cor_ed50), by = ".row") %>%
      dplyr::select(-c(.row))
      # Add correction flags
      df_ranges$cor_flag <- 1
      df_ranges$cor_flag[which(is.na(df_ranges$temp))] <- 2
      df_ranges$cor_flag[which(is.na(df_ranges$noise))] <- 2
      df_ranges$cor_flag <- as.factor(df_ranges$cor_flag)
    df_ranges$cor_ed50[is.na(df_ranges$cor_ed50)] <- df_ranges$ed50[is.na(df_ranges$cor_ed50)] # Use modelled values when receiver data not available!
    df_ranges <- df_ranges %>%
          dplyr::select(-c(noise, temp))

  # Combine output
  output <- list(
    global = data.table::rbindlist(lapply(out, `[[`, "global")),
    ranges = data.table::data.table(df_ranges),
    curves = data.table::rbindlist(lapply(out, `[[`, "curves"))
  )

  # Add minimum range for days with no detections 
  aux_zero <- subset(output$ranges, is.na(daily_efficiency))
  if (nrow(aux_zero) > 0) {
    # Find minimum ranges per station
    aux_min <- output$ranges %>%
      dplyr::filter(station_name %in% aux_zero$station_name) %>%
      dplyr::group_by(station_name) %>%
      dplyr::summarise(min_ed50 = min(ed50))
    # Minimum distance: min ML corrected range - 5% of minimum detected distance
    dist_fac <- min(output$ranges$max_distance_detected[which(output$ranges$max_distance_detected > -Inf)])
    dist_fac <- round(dist_fac * (5 / 100), 0) #
    dist_min <- round(min(output$ranges$cor_ed50) - dist_fac, 2)
    # Add minimum to no detections
    index <- which(output$ranges$tot_stations_detected == 0)
    output$ranges$ed50[index] <- dist_min
    output$ranges$cor_ed50[index] <- dist_min
    output$ranges$max_distance_detected[index] <- 0
    output$ranges$daily_efficiency[index] <- 0
    # Add range flag
    output$ranges$range_flag <- 1 # Modelled Range
    output$ranges$range_flag[index] <- 2 # Min estimated range
    output$ranges$range_flag <- as.factor(output$ranges$range_flag)
  } else {
    output$ranges$range_flag <- 1 # Modelled Range
  }

  # Fill missing locations
  index <- which(is.na(output$ranges$lat))
  if (length(index) > 0) {
    aux_locs <- output$ranges %>%
      dplyr::group_by(station_name) %>%
      dplyr::summarise(y = mean(lat, na.rm = TRUE), 
        x = mean(lon, na.rm = TRUE))
        
    output$ranges$lon[index] <- aux_locs$x[match(output$ranges$station_name[index],
      aux_locs$station_name)]
    output$ranges$lat[index] <- aux_locs$y[match(output$ranges$station_name[index],
      aux_locs$station_name)]
  }

  # Reorganize output
  output$ranges <- output$ranges %>%
    dplyr::select(c(date, period, station_name, lon, lat, b0, b1, tot_stations_detected, max_distance_detected,
      daily_efficiency, ed50, range_flag, cor_ed50, cor_flag))
  names(output$ranges)[which(names(output$ranges) %in% c("ed50", "cor_ed50"))] <- c("range", "cor_range")

  return(output)
}
