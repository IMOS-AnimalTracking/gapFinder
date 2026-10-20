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

  # Get daily means
  df_diag <- diag_data$diag %>%
    dplyr::mutate(noise_num = as.numeric(`Noise Mean (mV)`),
      temp_num = as.numeric(`Ambient Temperature (deg C)`)) %>%
    dplyr::filter(station_name %in% unique(input$station_detected)) %>%
    dplyr::mutate(date = as.Date(Time, tz = "UTC")) %>%
    dplyr::group_by(station_name, date) %>%
    dplyr::summarise(noise = mean(noise_num, na.rm = TRUE),
      temp = mean(temp_num, na.rm = TRUE))

  data.table::setDT(input)
  # input <- input[station_detected != "None"]
  stations <- unique(input$station_transmission)

  # Set circadian helper
  if ("circadian" %in% names(input)) {
      circadian <- TRUE
    } else {
      circadian <- FALSE
    }

  station_worker <- function(stat, input, max_dist, min_slope, max_slope,
                           use_circadian = FALSE) {

  logit_fit <- function(df_sub, b1_start, b0_fallback = NULL) {
    fit_df <- unique(rbind(
      df_sub[, .(x = distance_detected, p = efficiency)],
      data.table::data.table(x = c(50, max_dist), p = c(0.99, 0.001))
    ))
    fit_df[p >= 1, p := 0.9999]
    fit_df[p <= 0, p := 1e-6]

    b1_start <- max(min(b1_start, max_slope), min_slope)
    b0_start <- stats::qlogis(mean(fit_df$p)) - b1_start * mean(fit_df$x)

    fit <- tryCatch(
      minpack.lm::nlsLM(
        p ~ 1 / (1 + exp(-(b0 + b1 * x))),
        data = fit_df,
        start = list(b0 = b0_start, b1 = b1_start),
        lower = c(-Inf, min_slope),
        upper = c(Inf, max_slope),
        control = minpack.lm::nls.lm.control(maxiter = 1000)
      ),
      error = function(e) NULL
    )
    if (is.null(fit)) return(NULL)
    list(b0 = unname(stats::coef(fit)["b0"]), b1 = unname(stats::coef(fit)["b1"]))
  }

  make_grid <- function(dates) {
    if (use_circadian) CJ(date = dates, circadian = c("day", "night"), sorted = TRUE)
    else data.table(date = dates)
  }

  df_stat <- input[station_detected == stat]

  # ---- Station never detected ----
  if (nrow(df_stat) == 0) {
    df_tx  <- input[station_transmission == stat]
    dates  <- seq(min(df_tx$date), max(df_tx$date), by = 1)
    lon0   <- df_tx$lon_transmission[1]
    lat0   <- df_tx$lat_transmission[1]

    ranges <- make_grid(dates)
    ranges[, `:=`(station_name = stat, lon = lon0, lat = lat0,
                  b0 = NaN, b1 = NaN, ed50 = NaN,
                  tot_stations_detected = 0L, max_distance_detected = 0,
                  daily_efficiency = 0, period = NaN)]

    curves <- make_grid(dates)
    curves[, `:=`(station_name = stat, x = NaN, p = NaN)]

    return(list(
      global = data.table(station_name = stat, b0 = NaN, b1 = NaN, ed50 = NaN),
      ranges = ranges,
      curves = curves
    ))
  }

  # ---- Station detected ----
  dates <- seq(min(df_stat$date), max(df_stat$date), by = 1)

  # periods of consecutive days WITH detections
  obs_dates <- sort(unique(df_stat$date))
  obs_per   <- cumsum(c(1, diff(as.numeric(obs_dates)) > 1))
  lon0 <- df_stat$lon_detected[1]
  lat0 <- df_stat$lat_detected[1]

  # Global model
  g <- logit_fit(df_stat, b1_start = -0.01)
  if (is.null(g)) {
    warning(paste("Global fit failed for station:", stat))
    return(NULL)
  }

  grid <- make_grid(dates)
  x_grid <- seq(0, max_dist, length.out = 50)

  res <- lapply(seq_len(nrow(grid)), function(j) {
    d  <- grid$date[j]
    df_day <- if (use_circadian) {
      df_stat[date == d & circadian == grid$circadian[j]]
    } else {
      df_stat[date == d]
    }

    f <- logit_fit(df_day, b1_start = g$b1)
    b0 <- if (is.null(f)) g$b0 else f$b0
    b1 <- if (is.null(f)) g$b1 else f$b1

    aux_det <- df_day[station_transmission != station_detected]
    has_det <- nrow(aux_det) > 0

    range <- data.table(
      date = d,
      station_name = stat,
      lon = lon0, lat = lat0,
      b0 = b0, b1 = b1, ed50 = -b0 / b1,
      tot_stations_detected = uniqueN(aux_det$station_transmission),
      max_distance_detected = if (has_det) max(aux_det$distance_detected, na.rm = TRUE) else 0,
      daily_efficiency      = if (has_det) mean(aux_det$efficiency, na.rm = TRUE) else 0,
      period = obs_per[match(d, obs_dates)]
    )
    curve <- data.table(date = d, station_name = stat,
                        x = x_grid, p = stats::plogis(b0 + b1 * x_grid))

    if (use_circadian) {
      range[,  circadian := grid$circadian[j]]
      curve[,  circadian := grid$circadian[j]]
      setcolorder(range, c("date", "circadian"))
      setcolorder(curve, c("date", "circadian"))
    }
    list(range = range, curve = curve)
  })

  list(
    global = data.table(station_name = stat, b0 = g$b0, b1 = g$b1,
                        ed50 = -g$b0 / g$b1),
    ranges = rbindlist(lapply(res, `[[`, "range")),
    curves = rbindlist(lapply(res, `[[`, "curve"))
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

  # Add minimum range for days with no detections 
  df_ranges <- data.table::rbindlist(lapply(out, `[[`, "ranges"))
  df_ranges <- df_ranges %>%
    dplyr::mutate(date = as.Date(date, tz = "UTC")) %>%
    dplyr::left_join(
      df_diag %>% dplyr::select(station_name, date, noise, temp),
      by = c("station_name", "date")
    )

  aux_zero <- subset(df_ranges, daily_efficiency == 0)
  if (nrow(aux_zero) > 0) {
    # Find minimum ranges per station
    aux_min <- df_ranges %>%
      dplyr::filter(station_name %in% aux_zero$station_name) %>%
      dplyr::group_by(station_name) %>%
      dplyr::summarise(min_ed50 = min(ed50))
    # Minimum distance: min ML corrected range - 5% of minimum detected distance
    dist_fac <- min(df_ranges$max_distance_detected[which(df_ranges$max_distance_detected > 0)])
    dist_fac <- round(dist_fac * (5 / 100), 0) #
    aux_min$min_ed50 <- round(aux_min$min_ed50 - dist_fac, 2)
    # Add minimum distance to gaps (allow gaps to still be calculated)
    index <- which(is.na(aux_min$min_ed50))
    if (length(index) > 0) 
      aux_min$min_ed50[index] <- dist_fac
    # Add minimum to no detections
    index <- which(df_ranges$tot_stations_detected == 0)
    if (length(index) > 0)
      df_ranges$ed50[index] <- aux_min$min_ed50[match(df_ranges$station_name[index], aux_min$station_name)]
    
    # Add range flags
    df_ranges$range_flag <- 1 # Modelled Range
    df_ranges$range_flag[index] <- 2 # Min estimated range
    index_na <- which(is.na(df_ranges$b0))
      if (length(index_na) > 0)
        df_ranges$range_flag[index_na] <- 0
    df_ranges$range_flag <- as.factor(df_ranges$range_flag)
  }
  
  # ML correction of ranges
  message("Applying ML correction to modelled ranges")
    
    # Apply model correction: for only dates with diagnostic data and efficiency
    wf_fit <- gapFinder::ML_correction # UPDATE FILE IN PACKAGE!
    df_ranges <- df_ranges %>%
      dplyr::mutate(.row = dplyr::row_number())
    df_ranges_pred <- df_ranges %>%
      dplyr::select(.row, ed50, daily_efficiency, noise, temp) %>%
      tidyr::drop_na()
    fit <- workflows::extract_fit_parsnip(wf_fit)
    df_ranges_pred <- df_ranges_pred %>%
      dplyr::bind_cols(suppressWarnings(stats::predict(fit, new_data = df_ranges_pred)))
    df_ranges_pred <- rename(df_ranges_pred, pred_range_diff = .pred)
    df_ranges_pred <- df_ranges_pred %>%
      dplyr::mutate(cor_ed50 = ed50 - pred_range_diff)    
    df_ranges <- df_ranges %>%
      dplyr::left_join(df_ranges_pred %>% dplyr::select(.row, cor_ed50), by = ".row") %>%
      dplyr::select(-c(.row))
      
      # Add correction flags
      df_ranges$cor_flag <- 1
      df_ranges$cor_flag[which(is.na(df_ranges$temp))] <- 2
      df_ranges$cor_flag[which(is.na(df_ranges$noise))] <- 2
      df_ranges$cor_flag[which(is.na(df_ranges$b0))] <- 0
      df_ranges$cor_flag <- as.factor(df_ranges$cor_flag)

    # Using modelled ranges is corrected is missing or negative
    index <- which(is.na(df_ranges$cor_ed50))
    if (length(index) > 0) 
      df_ranges$cor_ed50[index] <- df_ranges$ed50[index]       
    index <- which(df_ranges$cor_ed50 < 0)
    if (length(index) > 0) 
      df_ranges$cor_ed50[index] <- df_ranges$ed50[index]       
    
  # Combine output
  output <- list(
    global = data.table::rbindlist(lapply(out, `[[`, "global")),
    ranges = data.table::data.table(df_ranges),
    curves = data.table::rbindlist(lapply(out, `[[`, "curves"))
  )

  # # Add minimum range for days with no detections 
  # aux_zero <- subset(output$ranges, is.na(daily_efficiency))
  # if (nrow(aux_zero) > 0) {
  #   # Find minimum ranges per station
  #   aux_min <- output$ranges %>%
  #     dplyr::filter(station_name %in% aux_zero$station_name) %>%
  #     dplyr::group_by(station_name) %>%
  #     dplyr::summarise(min_ed50 = min(ed50))
  #   # Minimum distance: min ML corrected range - 5% of minimum detected distance
  #   dist_fac <- min(output$ranges$max_distance_detected[which(output$ranges$max_distance_detected > -Inf)])
  #   dist_fac <- round(dist_fac * (5 / 100), 0) #
  #   dist_min <- round(min(output$ranges$cor_ed50) - dist_fac, 2)
  #   # Add minimum to no detections
  #   index <- which(output$ranges$tot_stations_detected == 0)
  #   output$ranges$ed50[index] <- dist_min
  #   output$ranges$cor_ed50[index] <- dist_min
  #   output$ranges$max_distance_detected[index] <- 0
  #   output$ranges$daily_efficiency[index] <- 0
  #   # Add range flags
  #   output$ranges$range_flag <- 1 # Modelled Range
  #   output$ranges$range_flag[index] <- 2 # Min estimated range
  #     index_na <- which(is.na(output$ranges$range))
  #     if (length(index_na) > 0)
  #       output$ranges$range[index2] <- 0
  #   output$ranges$range_flag <- as.factor(output$ranges$range_flag)

  #   # ML fix the minimum ranges were diagnostic data is present
  #   df_ranges <- output$ranges
  #   df_ranges <- df_ranges %>%
  #       dplyr::mutate(.row = dplyr::row_number())
  #     df_ranges_pred <- df_ranges %>%
  #       dplyr::select(.row, ed50, daily_efficiency, noise, temp) %>%
  #       tidyr::drop_na()
  #     df_ranges_pred <- df_ranges_pred %>%
  #       dplyr::bind_cols(stats::predict(wf_fit, new_data = df_ranges_pred) %>% rename(pred_range_diff = .pred)) %>%
  #       dplyr::mutate(cor_ed50_new = ed50 - pred_range_diff)    
  #     # Correct negative ranges to be minimum 
  #     df_ranges_pred$cor_ed50_new[which(df_ranges_pred$cor_ed50_new < 0)] <- dist_min
  #     df_ranges_pred$cor_ed50_new[which(is.na(df_ranges_pred$cor_ed50_new))] <- dist_min
  #     # Add new minimum range corrected values    
  #     df_ranges <- df_ranges %>%
  #       dplyr::left_join(df_ranges_pred %>% dplyr::select(.row, cor_ed50_new), by = ".row") %>%
  #       dplyr::select(-c(.row))
  #     df_ranges$cor_ed50_new[is.na(df_ranges$cor_ed50_new)] <- dist_min
  #     df_ranges$cor_ed50[which(df_ranges$range_flag == 2)] <- df_ranges$cor_ed50_new[which(df_ranges$range_flag == 2)]
  #     df_ranges <- df_ranges %>%
  #           dplyr::select(-c(noise, temp, cor_ed50_new))
  #   output$ranges <- df_ranges  
  # } else {
  #   output$ranges$range_flag <- 1 # Modelled Range
  # }

  # Reorganize output
  if (circadian) {
    output$ranges <- output$ranges %>%
    dplyr::select(c(date, circadian, period, station_name, lon, lat, b0, b1, tot_stations_detected, max_distance_detected,
      daily_efficiency, ed50, range_flag, cor_ed50, cor_flag))
  } else {
    output$ranges <- output$ranges %>%
      dplyr::select(c(date, period, station_name, lon, lat, b0, b1, tot_stations_detected, max_distance_detected,
        daily_efficiency, ed50, range_flag, cor_ed50, cor_flag))
  }  
  names(output$ranges)[which(names(output$ranges) %in% c("ed50", "cor_ed50"))] <- c("range", "cor_range")
  
  return(output)
}
