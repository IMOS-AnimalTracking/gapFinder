library(data.table)
library(minpack.lm)
library(doParallel)
library(foreach)

get_global_ranges <- function(input,
                              parallel = FALSE,
                              max_anchor = 1400,
                              min_slope = -30,
                              max_slope = -1e-4,
                              n_cores = parallel::detectCores() - 1) {

  data.table::setDT(input)
  input <- input[station_detected != "None"]
  stations <- unique(input$station_detected)

  message("Calculating global + daily NLO ranges")

  station_worker <- function(stat, input, max_dist) {
    df_stat <- input[station_detected == stat]
    dates <- sort(unique(df_stat$date))
    period <- cumsum(c(1, diff(dates) > 1)) # keeps track of consecutive periods

    # -----------------------------
    # GLOBAL MODEL
    # -----------------------------
    global_df <- unique(rbind(
      df_stat[, .(x = distance_detected, p = efficiency)],
      data.table(x = c(50, max_dist), p = c(0.99, 0.001))
    ))
    global_df[p >= 1, p := 0.9999]
    global_df[p <= 0, p := 1e-6]

    b1_start <- -0.01
    b0_start <- qlogis(mean(global_df$p)) - b1_start * mean(global_df$x)

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

    b0_global <- coef(global_fit)["b0"]
    b1_global <- coef(global_fit)["b1"]

    # -----------------------------
    # DAILY MODELS
    # -----------------------------
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
      b0_start_day <- qlogis(mean(fit_df$p)) - b1_start_day * mean(fit_df$x)

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
        b0_day <- coef(daily_fit)["b0"]
        b1_day <- coef(daily_fit)["b1"]
      } else {
        # fallback to global coefficients if daily fit fails
        b0_day <- b0_global
        b1_day <- b1_global
      }

      ed50_day <- -b0_day / b1_day

      ranges_day[[j]] <- data.table(
        date = dates[j],
        station_name = stat,
        b0 = b0_day,
        b1 = b1_day,
        ed50 = ed50_day,
        period = period[j]
      )

      x_grid <- seq(0, max_dist, length.out = 50)
      curves_day[[j]] <- data.table(
        date = dates[j],
        station_name = stat,
        x = x_grid,
        p = plogis(b0_day + b1_day * x_grid)
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

  # -----------------------------
  # SERIAL / PARALLEL EXECUTION
  # -----------------------------
  if (!parallel) {
    out <- lapply(stations, station_worker, input = input, max_dist = max_anchor)
  } else {
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    out <- foreach::foreach(
      stat = stations,
      .packages = c("data.table", "minpack.lm")
    ) %dopar% {
      station_worker(stat, input, max_anchor)
    }
  }

  # -----------------------------
  # COMBINE RESULTS
  # -----------------------------
  list(
    global = data.table::rbindlist(lapply(out, `[[`, "global")),
    ranges = data.table::rbindlist(lapply(out, `[[`, "ranges")),
    curves = data.table::rbindlist(lapply(out, `[[`, "curves"))
  )
}
