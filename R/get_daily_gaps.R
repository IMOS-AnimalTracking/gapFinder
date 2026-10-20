#' @title Calculate potential gaps in the station sequence
#'
#' @description This function calculates gaps in the station sequence with daily resolution.
#' If the ranges contain a "circadian" column, gaps are calculated separately within each circadian group.
#' When a station has no corrected range (cor_range) for a given day, its minimum corrected range
#' across the study period (min_range) is used instead, and the station is flagged
#' with range_flag = 2 and cor_flag = 2.
#'
#' @param input File output from get_daily_ranges()
#' @param start_time Initial date and time of interest in Y-m-d format. Default is entire data available.
#' @param stop_time Final date and time of interest in Y-m-d format. Default is entire data available.
#' @param tz When start_time and stop_time are provided, their respective timezone.
#' @param station_order The names of the stations with expected overlapping ranges, from first to last.
#' @param parallel Should the daily gaps be calculated in parallel? Default is TRUE.
#' @param n_cores How many cores should be used for parallel computing. Default it total available - 1.
#' @param range_type Range flag to select (default NULL is both): 1 = Modelled range (default), 2 = Minimum estimated range
#' @param cor_type Correction flag to select (default NULL is both): 1 = ML corrected range (default), 2 = Missing diagnostic data 
#' 
#' @return Data frame containing the daily gaps and percentage of overlaps for all stations included in the "station_order" argument.
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
get_daily_gaps <- function(input,
  start_time = NULL,
  stop_time = NULL,
  tz = NULL,
  station_order,
  parallel = TRUE,
  n_cores = parallel::detectCores() - 1,
  range_type = NULL, 
  cor_type = NULL) {

  options(dplyr.summarise.inform = FALSE)
  input <- input$ranges

  # circadian?
  circ <- "circadian" %in% names(input)
  if (circ) message("Circadian column found: gaps calculated separately for each circadian group.")

  if (!is.null(start_time) && is.null(tz))
    stop("start_time provided but no timezone. Please provide tz for your study area.")
  if (!is.null(stop_time) && is.null(tz))
    stop("stop_time provided but no timezone. Please provide tz for your study area.")
  if (!is.null(start_time))
    input <- input[input$date >= as.Date(start_time, format = "%Y-%m-%d", tz = tz), ]
  if (!is.null(stop_time))
    input <- input[input$date <= as.Date(stop_time, format = "%Y-%m-%d", tz = tz), ]

  # Initial check
  input <- input[which(input$station_name %in% station_order), ]
  if (nrow(input) == 0)
    stop("station_order stations not present in the data. Please check input file.")
  input$station_name <- factor(input$station_name, levels = station_order)
  if (!is.null(range_type)) {
    input <- input[which(input$range_flag == range_type),]
  }
  if (!is.null(cor_type)) {
    input <- input[which(input$cor_flag == cor_flag),]
  }

  # Station locations and minimum corrected range (NA if a station never had a valid cor_range)
  df_locs <- input %>%
    dplyr::group_by(station_name) %>%
    dplyr::summarise(
      x = mean(lon, na.rm = TRUE),
      y = mean(lat, na.rm = TRUE),
      min_range = {
        v <- cor_range[!is.na(cor_range) & cor_range > 1]
        if (length(v) == 0) NA_real_ else min(v)
      },
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

  no_min <- df_locs$station_name[is.na(df_locs$min_range)]
  if (length(no_min) > 0)
    message("No corrected range available on any day for: ",
            paste(no_min, collapse = ", "),
            ". Gaps involving these stations will be NA.")

  # Units of calculation: date, or date x circadian
  if (circ) {
    units <- unique(data.frame(date = input$date,
                               circadian = as.character(input$circadian),
                               stringsAsFactors = FALSE))
    units <- units[order(units$date, units$circadian), ]
  } else {
    units <- data.frame(date = sort(unique(input$date)))
  }
  rownames(units) <- NULL

  run_unit <- function(ii) {
    gaps_one_unit(input, units$date[ii],
                  if (circ) units$circadian[ii] else NULL,
                  station_order, df_locs)
  }

  message("Calculating potential gaps...")
  if (parallel) {
    message("Running in parallel on ", n_cores, " cores...progress bar not available in parallel mode")
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    doParallel::registerDoParallel(cl)
    df.save <- foreach::foreach(ii = seq_len(nrow(units)),
      .combine = function(...) dplyr::bind_rows(...),
      .packages = c("dplyr", "sf", "gapFinder"),
      .export = c("gaps_one_unit", "pair_gap", "pick_radius")) %dopar% {
        gaps_one_unit(input, units$date[ii],
                      if (circ) units$circadian[ii] else NULL,
                      station_order, df_locs)
      }
  } else {
    pb <- utils::txtProgressBar(min = 0, max = nrow(units), initial = 0, style = 3, width = 50)
    res <- vector("list", nrow(units))
    for (ii in seq_len(nrow(units))) {
      res[[ii]] <- run_unit(ii)
      utils::setTxtProgressBar(pb, ii)
    }
    close(pb)
    df.save <- dplyr::bind_rows(res)
  }

  # Add range and cor_range flags (key includes circadian when present)
  make_key <- function(st, dt, ci = NULL) {
    if (is.null(ci)) paste(st, dt, sep = "_") else paste(st, dt, ci, sep = "_")
  }
  df.save$aux <- make_key(df.save$station, df.save$date,
                          if (circ) df.save$circadian else NULL)
  input$aux   <- make_key(input$station_name, input$date,
                          if (circ) as.character(input$circadian) else NULL)
  df.save$range_flag <- input$range_flag[match(df.save$aux, input$aux)]
  df.save$cor_flag   <- input$cor_flag[match(df.save$aux, input$aux)]
  df.save$aux <- NULL

  # Flag = 2 where no flag exists (no range that day) OR where min_range was used
  used <- df.save$used_min_range
  df.save$range_flag <- set_flag(df.save$range_flag, is.na(df.save$range_flag) | used)
  df.save$cor_flag   <- set_flag(df.save$cor_flag,   is.na(df.save$cor_flag)   | used)
  df.save$used_min_range <- NULL
    # Fix flag for stations with only minimum range and no correction
    aux_stat <- input %>%
      dplyr::group_by(station_name, range_flag, cor_flag) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::filter(range_flag == 0 & cor_flag == 0)
    if (nrow(aux_stat) > 0) {
      index <- which(df.save$station %in% aux_stat$station_name)
      df.save$range_flag[index] <- 0
      df.save$cor_flag[index] <- 0
    }

  names(df.save)[1] <- "station_name"

  # Put circadian right after date
  if (circ) {
    df.save <- df.save[, c("station_name", "date", "circadian",
                           setdiff(names(df.save), c("station_name", "date", "circadian")))]
  }
  df.save
}


# Internal: all station gaps for one date (and one circadian group, if given)
#' @noRd
gaps_one_unit <- function(input, date, circadian = NULL, station_order, df_locs) {

  keep <- input$date == date
  if (!is.null(circadian)) keep <- keep & as.character(input$circadian) == circadian
  aux <- input[keep, ]

  # Stations with no data on this date / group: add as placeholders (ranges filled below)
  missing_idx <- which(!station_order %in% aux$station_name)
  if (length(missing_idx) > 0) {
    aux.missing <- aux[rep(1, length(missing_idx)), ]
    aux.missing$station_name <- station_order[missing_idx]
    aux.missing$lat <- df_locs$y[match(aux.missing$station_name, df_locs$station_name)]
    aux.missing$lon <- df_locs$x[match(aux.missing$station_name, df_locs$station_name)]
    aux.missing$tot_stations_detected <- 0
    aux.missing$daily_efficiency <- 0
    aux.missing$max_distance_detected <- 0
    aux.missing$range <- NA_real_
    aux.missing$cor_range <- NA_real_
    aux <- rbind(aux, aux.missing)
  }
  aux$station_name <- factor(aux$station_name, levels = station_order)
  aux <- aux[order(aux$station_name), ]

  # No usable cor_range (absent station, NA, or <= 1): use the station's min_range
  use_min <- is.na(aux$cor_range) | aux$cor_range <= 1
  min_r   <- df_locs$min_range[match(aux$station_name, df_locs$station_name)]
  aux$range[use_min]     <- min_r[use_min]
  aux$cor_range[use_min] <- min_r[use_min]
  aux$used_min <- use_min

  over_save <- do.call(rbind, lapply(seq_along(station_order), function(i) {
    type <- if (i == 1) "first" else if (i == length(station_order)) "last" else "middle"
    calc_gaps(type, aux, i, date, station_order)
  }))

  # Record which stations used min_range, so flags can be set to 2 / 2
  over_save$used_min_range <- aux$used_min[match(over_save$station, as.character(aux$station_name))]

  if (!is.null(circadian)) over_save$circadian <- circadian
  over_save
}