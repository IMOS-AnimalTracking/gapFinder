#' @title Calculate daily gaps between stations
#'
#' @description Calculates gaps and overlaps between consecutive stations for each day.
#' If the ranges contain a "circadian" column, gaps are calculated separately
#' within each circadian group (e.g. day and night).
#'
#' @param input File exported from get_daily_ranges()
#' @param start_time Initial date of interest in Y-m-d format. Default is entire data available.
#' @param stop_time Final date of interest in Y-m-d format. Default is entire data available.
#' @param tz When start_time and stop_time are provided, their respective timezone.
#' @param station_order The names of the stations with expected overlapping ranges, from first to last.
#' @param parallel Run in parallel? Default TRUE.
#' @param n_cores Number of cores to use when parallel = TRUE.
#'
#' @return Data frame of gaps and overlaps per station, date (and circadian group).
#'
#' @export
#'
get_daily_gaps <- function(input,
  start_time = NULL,
  stop_time = NULL,
  tz = NULL,
  station_order,
  parallel = TRUE,
  n_cores = parallel::detectCores() - 1) {

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
      res[[ii]] <- gaps_one_unit(input, units$date[ii],
                                 if (circ) units$circadian[ii] else NULL,
                                 station_order, df_locs)
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
  df.save <- df.save[, -which(names(df.save) == "aux")]

  # NAs are modelled range days with no correction - add flag "2"
  # (type-safe: works for factor, character or numeric flags)
  fill_flag <- function(x, value = 2) {
    if (is.factor(x)) {
      x <- factor(x, levels = union(levels(x), as.character(value)))
      x[is.na(x)] <- as.character(value)
    } else if (is.character(x)) {
      x[is.na(x)] <- as.character(value)
    } else {
      x[is.na(x)] <- value
    }
    x
  }
  df.save$cor_flag   <- fill_flag(df.save$cor_flag)
  df.save$range_flag <- fill_flag(df.save$range_flag)
  names(df.save)[1] <- "station_name"

  # Put circadian right after date
  if (circ) {
    df.save <- df.save[, c("station_name", "date", "circadian",
                           setdiff(names(df.save), c("station_name", "date", "circadian")))]
  }
  df.save
}


#' @title Summary of gap performance metrics
#'
#' @description This function calculates the summary statistics of gaps for all stations of interest.
#' If the input contains a "circadian" column, statistics are calculated and plotted per circadian group.
#'
#' @param input File exported from get_daily_gaps()
#' @param start_time Initial date and time of interest in Y-m-d format. Default is entire data available.
#' @param stop_time Final date and time of interest in Y-m-d format. Default is entire data available.
#' @param tz When start_time and stop_time are provided, their respective timezone.
#' @param station_order The names of the stations with expected overlapping ranges, from first to last.
#' @param range_type Range flag to select: 1 = Modelled range (default), 2 = Minimum estimated range
#' @param cor_type Correction flag to select: 1 = ML corrected range (default), 2 = Missing diagnostic data
#'
#' @return A list with a data frame of gap metrics (gaps) and a plot (gaps_plot)
#'
#' @export
#'
gaps_diag <- function(input,
  start_time = NULL,
  stop_time = NULL,
  tz = NULL,
  station_order,
  range_type = 1,
  cor_type = 1) {

  options(dplyr.summarise.inform = FALSE)

  if (!all(station_order %in% unique(input$station_name)))
    stop("Some station names were not found in the input dataset. Please check names provided.")
  input <- subset(input, station_name %in% station_order)
  input$station_name <- factor(input$station_name, levels = station_order)

  if (!is.null(start_time) && is.null(tz))
    stop("start_time provided but no timezone. Please provide tz for your study area.")
  if (!is.null(stop_time) && is.null(tz))
    stop("stop_time provided but no timezone. Please provide tz for your study area.")
  if (!is.null(start_time))
    input <- input[input$date >= as.Date(start_time, format = "%Y-%m-%d", tz = tz), ]
  if (!is.null(stop_time))
    input <- input[input$date <= as.Date(stop_time, format = "%Y-%m-%d", tz = tz), ]

  circ <- "circadian" %in% names(input)
  grp  <- if (circ) c("station_name", "circadian") else "station_name"

  # % of days with a gap, out of days where the gap could be calculated
  pct_days <- function(g) {
    n <- sum(!is.na(g))
    if (n == 0) NA_real_ else round(sum(g > 0, na.rm = TRUE) / n * 100, 1)
  }

  gaps_filt <- input %>%
    dplyr::filter(range_flag == range_type & cor_flag == cor_type) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::summarise(
      days_before = pct_days(gap_before),
      days_after  = pct_days(gap_after),
      mean_before = mean(gap_before, na.rm = TRUE),
      sd_before   = stats::sd(gap_before, na.rm = TRUE),
      mean_after  = mean(gap_after, na.rm = TRUE),
      sd_after    = stats::sd(gap_after, na.rm = TRUE),
      .groups = "drop")

  # Axis limits (guard against all-NA / zero)
  round_up <- function(x) {
    x <- suppressWarnings(max(x, na.rm = TRUE))
    if (!is.finite(x)) 50 else max(50, ceiling(round(x, 0) / 50) * 50)
  }
  max_neg <- -round_up(gaps_filt$mean_before + gaps_filt$sd_before)
  max_pos <-  round_up(gaps_filt$mean_after  + gaps_filt$sd_after)

  # Reverse "before" so it plots to the left of zero
  gaps_filt$mean_before <- -gaps_filt$mean_before

  cols <- paletteer::paletteer_d("ggsci::nrc_npg")

  plot_gaps <- ggplot2::ggplot() + ggplot2::theme_bw() +
    ggplot2::geom_pointrange(
      data = gaps_filt,
      ggplot2::aes(x = station_name, y = mean_before,
                   ymin = mean_before - sd_before, ymax = mean_before),
      colour = cols[1], na.rm = TRUE) +
    ggplot2::geom_pointrange(
      data = gaps_filt,
      ggplot2::aes(x = station_name, y = mean_after,
                   ymin = mean_after, ymax = mean_after + sd_after),
      colour = cols[6], na.rm = TRUE) +
    ggplot2::geom_text(
      data = gaps_filt,
      ggplot2::aes(x = station_name, y = (mean_before - sd_before) - 20, label = days_before),
      colour = cols[1], na.rm = TRUE) +
    ggplot2::geom_text(
      data = gaps_filt,
      ggplot2::aes(x = station_name, y = (mean_after + sd_after) + 20, label = days_after),
      colour = cols[6], na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = 0, lty = "dashed") +
    ggplot2::scale_y_continuous(
      breaks = seq(max_neg, max_pos, 50),
      labels = abs,
      limits = c(max_neg - 30, max_pos + 30),
      sec.axis = ggplot2::sec_axis(~ . * 1, name = "Position",
        breaks = c(max_neg / 2, max_pos / 2), labels = c("Before", "After"))) +
    ggplot2::labs(y = "Gap size (m)", x = "Station Name") +
    ggplot2::coord_flip()

  if (circ) plot_gaps <- plot_gaps + ggplot2::facet_wrap(~circadian)

  list(gaps = gaps_filt, gaps_plot = plot_gaps)
}


#' @title Plot potential gaps in the sequence of stations
#'
#' @description This function plots the range and gap size in the sequence of stations for a particular day of interest.
#' If the ranges contain a "circadian" column, both panels are faceted by circadian group.
#'
#' @param ranges File exported from get_daily_ranges()
#' @param gaps File exported from get_daily_gaps()
#' @param date Date of interest to plot
#' @param station_order The names of the stations with expected overlapping ranges, from first to last.
#'
#' @return Plot of station ranges, and gap sizes for that day.
#'
#' @importFrom patchwork wrap_plots
#' @importFrom magrittr %>%
#'
#' @export
#'
plot_gaps <- function(ranges,
  gaps,
  date,
  station_order) {

  options(dplyr.summarise.inform = FALSE)
  ranges <- ranges$ranges
  ranges <- subset(ranges, station_name %in% station_order)
  if (nrow(ranges) == 0)
    stop("station_order stations not present in the data. Please check ranges file.")
  ranges$station_name <- factor(ranges$station_name, levels = station_order)
  ranges <- as.data.frame(ranges)

  # circadian?
  circ <- "circadian" %in% names(ranges)
  if (circ && !"circadian" %in% names(gaps))
    stop("'circadian' found in ranges but not in gaps. Please re-run get_daily_gaps() on circadian ranges.")
  # get_daily_gaps() names the first column station_name; accept either
  if (!"station" %in% names(gaps) && "station_name" %in% names(gaps))
    gaps$station <- gaps$station_name

  date <- as.Date(date, tz = "UTC")

  # Circadian levels from the whole dataset, so every facet always appears
  if (circ) {
    circ_levels <- if (is.factor(ranges$circadian)) {
      levels(droplevels(ranges$circadian))
    } else {
      sort(unique(as.character(ranges$circadian)))
    }
  }

  # Station locations (one row per station)
  df_locs <- ranges %>%
    dplyr::group_by(station_name) %>%
    dplyr::summarise(x = lon[1], y = lat[1], .groups = "drop") %>%
    as.data.frame()
  n_st <- nrow(df_locs)

  ranges <- ranges[which(ranges$date == date), ]

  # Replicate stations for each circadian group, then match ranges on station (+ circadian)
  if (circ) {
    df_locs <- df_locs[rep(seq_len(n_st), times = length(circ_levels)), ]
    df_locs$circadian <- factor(rep(circ_levels, each = n_st), levels = circ_levels)
    key_loc <- paste(df_locs$station_name, df_locs$circadian, sep = "_")
    key_rng <- paste(ranges$station_name, as.character(ranges$circadian), sep = "_")
  } else {
    key_loc <- as.character(df_locs$station_name)
    key_rng <- as.character(ranges$station_name)
  }
  m <- match(key_loc, key_rng)
  df_locs$range_flag <- ranges$range_flag[m]
  df_locs$cor_flag   <- ranges$cor_flag[m]
  df_locs$range      <- ranges$range[m]
  df_locs$cor_range  <- ranges$cor_range[m]

  # Radius drawn on the map = radius used in the gap calculation
  df_locs$draw_range <- pick_radius(df_locs$range, df_locs$cor_range)
  df_locs$draw_range[is.na(df_locs$draw_range)] <- 0

  # Gap data
  df_gaps <- gaps[which(gaps$date == date), ]
  df_gaps$station <- factor(df_gaps$station, levels = station_order)
  if (circ)
    df_gaps$circadian <- factor(as.character(df_gaps$circadian), levels = circ_levels)

  # Zero-fill "not calculated" gaps so the plot works, and flag them (nc).
  # First/last position logic is applied within each date x circadian group.
  fix_gaps <- function(g) {
    g <- g[order(g$station), ]
    n <- nrow(g)
    g$nc <- FALSE
    if (n > 0) {
      nc_b <- is.na(g$gap_before); nc_b[1] <- FALSE   # first station has no "before"
      nc_a <- is.na(g$gap_after);  nc_a[n] <- FALSE   # last station has no "after"
      g$nc <- nc_b | nc_a
      g$gap_before[nc_b] <- 0
      g$gap_after[nc_a]  <- 0
    }
    g
  }
  groups <- if (circ) split(df_gaps, df_gaps$circadian, drop = TRUE) else list(df_gaps)
  df_gaps <- do.call(rbind, lapply(groups, fix_gaps))
  rownames(df_gaps) <- NULL

  # Axis range shared across facets; always at least +/-100 m so both sides show
  safe_max <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) 0 else max(x)
  }
  max_before <- min(-(ceiling(round(safe_max(df_gaps$gap_before), 0) / 100) * 100), -100)
  max_after  <- max(  ceiling(round(safe_max(df_gaps$gap_after),  0) / 100) * 100,   100)

  # Remaining NAs (first station before / last station after): avoid plot messages
  df_gaps$gap_before[is.na(df_gaps$gap_before)] <- 0
  df_gaps$gap_after[is.na(df_gaps$gap_after)]   <- 0

  # Convert to points and buffers (metres)
  pts   <- sf::st_as_sf(df_locs, coords = c("x", "y"), crs = 4326)
  pts_m <- sf::st_transform(pts, 3857)
  buffs <- sf::st_buffer(pts_m, dist = df_locs$draw_range)

  # Receiver location boundaries
  xlim <- c(min(df_locs$x) - 0.009, max(df_locs$x) + 0.009)
  ylim <- c(min(df_locs$y) - 0.009, max(df_locs$y) + 0.009)
  plot_lim <- sf::st_as_sf(data.frame(x = xlim, y = ylim), coords = c("x", "y"), crs = 4326)
  plot_lim <- as.data.frame(sf::st_coordinates(sf::st_transform(plot_lim, 3857)))

  # Station labels
  df_label   <- df_locs %>% dplyr::mutate(x = x + 0.0025, y = y + 0.002)
  plot_lab_m <- sf::st_transform(sf::st_as_sf(df_label, coords = c("x", "y"), crs = 4326), 3857)

  cols <- paletteer::paletteer_d("colorBlindness::SteppedSequential5Steps")[c(11, 18)]

  # Map
  plot1 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = buffs, ggplot2::aes(fill = range_flag, linetype = cor_flag),
      alpha = 0.3, linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::geom_sf(data = pts_m, color = "black", size = 2) +
    ggplot2::geom_sf_text(data = plot_lab_m, ggplot2::aes(label = station_name),
      colour = "black", check_overlap = TRUE) +
    ggplot2::coord_sf(xlim = plot_lim$X, ylim = plot_lim$Y) +
    ggplot2::theme_minimal() +
    ggspatial::annotation_scale(
      location = "bl", width_hint = 0.2, text_cex = 0.8, line_width = 1,
      height = ggplot2::unit(0.15, "cm"), unit_category = "metric") +
    ggplot2::labs(title = paste("Date:", date), x = "", y = "") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1),
      linetype = ggplot2::guide_legend(order = 2))

  # Gap plot
  plot2 <- ggplot2::ggplot() + ggplot2::theme_bw() +
    ggplot2::coord_cartesian(xlim = c(max_before, max_after)) +
    ggplot2::geom_pointrange(data = df_gaps, position = ggplot2::position_dodge(width = 0.1),
      ggplot2::aes(x = 0, xmin = 0 - gap_before, xmax = 0, y = station,
                   colour = range_flag, linetype = cor_flag),
      size = 0.3, linewidth = 1) +
    ggplot2::geom_pointrange(data = df_gaps, position = ggplot2::position_dodge(width = 0.1),
      ggplot2::aes(x = 0, xmin = 0 + gap_after, xmax = 0, y = station,
                   colour = range_flag, linetype = cor_flag),
      size = 0.3, linewidth = 1) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::scale_x_continuous(
      breaks = seq(max_before, max_after, 100),
      labels = abs,
      sec.axis = ggplot2::sec_axis(~ . * 1, name = "Position",
        breaks = c(-100, 100), labels = c("Before", "After"))) +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1),
      linetype = ggplot2::guide_legend(order = 2)) +
    ggplot2::labs(y = "Station name", x = "Gap size (m)")

  # Mark stations whose gap could not be calculated (instead of looking like a 0 m gap)
  if (any(df_gaps$nc)) {
    plot2 <- plot2 +
      ggplot2::geom_point(data = df_gaps[df_gaps$nc, ],
        ggplot2::aes(x = 0, y = station),
        shape = 4, size = 3, colour = "red", inherit.aes = FALSE) +
      ggplot2::labs(caption = "Red cross: gap could not be calculated")
  }

  # Circadian facets
  if (circ) {
    plot1 <- plot1 + ggplot2::facet_wrap(~circadian, nrow = 2)
    plot2 <- plot2 + ggplot2::facet_wrap(~circadian, nrow = 2)
  }

  (plot1 | plot2) + patchwork::plot_layout(widths = c(4, 3))
}




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
  n_cores = parallel::detectCores() - 1) {

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