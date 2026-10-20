#' @title Plot potential gaps in the sequence of stations 
#'
#' @description This function plots the range and gap size in the sequence of stations for a particular day of interest.
#'
#' @param ranges File exported from get_daily_ranges()
#' @param gaps File exported from daily_gaps()
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
  # Rename station name to facilitate indexing
  ranges <- ranges$ranges  
  ranges <- subset(ranges, station_name %in% station_order)
  if (nrow(ranges) == 0)
    stop("station_order stations not present in the data. Please check ranges file.")
  ranges$station_name <- factor(ranges$station_name, levels = station_order)
  
  # Circadian:
  if ("circadian" %in% names(ranges)) {
    circ <- TRUE
  } else {
    circ <- FALSE
  }

  # Get all station points
  date <- as.Date(date, tz = "UTC")
  ranges <- as.data.frame(ranges)
  if (circ) {
    df_locs <- ranges %>%
      dplyr::group_by(station_name, range_flag, cor_flag, circadian) %>%
      dplyr::summarise(x = lon[1],
        y = lat[1],
        min_range = min(cor_range)) %>%
      dplyr::filter(range_flag %in% c(0, 2) &
        cor_flag %in% c(0, 2))

     ranges <- ranges[which(ranges$date == date),]  

    df_locs$range_flag2 <- ranges$range_flag[match(df_locs$station_name, ranges$station_name)]
    df_locs$cor_flag2 <- ranges$cor_flag[match(df_locs$station_name, ranges$station_name)]
    df_locs$cor_range <- ranges$cor_range[match(df_locs$station_name, ranges$station_name)]
    df_locs$range <- ranges$range[match(df_locs$station_name, ranges$station_name)]
    # Missing station: non-corrected minimum range
    index <- which(is.na(df_locs$cor_range))
    if (length(index) > 0) {
      df_locs$cor_range[index] <- df_locs$min_range[index]
      df_locs$range_flag2[index] <- df_locs$range_flag[index]
      df_locs$cor_flag2[index] <- df_locs$cor_flag[index]
    }
    df_locs$range_flag <- df_locs$range_flag2
    df_locs$cor_flag <- df_locs$cor_flag2

    # Extract gap data
    df_gaps <- gaps[which(gaps$date == date),]
    df_gaps$station <- factor(df_gaps$station,
      levels = station_order)
  } else {
    df_locs <- ranges %>%
      dplyr::group_by(station_name, range_flag, cor_flag) %>%
      dplyr::summarise(x = lon[1],
        y = lat[1],
        min_range = min(cor_range)) %>%
      dplyr::filter(range_flag %in% c(0, 2) &
        cor_flag %in% c(0, 2))

    ranges <- ranges[which(ranges$date == date),]  

    df_locs$range_flag2 <- ranges$range_flag[match(df_locs$station_name, ranges$station_name)]
    df_locs$cor_flag2 <- ranges$cor_flag[match(df_locs$station_name, ranges$station_name)]
    df_locs$cor_range <- ranges$cor_range[match(df_locs$station_name, ranges$station_name)]
    df_locs$range <- ranges$range[match(df_locs$station_name, ranges$station_name)]
    # Missing station: non-corrected minimum range
    index <- which(is.na(df_locs$cor_range))
    if (length(index) > 0) {
      df_locs$cor_range[index] <- df_locs$min_range[index]
      df_locs$range_flag2[index] <- df_locs$range_flag[index]
      df_locs$cor_flag2[index] <- df_locs$cor_flag[index]
    }
    df_locs$range_flag <- df_locs$range_flag2
    df_locs$cor_flag <- df_locs$cor_flag2

    # Extract gap data
    df_gaps <- gaps[which(gaps$date == date),]
    df_gaps$station <- factor(df_gaps$station,
      levels = station_order)
  }


  # # Extract range data
  # date.aux <- date
  # aux.data <- subset(ranges, date == date.aux)
  # aux.data$station_name <- factor(aux.data$station_name,
  #   levels = station_order)
  # df_locs$range <- aux.data$cor_range[match(df_locs$station_name, aux.data$station_name)]
  # index <- which(is.na(df_locs$range))
  # if (length(index) > 0)
  #   df_locs$range[index] <- 0
  
  # Convert to points
  pts <- sf::st_as_sf(df_locs, coords = c("x", "y"), crs = 4326)
  pts_m <- sf::st_transform(pts, 3857)
  # Create buffers in meters
  buffs <- sf::st_buffer(pts_m, dist = df_locs$cor_range)
  # Receiver location boundaries
  xlim <- c(min(df_locs$x) - 0.009, max(df_locs$x) + 0.009)
  ylim <- c(min(df_locs$y) - 0.009, max(df_locs$y) + 0.009)
  plot_lim <- sf::st_as_sf(data.frame(x = xlim, y = ylim), coords = c("x", "y"), crs = 4326)
  plot_m <- sf::st_transform(plot_lim, 3857)
  plot_lim <- as.data.frame(sf::st_coordinates(plot_m))
  # Label station locations
  df_label <- df_locs %>%
    dplyr::mutate(x = x + 0.0025, y = y + 0.002)
  plot_lab <- sf::st_as_sf(df_label, coords = c("x", "y"), crs = 4326)
  plot_lab_m <- sf::st_transform(plot_lab, 3857)
  # Gap size boundaries
  if (length(unique(df_gaps$gap_before)) == 1) {
      if (is.na(unique(df_gaps$gap_before))) {
      gap1 <- 200
    }
  } else {
    gap1 <- round(max(df_gaps$gap_before, na.rm = TRUE), 0)  
  }
  if (length(unique(df_gaps$gap_after)) == 1) {
      if (is.na(unique(df_gaps$gap_after))) {
      gap2 <- 200
    }
  } else {
    gap2 <- round(max(df_gaps$gap_after, na.rm = TRUE), 0)  
  }
  gap_max <- max(gap1, gap2)
  if (gap_max < 200)
    gap_max <- 200
  gap_labs <- seq(0, gap_max, 200)
  # Add variable for gaps that couldn't be calculated
  # Account for first and last points
  df_gaps$left_type <- "Actual gap"
  index <- which(is.na(df_gaps$gap_before))[-1]
  if (length(index) > 0)
    df_gaps$left_type[index] <- "Gap not calculated"
  df_gaps$right_type <- "Actual gap"
  index <- which(is.na(df_gaps$gap_after))
  index <- index[-length(index)]
  if (length(index) > 0)
    df_gaps$right_type[index] <- "Gap not calculated" 
  # Colour scale
  aux_gaps <- df_gaps[-c(1,nrow(df_gaps)),]
  if (length(unique(aux_gaps$right_type)) == 1) {
    if (unique(aux_gaps$right_type) == "Gap not calculated")
      gap_colours <- "red"
    if (unique(aux_gaps$right_type) == "Actual gap")
      gap_colours <- "dodgerblue"
  } else {
    aux_gaps$right_type <- factor(aux_gaps$right_type, levels = c("Actual gap", "Gap not calculated"))
    gap_colours <- c("dodgerblue", "red")
  }
  # Add zeros so plot works
  index <- which(is.na(df_gaps$gap_before))
  index <- index[-1]
  if (length(index) > 0)
    df_gaps$gap_before[index] <- 0
  index <- which(is.na(df_gaps$gap_after))
  index <- index[-length(index)]
  if (length(index) > 0)
    df_gaps$gap_after[index] <- 0

  # Range values
  max_before <- (ceiling(round(max(df_gaps$gap_before, na.rm = TRUE), 0) / 100) * 100) * -1
  max_after <- (ceiling(round(max(df_gaps$gap_after, na.rm = TRUE), 0) / 100) * 100)

  # Add zeros: avoid messages when plotting
  df_gaps$gap_before[is.na(df_gaps$gap_before)] <- 0
  df_gaps$gap_after[is.na(df_gaps$gap_after)] <- 0

  # Plot
  # legend_unused: "drop" = hide levels not in the data
  #                "keep" = show all levels (previous behaviour)
  #                "label" = show all levels, but mark absent ones "(none)"
  legend_unused <- "drop"

  flag_levels <- c("0", "1", "2")
  flag_cols <- stats::setNames(
    paletteer::paletteer_d("colorBlindness::SteppedSequential5Steps")[c(2, 11, 18)],
    flag_levels
  )
  flag_ltys <- stats::setNames(c("dotted", "solid", "dashed"), flag_levels)

  buffs$range_flag <- factor(buffs$range_flag, levels = flag_levels)
  buffs$cor_flag   <- factor(buffs$cor_flag,   levels = flag_levels)

  # Build legend label helper
  make_labels <- function(present) {
    if (legend_unused == "label") {
      function(x) ifelse(x %in% present, x, paste0(x, " (none)"))
    } else {
      ggplot2::waiver()
    }
  }

  drop_flag <- legend_unused == "drop"

  plot1 <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = buffs,
      ggplot2::aes(fill = range_flag, linetype = cor_flag),
      alpha = 0.3, linewidth = 0.5
    ) +
    ggplot2::scale_fill_manual(
      values = flag_cols, drop = drop_flag, name = "Range flag",
      labels = make_labels(unique(as.character(buffs$range_flag)))
    ) +
    ggplot2::scale_linetype_manual(
      values = flag_ltys, drop = drop_flag, name = "Cor flag",
      labels = make_labels(unique(as.character(buffs$cor_flag)))
    ) +
    ggplot2::geom_sf(data = pts_m, color = "black", size = 0.5) +
    ggplot2::geom_sf_text(
      data = plot_lab_m, ggplot2::aes(label = station_name),
      colour = "black", check_overlap = TRUE
    ) +
    ggplot2::coord_sf(xlim = plot_lim$X, ylim = plot_lim$Y) +
    ggplot2::theme_minimal() +
    ggspatial::annotation_scale(
      location = "bl", width_hint = 0.2, text_cex = 0.8,
      line_width = 1, height = ggplot2::unit(0.15, "cm"),
      unit_category = "metric"
    ) +
    ggplot2::labs(title = paste("Date:", date), x = "", y = "") +
    # ggplot2::guides(
    #   fill = ggplot2::guide_legend(order = 1),
    #   linetype = ggplot2::guide_legend(
    #     order = 2,
    #     override.aes = list(fill = "white", alpha = 1, linewidth = 0.8)
    #   )
    # )
    ggplot2::theme(legend.position = 'none')

  if (circ) {
    plot1 <- plot1 + ggplot2::facet_wrap(~circadian, nrow = 2)
  }

  # ---- Shared setup (define once, above plot1 and plot2) ----
  legend_unused <- "drop"   # "drop", "keep" or "label"
  share_legend  <- TRUE     # TRUE = only plot1 shows legends (plot2 uses the same colours/linetypes)

  flag_levels <- c("0", "1", "2")
  flag_cols <- stats::setNames(
    paletteer::paletteer_d("colorBlindness::SteppedSequential5Steps")[c(2, 11, 18)],
    flag_levels
  )
  flag_ltys <- stats::setNames(c("dotted", "solid", "dashed"), flag_levels)

  buffs$range_flag    <- factor(buffs$range_flag,    levels = flag_levels)
  buffs$cor_flag      <- factor(buffs$cor_flag,      levels = flag_levels)
  df_gaps$range_flag  <- factor(df_gaps$range_flag,  levels = flag_levels)
  df_gaps$cor_flag    <- factor(df_gaps$cor_flag,    levels = flag_levels)

  drop_flag <- legend_unused == "drop"

  make_labels <- function(present) {
    if (legend_unused == "label") {
      function(x) ifelse(x %in% present, x, paste0(x, " (none)"))
    } else {
      ggplot2::waiver()
    }
  }

  # ---- plot2 ----
  plot2 <- ggplot2::ggplot() + ggplot2::theme_bw() +
    ggplot2::coord_cartesian(xlim = c(max_before, max_after)) +
    ggplot2::geom_pointrange(
      data = df_gaps, position = ggplot2::position_dodge(width = 0.1),
      ggplot2::aes(x = 0, xmin = 0 - gap_before, xmax = 0, y = station,
                   colour = range_flag, linetype = cor_flag),
      size = 0.3, linewidth = 1
    ) +
    ggplot2::geom_pointrange(
      data = df_gaps, position = ggplot2::position_dodge(width = 0.1),
      ggplot2::aes(x = 0, xmin = 0 + gap_after, xmax = 0, y = station,
                   colour = range_flag, linetype = cor_flag),
      size = 0.3, linewidth = 1
    ) +
    ggplot2::scale_colour_manual(
      values = flag_cols, drop = drop_flag, name = "Range flag",
      labels = make_labels(unique(as.character(df_gaps$range_flag)))
    ) +
    ggplot2::scale_linetype_manual(
      values = flag_ltys, drop = drop_flag, name = "Cor flag",
      labels = make_labels(unique(as.character(df_gaps$cor_flag)))
    ) +
    # ggplot2::guides(
    #   colour   = ggplot2::guide_legend(order = 1),
    #   linetype = ggplot2::guide_legend(order = 2)
    # ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        order = 1,
        override.aes = list(linetype = "blank")   # hides the line, keeps the coloured point
      ),
      linetype = ggplot2::guide_legend(
        order = 2,
        override.aes = list(shape = NA, size = 0, colour = "grey30")  # hides the point, keeps the line
      )
    ) +
    ggplot2::labs(y = "Station name", x = "Gap size (m)")

  # if (share_legend) {
  #   plot2 <- plot2 + ggplot2::theme(legend.position = "none")
  # }

  if (circ) {
    plot2 <- plot2 + ggplot2::facet_wrap(~circadian, nrow = 2)
  }

  # ---- Fix axes ----
  if (max_before == 0 && max_after == 0) {
    plot2 <- plot2 +
      ggplot2::scale_x_continuous(
        breaks = c(-0.025, 0, 0.025),
        labels = c("", "0", ""),
        sec.axis = ggplot2::sec_axis(~ ., name = "Position",
          breaks = c(-0.025, 0.025), labels = c("Before", "After"))
      )
  } else {
    plot2 <- plot2 +
      ggplot2::scale_x_continuous(
        breaks = seq(max_before, max_after, 100),
        labels = function(x) abs(x),
        sec.axis = ggplot2::sec_axis(~ ., name = "Position",
          breaks = c(-100, 100), labels = c("Before", "After"))
      )
  }
  if (max_after > 500) {
    plot2 <- suppressMessages(plot2 +
      ggplot2::scale_x_continuous(
        breaks = seq(max_before, max_after, 200),
        labels = function(x) abs(x),
        sec.axis = ggplot2::sec_axis(~ ., name = "Position",
          breaks = c(-100, 100), labels = c("Before", "After"))
      ))
  }

  plot_save <- (plot1 | plot2) + patchwork::plot_layout(widths = c(4, 3), guides = "collect")

  return(plot_save) 
}

