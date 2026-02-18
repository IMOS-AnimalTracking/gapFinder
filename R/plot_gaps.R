#' @title Plot potential gaps in the sequence of stations 
#'
#' @description This function plots the range and gap size in the sequence of stations for a particular day of interest.
#'
#' @param ranges File exported from get_daily_ranges()
#' @param gaps File exported from daily_gaps()
#' @param date Date of interest to plot
#' @param station_name Column in the metadata file identifying the station names.
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
  station_name, 
  station_order) {

  options(dplyr.summarise.inform = FALSE)
  # Rename station name to facilitate indexing
  ranges <- ranges$ranges  
  names(ranges)[which(names(ranges) == station_name)] <- "station_name"
  ranges <- subset(ranges, station_name %in% station_order)
  if (nrow(ranges) == 0)
    stop("station_order stations not present in the data. Please check ranges file.")
  ranges$station_name <- factor(ranges$station_name, levels = station_order)
  # Get all station points
  date <- as.Date(date, tz = "UTC")
  df_locs <- ranges %>%
    dplyr::group_by(station_name) %>%
    dplyr::summarise(x = mean(lon),
      y = mean(lat))
  # Extract range data
  date.aux <- date
  aux.data <- subset(ranges, date == date.aux)
  aux.data$station_name <- factor(aux.data$station_name,
    levels = station_order)
  df_locs$range <- aux.data$range[match(df_locs$station_name, aux.data$station_name)]
  index <- which(is.na(df_locs$range))
  if (length(index) > 0)
    df_locs$range[index] <- 0
  # Extract gap data
  df_gaps <- gaps[which(gaps$date == date),]
  df_gaps$station <- factor(df_gaps$station,
    levels = station_order)
  # Convert to points
  pts <- sf::st_as_sf(df_locs, coords = c("x", "y"), crs = 4326)
  pts_m <- sf::st_transform(pts, 3857)
  # Create buffers in meters
  buffs <- sf::st_buffer(pts_m, dist = df_locs$range)
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
  if (length(unique(df_gaps$gap_left)) == 1) {
      if (is.na(unique(df_gaps$gap_left))) {
      gap1 <- 200
    }
  } else {
    gap1 <- round(max(df_gaps$gap_left, na.rm = TRUE), 0)  
  }
  if (length(unique(df_gaps$gap_right)) == 1) {
      if (is.na(unique(df_gaps$gap_right))) {
      gap2 <- 200
    }
  } else {
    gap2 <- round(max(df_gaps$gap_right, na.rm = TRUE), 0)  
  }
  gap_max <- max(gap1, gap2)
  if (gap_max < 200)
    gap_max <- 200
  gap_labs <- seq(0, gap_max, 200)
  # Add variable for gaps that couldn't be calculated
  # Account for first and last points
  df_gaps$left_type <- "Actual gap"
  index <- which(is.na(df_gaps$gap_left))[-1]
  if (length(index) > 0)
    df_gaps$left_type[index] <- "Gap not calculated"
  df_gaps$right_type <- "Actual gap"
  index <- which(is.na(df_gaps$gap_right))
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
  index <- which(is.na(df_gaps$gap_left))
  index <- index[-1]
  if (length(index) > 0)
    df_gaps$gap_left[index] <- 0
  index <- which(is.na(df_gaps$gap_right))
  index <- index[-length(index)]
  if (length(index) > 0)
    df_gaps$gap_right[index] <- 0
  
  # Plot
  plot1 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = buffs, fill = "skyblue", alpha = 0.3, color = "skyblue") +
    ggplot2::geom_sf(data = pts_m, color = "black", size = 2) +
    ggplot2::geom_sf_text(data = plot_lab_m, ggplot2::aes(label = station_name), colour = "black",
      check_overlap = TRUE) +
    ggplot2::coord_sf(xlim = plot_lim$X, ylim = plot_lim$Y) +
    ggplot2::theme_minimal() +
    ggspatial::annotation_scale(
      location = "bl",        # bottom left
      width_hint = 0.3,       # relative width of scale bar
      text_cex = 0.8,
      line_width = 1,
      height = ggplot2::unit(0.15, "cm")) +
    ggplot2::labs(title = paste("Date:", date), x = "", y = "")
  plot2 <- ggplot2::ggplot() + ggplot2::theme_bw() + 
    ggplot2::coord_cartesian(xlim = c(0, gap_max)) +
    ggplot2::geom_pointrange(data = df_gaps[2:nrow(df_gaps),], position = ggplot2::position_dodge(width = 0.1),
      ggplot2::aes(x = 0, xmin = 0 + gap_left, xmax = 0, y = station, colour = left_type),
      size = 0.1, linewidth = 1) +
    ggplot2::scale_colour_manual(values = gap_colours) +
    ggplot2::scale_x_continuous(breaks = gap_labs) +
    ggplot2::labs(y = "Station name", x = "Gap size (m)", colour = "", title = "West") 
  plot3 <- ggplot2::ggplot() + ggplot2::theme_bw() + 
    ggplot2::coord_cartesian(xlim = c(0, gap_max)) +
    ggplot2::geom_pointrange(data = df_gaps[1:(nrow(df_gaps)-1),], position = ggplot2::position_dodge(width = 0.1),
      ggplot2::aes(x = 0, xmin = 0 + gap_right, xmax = 0, y = station, colour = right_type),
      size = 0.1, linewidth = 1) +
    ggplot2::scale_colour_manual(values = gap_colours) +
    ggplot2::scale_x_continuous(breaks = gap_labs) +
    ggplot2::labs(y = "", x = "Gap size (m)", colour = "", title = "East") 
  plot2 <- (plot2 + plot3) + patchwork::plot_layout(guides = "collect")
  plot_save <- (plot1 | plot2) + patchwork::plot_layout(widths = c(4,3))
  return(plot_save) 
}

