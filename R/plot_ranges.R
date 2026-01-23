#' @title Plot daily ranges as timeseries
#'
#' @description This function will plot the overall range distribution, and daily timeseries of range, 
#' detection efficiency, and detection probability curves, for each station.
#'
#' @param input File exported from get_daily_ranges()
#' @param type Which plot to make, options include: timeseries, total, or both.
#' @param station_order The names of the stations with expected overlapping ranges, from first to last.
#' @param linewidth Linewidth of lines for timeseries plot.
#' @param start_time Initial date and time of interest in Y-m-d H:M:S format. Default is entire data available.
#' @param stop_time Final date and time of interest in Y-m-d H:M:S format. Default is entire data available.
#' @param tz When start_time and stop_time are provided, their respective timezone.
#' 
#' @return A plot of the daily range variations.
#' 
#' @importFrom patchwork wrap_plots
#' 
#' @export
#' 
plot_ranges <- function(input,
  type = c("timeseries", "total", "both"),
  linewidth = 0.15,
  station_order,
  start_time = NULL,
  stop_time = NULL,
  tz) {
  
  options(dplyr.summarise.inform = FALSE)

  # Checks
  if(!all(station_order %in% unique(input$ranges$station_name)))
    stop("Some station names were not found in the input dataset. Please check names provided.")

  if (!is.null(start_time)) {
    input$ranges <- input$ranges[input$ranges$date >= as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S", tz = tz)]
    input$curves <- input$curves[input$curves$date >= as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S", tz = tz)]
  }
  if (!is.null(stop_time)) {
    input$ranges <- input$ranges[input$ranges$date <= as.POSIXct(stop_time, format = "%Y-%m-%d %H:%M:%S", tz = tz)]
    input$curves <- input$curves[input$curves$date <= as.POSIXct(stop_time, format = "%Y-%m-%d %H:%M:%S", tz = tz)]
  }

  # Sort receiver order
  input$ranges <- subset(input$ranges, station_name %in% station_order)
  input$curves <- subset(input$curves, station_name %in% station_order)

  input$ranges$station_name <- factor(input$ranges$station_name,
    levels = station_order)
  input$curves$station_name <- factor(input$curves$station_name,
    levels = station_order)


  # Help to find stations where no range could be calulated
  station_missing <- input$ranges %>%
    dplyr::group_by(station_name) %>%
    dplyr::summarise(min_range = if (all(is.na(range))) {
          NA_real_
        } else {
          round(min(range, na.rm = TRUE), 2)
        }) %>%
    dplyr::filter(is.na(min_range))
  if (nrow(station_missing)) {
    message(paste("No daily ranges available for:", paste(unique(station_missing$station_name), collapse = ", ")))
    input$ranges <- subset(input$ranges, !(station_name %in% station_missing$station_name))
    input$curves <- subset(input$curves, !(station_name %in% station_missing$station_name))
  }

  # Plot
  if (type %in% c("total", "both")) {
    plot_total <- ggplot2::ggplot() + ggplot2::theme_bw() +
    ggplot2::geom_boxplot(data = subset(input$ranges, !is.na(range)), 
      ggplot2::aes(x = station_name, group = station_name, y = range),
      colour = paletteer::paletteer_d("ggsci::default_nejm")[4]) +
    ggplot2::labs(x = "Station name", y = "Range (m)") +
    ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 11),
        axis.text.y = ggplot2::element_text(size = 11))
  }

  if (type %in% c("timeseries", "both")) {
    plot1 <- ggplot2::ggplot() + ggplot2::theme_bw() +
      ggplot2::geom_line(data = subset(input$ranges, !is.na(range)),   
            ggplot2::aes(x = date, group = period, y = range), colour = paletteer::paletteer_d("ggsci::default_nejm")[4], linewidth = linewidth) +
      ggplot2::scale_x_date(date_labels = "%b %Y") +
        ggplot2::facet_wrap(~station_name, nrow = length(station_order)) +
        ggplot2::labs(x = "Day", y = "Range (m)") +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 9),
          axis.text.y = ggplot2::element_text(size = 7))
    plot2 <- ggplot2::ggplot() + ggplot2::theme_bw() +
      ggplot2::geom_line(data = input$ranges,   
            ggplot2::aes(x = date, group = period, y = daily_efficiency), colour = paletteer::paletteer_d("ggsci::default_nejm")[1], linewidth = linewidth) +
      ggplot2::scale_x_date(date_labels = "%b %Y") +
        ggplot2::facet_wrap(~station_name, nrow = length(station_order)) +
        ggplot2::labs(x = "Day", y = "Mean efficiency (%)") +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 9),
          axis.text.y = ggplot2::element_text(size = 7))
    plot3 <- ggplot2::ggplot(
        subset(input$curves, type == "curve"),
        ggplot2::aes(x = x, y = y)
      ) +
      ggplot2::geom_line(ggplot2::aes(group = date), colour = "lightgray", linewidth = 1) +
      ggplot2::geom_smooth(
        method = "glm",
        method.args = list(family = stats::binomial(link = "logit")),
        formula = y ~ x,
        se = FALSE,
        colour = "blue",
        linewidth = 1,
        inherit.aes = TRUE,
        ggplot2::aes(group = 1)  # one curve per facet
      ) +
      ggplot2::geom_hline(yintercept = 0.5, lty = 'dashed') +
      ggplot2::coord_cartesian(ylim = c(0, 1)) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(~station_name, nrow = length(station_order)) +
      ggplot2::labs(y = "Detection probability", x = "Distance (m)") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 9),
        axis.text.y = ggplot2::element_text(size = 7)
      )
  }

  # Export options
  if (type == "total")
    plot_save <- plot_total
  if (type == "timeseries")
    plot_save <- plot1 | plot2 | plot3
  if (type == "both")
    plot_save <- (plot_total / (plot1 | plot2 | plot3)) + patchwork::plot_layout(heights = c(1, 5))

  return(suppressWarnings(print(plot_save)))
}
