#' @title Plot daily ranges as timeseries
#'
#' @description This function will plot the overall range distribution, and daily timeseries of range, 
#' detection efficiency, and detection probability curves, for each station.
#'
#' @param input File exported from get_daily_ranges()
#' @param station_order The names of the stations with expected overlapping ranges, from first to last.
#' 
#' @return A plot of the daily range variations.
#' 
#' @importFrom patchwork wrap_plots
#' 
#' @export
#' 
plot_ranges <- function(input,
  station_order) {
  # Checks
  if(!all(station_order %in% unique(input$ranges$station_name)))
    stop("Some station names were not found in the input dataset. Please check names provided.")
  # Sort receiver order
  input$ranges <- subset(input$ranges, station_name %in% station_order)
  input$curves <- subset(input$curves, station_name %in% station_order)

  input$ranges$station_name <- factor(input$ranges$station_name,
    levels = station_order)
  input$curves$station_name <- factor(input$curves$station_name,
    levels = station_order)
  # Plot
  plot_total <- ggplot2::ggplot() + ggplot2::theme_bw() +
    ggplot2::geom_boxplot(data = input$ranges, 
      ggplot2::aes(x = station_name, group = station_name, y = range),
      colour = paletteer::paletteer_d("ggsci::default_nejm")[4]) +
    ggplot2::labs(x = "Station name", y = "Range (m)") +
    ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 11),
        axis.text.y = ggplot2::element_text(size = 11))
  plot1 <- ggplot2::ggplot() + ggplot2::theme_bw() +
    ggplot2::geom_line(data = input$ranges,   
          ggplot2::aes(x = date, group = period, y = range), colour = paletteer::paletteer_d("ggsci::default_nejm")[4], linewidth = 0.15) +
    ggplot2::scale_x_date(date_labels = "%b %Y") +
      ggplot2::facet_wrap(~station_name, nrow = length(station_order)) +
      ggplot2::labs(x = "Day", y = "Range (m)") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 9),
        axis.text.y = ggplot2::element_text(size = 7))
  plot2 <- ggplot2::ggplot() + ggplot2::theme_bw() +
    ggplot2::geom_line(data = input$ranges,   
          ggplot2::aes(x = date, group = period, y = daily_efficiency), colour = paletteer::paletteer_d("ggsci::default_nejm")[1], linewidth = 0.15) +
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
  plot_save <- (plot_total / (plot1 | plot2 | plot3)) + patchwork::plot_layout(heights = c(1, 5))
  return(suppressWarnings(print(plot_save)))
}
