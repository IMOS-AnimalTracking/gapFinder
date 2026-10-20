#' @title Summary of gap performance metrics
#'
#' @description This function calculates the summary statistics of range variation for all stations of interest. 
#'
#' @param input File exported from get_daily_gaps()
#' @param start_time Initial date and time of interest in Y-m-d format. Default is entire data available.
#' @param stop_time Final date and time of interest in Y-m-d format. Default is entire data available.
#' @param tz When start_time and stop_time are provided, their respective timezone.
#' @param station_order The names of the stations with expected overlapping ranges, from first to last.
#' @param range_type Range flag to select (default NULL is both): 1 = Modelled range (default), 2 = Minimum estimated range
#' @param cor_type Correction flag to select (default NULL is both): 1 = ML corrected range (default), 2 = Missing diagnostic data 
#' 
#' @return A dataframe containing the range performance metrics
#' 
#' @export
#' 
gaps_diag <- function(input, 
  start_time = NULL,
  stop_time = NULL,
  tz = NULL,
  station_order,
  range_type = NULL, 
  cor_type = NULL) {
  
  options(dplyr.summarise.inform = FALSE)

  if(!all(station_order %in% unique(input$station_name)))
    stop("Some station names were not found in the input dataset. Please check names provided.")
  input <- subset(input, station_name %in% station_order)
  input$station_name <- factor(input$station_name, levels = station_order)
  
  if (!is.null(start_time) &
      is.null(tz)) {
        stop("start_time provided but no timezone. Please provide tz for your study area.")
      } 
  if (!is.null(stop_time) &
      is.null(tz)) {
        stop("stop_time provided but no timezone. Please provide tz for your study area.")
      }  
  if (!is.null(start_time)) {
    input <- input[input$date >= as.Date(start_time, format = "%Y-%m-%d", tz = tz)]
  }
  if (!is.null(stop_time)) {
    input <- input[input$date <= as.Date(stop_time, format = "%Y-%m-%d", tz = tz)]
  }
  if (!is.null(range_type)) {
    input <- input[which(input$range_flag == range_type),]
  }
  if (!is.null(cor_type)) {
    input <- input[which(input$cor_flag == cor_type),]
  }


  # circadian? 
  if("circadian" %in% names(input)) {
    circ <- "TRUE" 
  } else {
    circ <- "FALSE"
  }

  dates <- length(unique(input$date)) # Total 
  if (circ) {
    gaps_filt <- input %>%
      # dplyr::filter(range_flag == 1 & cor_flag == 1) %>%
      dplyr::group_by(station_name, circadian) %>%
      dplyr::summarise(
          days_before = round(((length(which(gap_before > 0)) / dates) * 100), 2),
          days_after = round(((length(which(gap_after > 0)) / dates) * 100), 2),
          mean_before = mean(gap_before, na.rm = TRUE), 
          sd_before = stats::sd(gap_before, na.rm = TRUE),
          mean_after = mean(gap_after, na.rm = TRUE),
          sd_after = stats::sd(gap_after, na.rm = TRUE))

  } else {
    gaps_filt <- input %>%
      # dplyr::filter(range_flag == 1 & cor_flag == 1) %>%
      dplyr::group_by(station_name) %>%
      dplyr::summarise(
          days_before = round(((length(which(gap_before > 0)) / dates) * 100), 2),
          days_after = round(((length(which(gap_after > 0)) / dates) * 100), 2),
          mean_before = mean(gap_before, na.rm = TRUE), 
          sd_before = stats::sd(gap_before, na.rm = TRUE),
          mean_after = mean(gap_after, na.rm = TRUE),
          sd_after = stats::sd(gap_after, na.rm = TRUE))
  }
  
  # Find maximum gap values to be used for axis
  max_neg <- (ceiling(round(max(gaps_filt$mean_before + gaps_filt$sd_before, na.rm = TRUE), 0) / 50) * 50) * -1
  max_pos <- ceiling(round(max(gaps_filt$mean_after + gaps_filt$sd_after, na.rm = TRUE), 0) / 50) * 50
  if (max_neg > -50)
    max_neg <- -70
  if (max_pos < 50)
    max_pos <- 70

  # Reverse negative values  
  gaps_filt$mean_before <- gaps_filt$mean_before *-1
  
  # Plot summary gaps
  pal <- paletteer::paletteer_d("ggsci::nrc_npg")

  plot_gaps <- ggplot2::ggplot() + ggplot2::theme_bw() +
    ggplot2::geom_pointrange(
      data = gaps_filt,
      ggplot2::aes(x = station_name, y = mean_before,
                   ymin = mean_before - sd_before, ymax = mean_before),
      colour = pal[1], na.rm = TRUE
    ) +
    ggplot2::geom_pointrange(
      data = gaps_filt,
      ggplot2::aes(x = station_name, y = mean_after,
                   ymin = mean_after, ymax = mean_after + sd_after),
      colour = pal[6], na.rm = TRUE
    ) +
    ggplot2::geom_text(
      data = gaps_filt,
      ggplot2::aes(x = station_name, y = (mean_before - sd_before) - 80,
                   label = days_before),
      colour = pal[1], nudge_x = 0.3, na.rm = TRUE
    ) +
    ggplot2::geom_text(
      data = gaps_filt,
      ggplot2::aes(x = station_name, y = (mean_after + sd_after) + 80,
                   label = days_after),
      colour = pal[6], nudge_x = 0.3, na.rm = TRUE
    ) +
    ggplot2::geom_hline(yintercept = 0, lty = "dashed") +
    ggplot2::scale_y_continuous(
      breaks = seq(max_neg, max_pos, 100),
      labels = function(x) abs(x),
      sec.axis = ggplot2::sec_axis(~ ., name = "Position",
                                   breaks = c(-50, 50),
                                   labels = c("Before", "After"))
    ) +
    ggplot2::labs(y = "Gap size (m)", x = "Station Name") +
    ggplot2::coord_flip()

    if (circ) 
      plot_gaps <- plot_gaps +
        ggplot2::facet_wrap(~circadian)

    if (max_pos > 500) {
      plot_gaps <- suppressMessages(plot_gaps +
      ggplot2::scale_y_continuous(
        breaks = seq(max_neg, max_pos, 200),
        labels = function(x) abs(x),
        sec.axis = ggplot2::sec_axis(~ ., name = "Position",
          breaks = c(-200, 200), labels = c("Before", "After"))
      ))
    }

  # Transform negative values back to positive for export
  gaps_filt$mean_before <- gaps_filt$mean_before * -1


  return(list(gaps = gaps_filt, gaps_plot = plot_gaps))
}
