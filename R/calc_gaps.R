#' @title Calculate spatial gap between stations
#'
#' @description Function used to calculate gaps between sequence of stations with expected overlapping range
#'
#' @param type Position of station in the sequence: "first", "middle", "last"
#' @param input Daily ranges for ONE date (and ONE circadian group, if used)
#' @param j Station position
#' @param date Date of interest
#' @param station_order The names of the stations with expected overlapping ranges, from first to last.
#'
#' @return Data frame of station spatial overlap and gaps
#'
#' @export
#'
calc_gaps <- function(type,
  input,
  j,
  date,
  station_order) {

  get_pair <- function(a, b) {
    s1 <- subset(input, station_name == station_order[a])
    s2 <- subset(input, station_name == station_order[b])
    pair_gap(s1, s2)
  }

  none   <- list(gap = NA, overlap = NA)
  before <- if (type %in% c("middle", "last"))  get_pair(j - 1, j) else none
  after  <- if (type %in% c("first", "middle")) get_pair(j, j + 1) else none

  data.frame(station = station_order[j],
             date = date,
             overlap_before = before$overlap,
             gap_before     = before$gap,
             overlap_after  = after$overlap,
             gap_after      = after$gap)
}

