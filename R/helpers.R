#' Set a flag value at specified indices
#'
#' @param x A vector in which values will be replaced.
#' @param idx A logical vector or vector of indices indicating which values
#'   should be replaced.
#' @param value The value to assign. Defaults to 2.
#'
#' @return A vector with the specified values replaced by \code{value}.
#'
#' @noRd
set_flag <- function(x, idx, value = 2) {
  idx <- idx %in% TRUE                       # NA-safe
  if (is.factor(x)) {
    x <- factor(x, levels = union(levels(x), as.character(value)))
    x[idx] <- as.character(value)
  } else if (is.character(x)) {
    x[idx] <- as.character(value)
  } else {
    x[idx] <- value
  }
  x
}


#' Select the appropriate detection radius
#'
#' Selects the correlation-based radius when it is available and greater
#' than 1; otherwise, uses the standard detection range.
#'
#' @param range The standard detection range.
#' @param cor_range The correlation-based detection range.
#'
#' @return A vector containing the selected radius.
#'
#' @noRd
pick_radius <- function(range, cor_range) {
  ifelse(!is.na(cor_range) & cor_range > 1, cor_range, range)
}


#' Calculate the gap and overlap between two stations
#'
#' Calculates the distance between detection areas when they do not overlap,
#' or the proportional overlap when the detection areas overlap.
#'
#' @param s1 A data frame containing information for the first station.
#' @param s2 A data frame containing information for the second station.
#'
#' @return A list containing \code{gap} and \code{overlap}.
#'
#' @noRd
pair_gap <- function(s1, s2) {

  # Missing or duplicated rows cannot be compared
  if (nrow(s1) != 1 || nrow(s2) != 1)
    return(list(gap = NA, overlap = NA))

  r1 <- pick_radius(s1$range, s1$cor_range)
  r2 <- pick_radius(s2$range, s2$cor_range)

  # Only calculate if actual ranges are available (isTRUE handles NA)
  if (!isTRUE(r1 > 1 && r2 > 1))
    return(list(gap = NA, overlap = NA))

  aux.data <- data.frame(
    lon = c(s1$lon, s2$lon),
    lat = c(s1$lat, s2$lat),
    radius_m = c(r1, r2))

  overlap_matrix <- suppressWarnings(get_overlap_matrix(aux.data))

  if (overlap_matrix[2, 1] == 0) {          # no overlap -> gap size
    pts   <- sf::st_as_sf(aux.data, coords = c("lon", "lat"), crs = 4326)
    pts_m <- sf::st_transform(pts, 3857)
    buffs <- sf::st_buffer(pts_m, dist = aux.data$radius_m)
    list(gap = round(as.numeric(sf::st_distance(buffs[1, ], buffs[2, ])), 2),
         overlap = 0)
  } else {                                   # overlap -> no gap
    list(gap = 0,
         overlap = round(overlap_matrix[1, 2] /
                           max(c(overlap_matrix[1, 1], overlap_matrix[2, 2])), 2))
  }
}

