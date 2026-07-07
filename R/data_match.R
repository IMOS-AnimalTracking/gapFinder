#' @title Auxiliary function to combine datasets
#'
#' @description Function used to match Fathom files and metadata.
#'
#' @param df List of files opened with parse_fathom_files() in FathomR.
#' @param meta Path to the receiver deployment metadata csv file.
#' @param station_name Column in the metadata file identifying the station names.
#' @param deploy_id Column in the metadata file identifying the receiver deployment number, if present. Otherwise, it will be created by default. 
#' @param deploy_datetime Column in the metadata file identifying the receiver deployment date and time in Y-m-d H:M:S format. 
#' @param recover_datetime Column in the metadata file identifying the receiver recovery date and time in Y-m-d H:M:S format. 
#' @param lat Column in the metadata file identifying the deployment latitude. 
#' @param lon Column in the metadata file identifying the deployment longitude.
#' 
#' @return Table contatining matched data
#' 
#' @keywords internal
#'  
data_match <- function(df, meta, station_name, deploy_id, deploy_datetime, recover_datetime, lat, lon) {
  # Fixed timestamps (points)
  points <- data.table::data.table(df)
  # Time intervals with values to attach
  intervals <- data.table::data.table(meta)
    # rename columns 
    names(intervals)[which(names(intervals) == deploy_datetime)] <- "deploy_datetime"
    names(intervals)[which(names(intervals) == recover_datetime)] <- "recover_datetime"
  # Point interval 
  points_interval <- points
  points_interval$deploy_datetime <- points_interval$Time
  points_interval$recover_datetime <- points_interval$Time
  # Key intervals
  data.table::setkey(intervals, Serial, deploy_datetime, recover_datetime)
  data.table::setkey(points_interval, Serial, deploy_datetime, recover_datetime)
  # Join
  result <- as.data.frame(data.table::foverlaps(
    points_interval,
    intervals,
    by.x = c("Serial", "deploy_datetime", "recover_datetime"),
    by.y = c("Serial", "deploy_datetime", "recover_datetime"),
    type = "within",
    nomatch = NULL
  ))
  # Tidy things up
  result <- tibble::as_tibble(result[,c(names(points), deploy_id, station_name, lat, lon)])
  names(result)[which(names(result) == station_name)] <- "station_name"
  return(result)
}
