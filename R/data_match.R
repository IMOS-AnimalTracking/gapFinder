#' @title Auxiliary function to combine datasets
#'
#' @description Function used to match Fathom files and metadata.
#'
#' @param df List of files opened with parse_fathom_files() in FathomR.
#' @param meta Path to the receiver deployment metadata csv file.
#' @param station_name Column in the metadata file identifying the station names.
#' @param rec_deploy_id Column in the metadata file identifying the receiver deployment number, if present. Otherwise, it will be created by default. 
#' @param rec_deploy_datetime Column in the metadata file identifying the receiver deployment date and time in Y-m-d H:M:S format. 
#' @param rec_recover_datetime Column in the metadata file identifying the receiver recovery date and time in Y-m-d H:M:S format. 
#' @param rec_lat Column in the metadata file identifying the deployment latitude. 
#' @param rec_lon Column in the metadata file identifying the deployment longitude.
#' 
#' @return Table contatining matched data
#' 
#' @keywords internal
#'  
data_match <- function(df, 
	meta, 
	station_name, 
	rec_deploy_id, 
	rec_deploy_datetime,
  rec_recover_datetime, 
  rec_lat, 
  rec_lon) {
  # Fixed timestamps (points)
  points <- data.table::data.table(df)
  # Time intervals with values to attach
  intervals <- data.table::data.table(meta)
    # rename columns 
    names(intervals)[which(names(intervals) == rec_deploy_datetime)] <- "rec_deploy_datetime"
    names(intervals)[which(names(intervals) == rec_recover_datetime)] <- "rec_recover_datetime"
  # Point interval 
  points_interval <- points
  points_interval$rec_deploy_datetime <- points_interval$Time
  points_interval$rec_recover_datetime <- points_interval$Time
  # Key intervals
  data.table::setkey(intervals, Serial, rec_deploy_datetime, rec_recover_datetime)
  data.table::setkey(points_interval, Serial, rec_deploy_datetime, rec_recover_datetime)
  # Join
  result <- as.data.frame(data.table::foverlaps(
    points_interval,
    intervals,
    by.x = c("Serial", "rec_deploy_datetime", "rec_recover_datetime"),
    by.y = c("Serial", "rec_deploy_datetime", "rec_recover_datetime"),
    type = "within",
    nomatch = NULL
  ))
  # Tidy things up
  result <- tibble::as_tibble(result[,c(names(points), rec_deploy_id, station_name, rec_lat, rec_lon)])
  return(result)
}