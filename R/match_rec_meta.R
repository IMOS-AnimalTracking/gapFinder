#' @title Match receiver deployment metadata to parsed Fathom files
#'
#' @description This function uses a receiver metadata to add receiver deployment data to the loaded Fathom files.
#'
#' @param input List of files opened with parse_fathom_files() in FathomR.
#' @param rec_meta Path to the receiver deployment metadata csv file.
#' @param station_name Column in the metadata file identifying the station names.
#' @param rec_name Column in the metadata file identifying the receiver name as "model-serial" format (e.g. VR2W-12345)
#' @param rec_deploy_id Column in the metadata file identifying the receiver deployment number, if present. Otherwise, it will be created by default. 
#' @param rec_deploy_datetime Column in the metadata file identifying the receiver deployment date and time in Y-m-d H:M:S format. 
#' @param rec_recover_datetime Column in the metadata file identifying the receiver recovery date and time in Y-m-d H:M:S format. 
#' @param tz The corresponding timezone of the timestamps. Default is UTC.
#' @param rec_lat Column in the metadata file identifying the deployment latitude. 
#' @param rec_lon Column in the metadata file identifying the deployment longitude.
#' 
#' @return List of Fathom files, including the matching metadata information. Please note that rows with no matching metadata will not be returned in the
#' exported outputs.
#' 
#' @export
#' 
match_rec_meta <- function(input, 
  rec_meta,
  station_name,
  rec_name, 
  rec_deploy_id = NULL,
  rec_deploy_datetime, 
  rec_recover_datetime,
  tz = "UTC",
  rec_lat,
  rec_lon) { 
  
  # Load metadata file and check parameters
  recmeta <- utils::read.csv(rec_meta)
  if (is.null(rec_deploy_id))
    recmeta$rec_deploy_id <- 1:nrow(recmeta)
  ## Initial checks of parameters
  if(!station_name %in% colnames(recmeta)){stop("Cannot find station_name column in dataset, provide column name where variable can be found")}
  if(!rec_name %in% colnames(recmeta)){stop("Cannot find rec_meta column in dataset, provide column name where variable can be found")}
  if(!rec_deploy_id %in% colnames(recmeta)){stop("Cannot find rec_deploy_id column in dataset, provide column name where variable can be found")}
  if(!rec_deploy_datetime %in% colnames(recmeta)){stop("Cannot find rec_deploy_datetime column in dataset, provide column name where variable can be found")}
  if(!rec_recover_datetime %in% colnames(recmeta)){stop('Cannot find rec_recover_datetime column in dataset, provide column name where variable can be found')}
  if(!rec_lat %in% colnames(recmeta)){stop("Cannot find rec_lat column in dataset, provide column name where variable can be found")}
  if(!rec_lon %in% colnames(recmeta)){stop("Cannot find rec_lon column in dataset, provide column name where variable can be found")}
  # Sort metadata columns
  recmeta[,rec_deploy_datetime] <- as.POSIXct(recmeta[,rec_deploy_datetime],
    format = "%Y-%m-%d %H:%M:%S", tz = tz)
  recmeta[,rec_recover_datetime] <- as.POSIXct(recmeta[,rec_recover_datetime],
    format = "%Y-%m-%d %H:%M:%S", tz = tz)
    # Remove deployments with no recovery info
    index <- which(is.na(recmeta[,rec_recover_datetime]),)
    recmeta <- recmeta[-index,]
    if (nrow(recmeta) == 0)
      stop("No receiver recovery date and time information present in the metadata. Please check metadata for missing data.")
    # Add serial info
    recmeta$Serial <- suppressWarnings(as.integer(stringr::str_split_fixed(recmeta[,rec_name], pattern = "-", n = 2)[,2]))
    recmeta <- recmeta[which(recmeta$Serial %in% unique(input[[1]]$Serial)),] # Select only receivers in the data
    if (nrow(recmeta) == 0)
      stop("No metadata found for the receivers present in the input Fathom files. Please check metadata for missing data.") 
  # Copy input file for export
  output <- input 
  # List of possible files to be processed
  files.aux <- names(input)
  files.tot <- c("attitude", "battery", "cfg_channel", 
    "clock_ref", "depth", "det", "diag", "event", "event_init",
    "event_offload", "health_vr2ar", "temp")
  files.aux <- files.tot[which(files.tot %in% files.aux)]
  if (length(files.aux) == 0) 
    stop("Files present in FathomR input not recognised. Options include:
      'attitude', 'battery', 'cfg_channel', 'clock_ref', 'depth', 'det', 'diag', 'event', 'event_init', 'event_offload', 'health_vr2ar', 'temp'")
  # Processing starts
  message("Matching receiver deployment metadata")
  pb <- utils::txtProgressBar(min = 0, max = length(files.aux), initial = 0, style = 3, width = 50)
  for (i in 1:length(files.aux)) {
    aux.file <- input[[files.aux[i]]]
    aux.matched <- data_match(df = aux.file, meta = recmeta,
      rec_deploy_datetime = rec_deploy_datetime,
      rec_recover_datetime = rec_recover_datetime, 
      rec_deploy_id = rec_deploy_id, 
      station_name = station_name, 
      rec_lat = rec_lat, 
      rec_lon = rec_lon)
    # Replace original file by matched file
    index <- which(names(output) == files.aux[i])
    output[[index]] <- aux.matched
    utils::setTxtProgressBar(pb, i) 
  } 
  close(pb)
  return(output)
}
