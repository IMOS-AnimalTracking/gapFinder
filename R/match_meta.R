#' @title Match receiver deployment metadata to parsed Fathom files
#'
#' @description This function uses a receiver metadata to add receiver deployment data to the loaded Fathom files.
#'
#' @param input List of files opened with parse_fathom_files() in FathomR.
#' @param path_meta Path to the receiver deployment metadata csv file.
#' @param station_name Column in the metadata file identifying the station names.
#' @param type Column identifying the metadata deployment type when sentinel transmitters are also deployed in the study area. In these cases, options must include: receiver and transmitter
#' @param delay When sentinel transmitters are present in the metadata, column name indentifying their respective delay times in secons.
#' @param id Column in the metadata file identifying the receiver name as "model-serial" format (e.g. VR2W-12345)
#' @param deploy_id Column in the metadata file identifying the receiver deployment number, if present. Otherwise, it will be created by default. 
#' @param deploy_datetime Column in the metadata file identifying the receiver deployment date and time in Y-m-d H:M:S format. 
#' @param recover_datetime Column in the metadata file identifying the receiver recovery date and time in Y-m-d H:M:S format. 
#' @param tz The corresponding timezone of the timestamps. Default is UTC.
#' @param lat Column in the metadata file identifying the deployment latitude. 
#' @param lon Column in the metadata file identifying the deployment longitude.
#' 
#' @return List of Fathom files, including the matching metadata information. Please note that rows with no matching metadata will not be returned in the
#' exported outputs.
#' 
#' @export
#' 
match_meta <- function(input, 
  path_meta,
  station_name,
  type = NULL,
  delay = NULL,
  id, 
  deploy_id = NULL,
  deploy_datetime,
  recover_datetime,
  tz = "UTC",
  lat,
  lon) { 
  
  options(dplyr.summarise.inform = FALSE)

  # Load metadata file and check parameters
  recmeta <- suppressWarnings(utils::read.csv(path_meta))
  # Rename columns to make dplyr life easier
  if(!id %in% colnames(recmeta)){stop("Cannot find id column in dataset, provide column name where variable can be found")}
  names(recmeta)[which(names(recmeta) == id)] <- "id"
  # Check other columns
  if (is.null(type))
    recmeta$type <- "receiver"
  if (is.null(deploy_id)) { # Add deployment ID if NULL - to be used for plotting
    recmeta <- recmeta %>%
      dplyr::group_by(id, type) %>%
      dplyr::mutate(deploy_id = dplyr::row_number()) %>%
      dplyr::ungroup()
    recmeta <- as.data.frame(recmeta)
    deploy_id <- "deploy_id"
  }
  ## Initial checks of parameters
  if(!station_name %in% colnames(recmeta)){stop("Cannot find station_name column in dataset, provide column name where variable can be found")}
  if(!deploy_datetime %in% colnames(recmeta)){stop("Cannot find deploy_datetime column in dataset, provide column name where variable can be found")}
  if(!recover_datetime %in% colnames(recmeta)){stop('Cannot find recover_datetime column in dataset, provide column name where variable can be found')}
  if(!lat %in% colnames(recmeta)){stop("Cannot find lat column in dataset, provide column name where variable can be found")}
  if(!lon %in% colnames(recmeta)){stop("Cannot find lon column in dataset, provide column name where variable can be found")}
  # Sort metadata columns
  recmeta[,deploy_datetime] <- as.POSIXct(recmeta[,deploy_datetime],
    format = "%Y-%m-%d %H:%M:%S", tz = tz)
  recmeta[,recover_datetime] <- as.POSIXct(recmeta[,recover_datetime],
    format = "%Y-%m-%d %H:%M:%S", tz = tz)
    # Remove deployments with no recovery info
    index <- which(is.na(recmeta[,recover_datetime]),)
    if (length(index) > 0)
      recmeta <- recmeta[-index,]
    if (nrow(recmeta) == 0)
      stop("No receiver recovery date and time information present in the metadata. Please check metadata for missing data.")
    # Add receiver serial info and select only receiver/transmitters in the detection data
    recmeta$Serial <- suppressWarnings(as.integer(stringr::str_split_fixed(recmeta[,"id"], pattern = "-", n = 2)[,2]))
    index <- which(recmeta$Serial %in% unique(input[['det']]$Serial))
    index2 <- which(recmeta[,"id"] %in% unique(input[['det']]$`Full ID`))
    index <- c(index, index2)
    if (length(index) > 0) {
      recmeta <- recmeta[index,] 
    } else {
      stop("No metadata found for the receivers present in the input Fathom files. Please check metadata for missing data.") 
    }
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
      deploy_datetime = deploy_datetime,
      recover_datetime = recover_datetime, 
      deploy_id = deploy_id, 
      station_name = station_name, 
      lat = lat, 
      lon = lon)
    # Replace original file by matched file
    index <- which(names(output) == files.aux[i])
    output[[index]] <- aux.matched
    utils::setTxtProgressBar(pb, i) 
  }
  close(pb)
  # If sentinel transmitters are found in metadata
  if ("transmitter" %in% recmeta[,type]) {
    if (is.null(delay))
      stop("Sentinel transmitters found in the metadata. Please provide the column identifying their respective delay times in seconds.")
    names(recmeta)[which(names(recmeta) == type)] <- "type"
    transmeta <- recmeta %>%
      dplyr::filter(type == "transmitter")    
    output$range_transmitter <- transmeta
  }
  return(output)
}
