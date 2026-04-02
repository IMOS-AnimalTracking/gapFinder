#' @title Extract daily transmissions/detections information
#'
#' @description This function will extract daily information on the number of transmissions, and their respective detections, 
#' from all sentinel transmitters available within all receivers deployed. 
#'
#' @param input File exported from match_rec_meta()
#' @param path_meta Path to the receiver deployment metadata csv file.
#' @param receiver_id Column in the metadata file identifying the receiver name as "model-serial" format (e.g. VR2W-12345)
#' @param transmitter_id Column in the metadata file identifying the transmitter id format (e.g. A69-1601-12345)
#' @param deploy_id Column in the metadata file identifying the receiver deployment number, if present. Otherwise, it will be created by default. 
#' @param station_name Column in the metadata file identifying the station names.
#' @param deploy_datetime Column in the metadata file identifying the receiver deployment date and time in Y-m-d H:M:S format. 
#' @param recover_datetime Column in the metadata file identifying the receiver recovery date and time in Y-m-d H:M:S format. 
#' @param lat Column in the metadata file identifying the deployment latitude. 
#' @param lon Column in the metadata file identifying the deployment longitude.
#' @param tz The corresponding timezone of the timestamps. Default is UTC.

#' @return Data frame containing the summary transmissions/detections of sentinel transmitters and acoustic receivers.
#' 
#' @import data.table
#'
#' @export
#' 
get_daily_dets <- function(input, 
  receiver_id, 
  path_meta,
  transmitter_id,
  deploy_id = NULL, 
  station_name, 
  deploy_datetime,
  recover_datetime,
  lat, 
  lon, 
  tz) {
  
  options(dplyr.summarise.inform = FALSE)

  # Load deployment metadata
  recmeta <- suppressWarnings(utils::read.csv(path_meta))
  # Sort metadata columns
  recmeta[,deploy_datetime] <- as.POSIXct(recmeta[,deploy_datetime],
    format = "%Y-%m-%d %H:%M:%S", tz = tz)
  recmeta[,recover_datetime] <- as.POSIXct(recmeta[,recover_datetime],
    format = "%Y-%m-%d %H:%M:%S", tz = tz)
  
  # Force data.table
  if (is.null(deploy_id))
    deploy_id <- "deploy_id"
  det <- data.table::as.data.table(input$det)
  cfg <- data.table::as.data.table(input$cfg_transmitter)
  data.table::setDT(det)
  data.table::setDT(cfg)
  # Standardise column name once
  data.table::setnames(det, old = "Full ID", new = "Full_ID", skip_absent = TRUE)
  data.table::setnames(cfg, old = "Full ID", new = "Full_ID", skip_absent = TRUE)
  # Verify columns exist
  if (!"Full_ID" %in% names(det)) {
    stop("det does not contain column 'Full_ID'. Names are: ",
         paste(names(det), collapse = ", "))
  }
  if (!"Full_ID" %in% names(cfg)) {
    stop("cfg does not contain column 'Full_ID'. Names are: ",
         paste(names(cfg), collapse = ", "))
  }
  det_recs <- det[det[["Full_ID"]] %in% recmeta[,transmitter_id], ]
  # If range transmitters are present
  if ("range_transmitter" %in% names(input)) {
    range <- data.table::as.data.table(input$range_transmitter)
    data.table::setDT(range)
    det_trans <- det[det[["Full_ID"]] %in% range[[transmitter_id]], ]
    det_trans$type <- "transmitter"
    if (nrow(det_recs) > 0)
      det_recs$type = "receiver"
    det <- rbind(det_recs, det_trans, fill = TRUE)
  } else {
    det_recs$type = "receiver"
    det <- det_recs
  }

  # Merge detections
  det[, aux := paste(Serial, Full_ID, sep = "_")]
  cfg[, aux := paste(Serial, Full_ID, sep = "_")]
  det[cfg, detection_type := "Transmission", on = "aux"]
  det[is.na(detection_type), detection_type := "Detection"]
  det[, receiver_name :=
        paste(stringr::str_split_fixed(Model, "-", 2)[, 1], Serial, sep = "-")]
  det[, date := data.table::as.IDate(Time, tz = "UTC")]
  # Change names to facilitate processing
  names(det)[which(names(det) == station_name)] <- "station_name"
  names(det)[which(names(det) == deploy_id)] <- "deploy_id"
  names(det)[which(names(det) == lat)] <- "lat"
  names(det)[which(names(det) == lon)] <- "lon"  
  # Extract daily data
  df_day <- det[
    ,
    .(
      lat = lat[1],
      lon = lon[1],
      detections = .N
    ),
    by = c(
      "date",
      "receiver_name",
      "type",
      "deploy_id",
      "station_name",
      "Full_ID",
      "detection_type"
    )
  ]

  # Add lost receivers data, if any
  aux_trans <- subset(df_day, detection_type == "Transmission")
  id_missing <- recmeta[which(!recmeta[,transmitter_id] %in% aux_trans$Full_ID),]
  if (nrow(id_missing) > 0) {
    id_save <- NULL
    for (i in 1:nrow(id_missing)) {
      id_run <- id_missing[i,]
      dates <- seq(as.Date(id_run[,deploy_datetime], tz = tz), 
        as.Date(id_run[,recover_datetime], tz = tz), 1)
      id_help <- df_day[rep(1, length(dates)),]
      # Add missing data
      id_help$date <- dates
      id_help$receiver_name <- id_run[,receiver_id]
      id_help$deploy_id <- 1
      id_help$station_name <- id_run[,station_name]
      id_help$Full_ID <- id_run[,transmitter_id]
      id_help$lat <- id_run[,lat]
      id_help$lon <- id_run[,lon]
      id_help$detection_type <- "Transmission"
      id_help$detections <- 144 # Max number of daily transmissions
      id_save <- rbind(id_save, id_help)
    }
    df_day <- rbind(df_day, id_save) 
  }

  # Add sentinel data daily transmissions, when these are present
  if ("transmitter" %in% unique(df_day$type)) {
    aux_trans <- input$range_transmitter
    trans_day <- aux_trans %>%
      dplyr::group_by(receiver_id, station_name, deploy_id) %>%
      dplyr::reframe(date = seq(as.Date(deploy_datetime[1], tz = tz), 
        as.Date(recover_datetime[1], tz = tz), 1))
    trans_day$delay_time <- aux_trans$delay_time[match(trans_day$receiver_id, aux_trans$receiver_id)]
    trans_day$deploy_lat <- aux_trans$lat[match(trans_day$receiver_id, aux_trans$receiver_id)]
    trans_day$deploy_lon <- aux_trans$lon[match(trans_day$receiver_id, aux_trans$receiver_id)]
    trans_day$trans <- round((24*60*60) / trans_day$delay_time, 0)
    final_trans <- df_day[rep(1, nrow(trans_day)),]
    final_trans$Full_receiver_id <- as.character(final_trans$Full_receiver_id)
    final_trans$detection_type <- "Transmission"
    # Match metadata
    final_trans$Full_ID <- trans_day$receiver_id
    final_trans$receiver_name <- trans_day$station_name
    final_trans$date <- trans_day$date
    final_trans$deploy_id <- trans_day$deploy_id
    final_trans$lat <- trans_day$deploy_lat
    final_trans$lon <- trans_day$deploy_lon
    final_trans$detections <- trans_day$trans
    # Combine datasets
    df_day <- rbind(df_day, final_trans)
  }

  # Sort dataset
  trans <- df_day[detection_type == "Transmission" & detections > 100]
  data.table::setnames(trans, "detections", "transmissions")
  dets <- df_day[detection_type == "Detection"]
  data.table::setnames(
    dets,
    c("station_name", "lat", "lon", "detections"),
    c("station_detected", "lat_detected", "lon_detected", "number_detections")
  )
  data.table::setkeyv(trans, c("date", "Full_ID"))
  data.table::setkeyv(dets,  c("date", "Full_ID"))
  out <- dets[trans, allow.cartesian = TRUE]
  out[is.na(station_detected), `:=`(
    station_detected = "None",
    lat_detected = NA_real_,
    lon_detected = NA_real_,
    number_detections = 0
  )]
  out[, efficiency := round(number_detections / transmissions, 4)]
  out <- out[efficiency <= 1]
  out[, distance_detected :=
        data.table::fifelse(
          is.na(lat_detected),
          NA_real_,
          round(
            geosphere::distHaversine(
              cbind(lon, lat),
              cbind(lon_detected, lat_detected)
            ),
            2
          )
        )
  ]
  # Rename columns and tidy things up
  out[, c("type", "i.type") := NULL]
  names(out) <- c("date", "receiver_name_detected", 
    "deploy_id", "station_detected",
    "Full_ID", "detection_type", "lat_detected",
    "lon_detected", "number_detections", "receiver_name_transmission",
    "receiver_deployment_id_transmission", "station_name_transmission",
    "i.detection_type", "lat_transmission", "lon_transmission",
    "transmissions", "efficiency", "distance_detected")
  out[, c("detection_type", "i.detection_type") := NULL]
  return(out)
}
