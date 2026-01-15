#' @title Extract daily transmissions/detections information
#'
#' @description This function will extract daily information on the number of transmissions, and their respective detections, 
#' from all sentinel transmitters available within all receivers deployed. 
#'
#' @param input File exported from match_rec_meta()
#' 
#' @return Data frame containing the summary transmissions/detections of sentinel transmitters and acoustic receivers.
#' 
#' @import data.table
#'
#' @export
#' 
get_daily_dets <- function(input) {
  options(dplyr.summarise.inform = FALSE)

  # Force data.table
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
  det <- det[det[["Full_ID"]] %in% cfg[["Full_ID"]], ]
  det[, aux := paste(Serial, Full_ID, sep = "_")]
  cfg[, aux := paste(Serial, Full_ID, sep = "_")]
  det[cfg, detection_type := "Transmission", on = "aux"]
  det[is.na(detection_type), detection_type := "Detection"]
  det[, receiver_name :=
        paste(stringr::str_split_fixed(Model, "-", 2)[, 1], Serial, sep = "-")]
  det[, date := data.table::as.IDate(Time, tz = "UTC")]
  df_day <- det[
    ,
    .(
      lat = receiver_deployment_latitude[1],
      lon = receiver_deployment_longitude[1],
      detections = .N
    ),
    by = c(
      "date",
      "receiver_name",
      "receiver_deployment_id",
      "station_name",
      "Full_ID",
      "detection_type"
    )
  ]
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
  names(out) <- c("date", "receiver_name_detected", 
    "receiver_deployment_id_detected", "station_detected",
    "Full_ID", "detection_type", "lat_detected",
    "lon_detected", "number_detections", "receiver_name_transmission",
    "receiver_deployment_id_transmission", "station_name_transmission",
    "i.detection_type", "lat_transmission", "lon_transmission",
    "transmissions", "efficiency", "distance_detected")
  out[, c("detection_type", "i.detection_type") := NULL]
  return(out)
}
