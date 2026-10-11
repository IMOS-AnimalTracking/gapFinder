#' @title Extract daily transmissions/detections information
#'
#' @description
#' This function extracts daily information on the number of transmissions
#' and their respective detections from all internal and sentinel transmitters
#' available within deployed receivers.
#'
#' @param input File exported from match_rec_meta().
#' @param circadian Should detection efficiencies by calculated separated for day/night? Default is FALSE.
#' @param delay Sentinel transmitter delay time (in seconds).
#' @param path_meta Path to the receiver deployment metadata CSV file.
#' @param type Column identifying the metadata deployment type. Options should
#'   include "receiver" and "transmitter" when sentinel transmitters are
#'   deployed in the study area.
#' @param receiver_id Column in the metadata identifying the receiver name in
#'   "model-serial" format (e.g. VR2W-12345).
#' @param transmitter_id Column in the metadata identifying the transmitter ID.
#' @param station_name Column in the metadata identifying station names.
#' @param deploy_id Column in the metadata identifying the receiver deployment
#'   number. If NULL, a deployment ID is created.
#' @param deploy_datetime Column in the metadata identifying receiver
#'   deployment date/time.
#' @param recover_datetime Column in the metadata identifying receiver
#'   recovery date/time.
#' @param lat Column in the metadata identifying deployment latitude.
#' @param lon Column in the metadata identifying deployment longitude.
#' @param tz Timezone corresponding to the timestamps. Default is UTC.
#'
#' @return
#' Data frame containing daily summary information for sentinel transmitters
#' and acoustic receivers.
#'
#' @import data.table
#' @export
#'
get_daily_dets <- function(
    input,
    delay = 660,
    circadian = FALSE,
    receiver_id,
    path_meta,
    transmitter_id,
    deploy_id = NULL,
    station_name,
    type = NULL,
    deploy_datetime,
    recover_datetime,
    lat,
    lon,
    tz = "UTC"
) {

  # ==========================================================================
  # 1. LOAD AND STANDARDISE METADATA
  # ==========================================================================

  recmeta <- data.table::as.data.table(
    suppressWarnings(
      utils::read.csv(
        path_meta,
        stringsAsFactors = FALSE
      )
    )
  )

  # Check required metadata columns
  required_meta <- c(
    receiver_id,
    transmitter_id,
    station_name,
    deploy_datetime,
    recover_datetime,
    lat,
    lon
  )

  missing_meta <- setdiff(
    required_meta,
    names(recmeta)
  )

  if (length(missing_meta) > 0) {
    stop(
      "The following columns are missing from the receiver metadata: ",
      paste(missing_meta, collapse = ", ")
    )
  }


  # --------------------------------------------------------------------------
  # Standardise metadata column names
  # --------------------------------------------------------------------------

  data.table::setnames(
    recmeta,
    old = c(
      receiver_id,
      transmitter_id,
      station_name,
      deploy_datetime,
      recover_datetime,
      lat,
      lon
    ),
    new = c(
      "receiver_id",
      "transmit_id",
      "station_name",
      "receiver_deployment_datetime",
      "receiver_recovery_datetime",
      "receiver_deployment_latitude",
      "receiver_deployment_longitude"
    ),
    skip_absent = FALSE
  )


  # --------------------------------------------------------------------------
  # Deployment ID
  # --------------------------------------------------------------------------

  if (is.null(deploy_id)) {

    # Create a unique deployment ID for each receiver deployment.
    #
    # rowid(receiver_id) gives deployment_1, deployment_2, etc. for each
    # receiver independently.

    recmeta[
      ,
      deploy_id := paste0(
        "deployment_",
        data.table::rowid(receiver_id)
      )
    ]

  } else {

    if (!deploy_id %in% names(recmeta)) {
      stop(
        "The deployment ID column '",
        deploy_id,
        "' was not found in the metadata."
      )
    }

    data.table::setnames(
      recmeta,
      old = deploy_id,
      new = "deploy_id"
    )
  }

  # Always use character deployment IDs.
  recmeta[
    ,
    deploy_id := as.character(deploy_id)
  ]


  # --------------------------------------------------------------------------
  # Deployment type
  # --------------------------------------------------------------------------

  if (is.null(type)) {

    # If no type column was supplied, all metadata are assumed to be receivers.
    recmeta[
      ,
      type := "receiver"
    ]

  } else {

    if (!type %in% names(recmeta)) {
      stop(
        "The type column '",
        type,
        "' was not found in the metadata."
      )
    }

    if (type != "type") {
      data.table::setnames(
        recmeta,
        old = type,
        new = "type"
      )
    }
  }


  # --------------------------------------------------------------------------
  # Standardise date/time columns
  # --------------------------------------------------------------------------

  recmeta[
    ,
    receiver_deployment_datetime :=
      as.POSIXct(
        receiver_deployment_datetime,
        format = "%Y-%m-%d %H:%M:%S",
        tz = tz
      )
  ]

  recmeta[
    ,
    receiver_recovery_datetime :=
      as.POSIXct(
        receiver_recovery_datetime,
        format = "%Y-%m-%d %H:%M:%S",
        tz = tz
      )
  ]


  # --------------------------------------------------------------------------
  # Standardise coordinates
  # --------------------------------------------------------------------------

  recmeta[
    ,
    `:=`(
      receiver_deployment_latitude =
        as.numeric(receiver_deployment_latitude),

      receiver_deployment_longitude =
        as.numeric(receiver_deployment_longitude)
    )
  ]


  # ==========================================================================
  # 2. PREPARE DETECTIONS AND TRANSMITTER CONFIGURATION
  # ==========================================================================

  if (!"det" %in% names(input)) {
    stop("input does not contain a 'det' object.")
  }

  if (!"cfg_transmitter" %in% names(input)) {
    stop("input does not contain a 'cfg_transmitter' object.")
  }

  det <- data.table::as.data.table(
    input$det
  )

  cfg <- data.table::as.data.table(
    input$cfg_transmitter
  )


  # --------------------------------------------------------------------------
  # Standardise Full ID
  # --------------------------------------------------------------------------

  data.table::setnames(
    det,
    old = "Full ID",
    new = "Full_ID",
    skip_absent = TRUE
  )

  data.table::setnames(
    cfg,
    old = "Full ID",
    new = "Full_ID",
    skip_absent = TRUE
  )

  if (!"Full_ID" %in% names(det)) {
    stop(
      "det does not contain column 'Full_ID'. Names are: ",
      paste(names(det), collapse = ", ")
    )
  }

  if (!"Full_ID" %in% names(cfg)) {
    stop(
      "cfg_transmitter does not contain column 'Full_ID'. Names are: ",
      paste(names(cfg), collapse = ", ")
    )
  }


  # --------------------------------------------------------------------------
  # Check required detection columns
  # --------------------------------------------------------------------------

  required_det <- c(
    "Model",
    "Serial",
    "Time"
  )

  missing_det <- setdiff(
    required_det,
    names(det)
  )

  if (length(missing_det) > 0) {
    stop(
      "The detection data are missing: ",
      paste(missing_det, collapse = ", ")
    )
  }


  # --------------------------------------------------------------------------
  # Create receiver ID
  # --------------------------------------------------------------------------

  det[
    ,
    receiver_id := paste(
      Model,
      Serial,
      sep = "-"
    )
  ]


  # --------------------------------------------------------------------------
  # Standardise detection timestamp
  # --------------------------------------------------------------------------

  det[
    ,
    Time := as.POSIXct(
      Time,
      tz = tz
    )
  ]


  # --------------------------------------------------------------------------
  # Remove any existing deploy_id before assigning metadata deployment IDs.
  #
  # This prevents data.table from trying to coerce character IDs such as
  # "deployment_1" into an existing integer column.
  # --------------------------------------------------------------------------

  if ("deploy_id" %in% names(det)) {
    det[
      ,
      deploy_id := NULL
    ]
  }


  # ==========================================================================
  # 3. ASSIGN RECEIVER/TRANSMITTER DEPLOYMENT ID TO EACH DETECTION
  # ==========================================================================

  # Receiver
  det[
    recmeta,
    deploy_id := i.deploy_id,
    on = .(
      receiver_id,
      Time >= receiver_deployment_datetime,
      Time <= receiver_recovery_datetime
    )
  ]
  # Transmitter
  na_rows <- det[is.na(deploy_id)]
  if (nrow(na_rows) > 0) {
    na_rows[
      recmeta,
      deploy_id := i.deploy_id,
      on = .(
        Full_ID == transmit_id,
        Time >= receiver_deployment_datetime,
        Time <= receiver_recovery_datetime
      )
    ]
    det[
      is.na(deploy_id),
      deploy_id := na_rows$deploy_id
    ]
  }
  # Select only receiver/transmitters present in metadata
  det <- det[which(!is.na(det$deploy_id)),]


  # ==========================================================================
  # 4. IDENTIFY INTERNAL RECEIVER TRANSMITTERS
  # ==========================================================================

  # These are transmitters associated with receiver deployments.
  # Sentinel transmitters have type == "transmitter" and are handled separately.

  internal_meta <- recmeta[
    type == "receiver" &
      !is.na(transmit_id) &
      transmit_id != ""
  ]

  # --------------------------------------------------------------------------
  # Keep detections corresponding to internal transmitters
  # --------------------------------------------------------------------------

  det_recs <- det[
    Full_ID %in% internal_meta$transmit_id
  ]

  if (nrow(det_recs) > 0) {
    det_recs[
      ,
      type := "receiver"
    ]
  }


  # ==========================================================================
  # 5. ADD RANGE TRANSMITTER DETECTIONS
  # ==========================================================================

  if ("range_transmitter" %in% names(input)) {

    range_det <- data.table::as.data.table(
      input$range_transmitter
    )

    # The range transmitter object should contain the same transmitter ID
    # column specified by transmitter_id.

    if (!"Full_ID" %in% names(range_det) &&
        !transmitter_id %in% names(range_det)) {

      stop(
        "The range_transmitter object does not contain either 'Full_ID' ",
        "or the specified transmitter ID column '",
        transmitter_id,
        "'."
      )
    }

    # Use Full_ID if already available.
    if ("Full_ID" %in% names(range_det)) {

      range_ids <- unique(
        range_det$Full_ID
      )

    } else {

      range_ids <- unique(
        range_det[[transmitter_id]]
      )
    }


    det_trans <- det[
      Full_ID %in% range_ids
    ]

    if (nrow(det_trans) > 0) {
      det_trans[
        ,
        type := "transmitter"
      ]
    }


    # Combine internal-transmitter and range-transmitter detections.

    det <- data.table::rbindlist(
      list(
        det_recs,
        det_trans
      ),
      fill = TRUE,
      use.names = TRUE
    )

  } else {

    det <- det_recs
  }


  # ==========================================================================
  # 6. CLASSIFY TRANSMISSIONS AND DETECTIONS
  # ==========================================================================

  # Create an identifier linking receiver serial numbers to transmitter IDs.

  det[
    ,
    aux := paste(
      Serial,
      Full_ID,
      sep = "_"
    )
  ]

  cfg[
    ,
    aux := paste(
      Serial,
      Full_ID,
      sep = "_"
    )
  ]

  # IDs occurring in the transmitter configuration are transmissions.
  transmission_ids <- unique(
    cfg$aux
  )

  det[
    aux %in% transmission_ids,
    detection_type := "Transmission"
  ]

  det[
    is.na(detection_type),
    detection_type := "Detection"
  ]


  # --------------------------------------------------------------------------
  # Create date
  # --------------------------------------------------------------------------

  det[
    ,
    date := data.table::as.IDate(
      Time,
      tz = tz
    )
  ]


  # ==========================================================================
  # 7. STANDARDISE DETECTION METADATA
  # ==========================================================================

  # These columns are expected to be present in the detection object.

  if (station_name %in% names(det) &&
      station_name != "station_name") {

    data.table::setnames(
      det,
      old = station_name,
      new = "station_name"
    )
  }

  if (lat %in% names(det) &&
      lat != "lat") {

    data.table::setnames(
      det,
      old = lat,
      new = "lat"
    )
  }

  if (lon %in% names(det) &&
      lon != "lon") {

    data.table::setnames(
      det,
      old = lon,
      new = "lon"
    )
  }

  # Convert coordinates to numeric.
  if ("lat" %in% names(det)) {
    det[
      ,
      lat := as.numeric(lat)
    ]
  }

  if ("lon" %in% names(det)) {
    det[
      ,
      lon := as.numeric(lon)
    ]
  }


  # ==========================================================================
  # 8. EXTRACT DAILY DATA
  # ==========================================================================

  if (circadian) {

    # ---- 1. One row per station/date, plus a "local" date used ONLY to query
    #         suncalc with the correct calendar day. Time is UTC; sunrise/dusk
    #         are inherently local, so a UTC date can be the wrong local day
    #         near local midnight (worst at high |lon|). ----
    df_times <- det[, .(lon = lon[1], lat = lat[1]), by = .(station_name, date)]
    df_times[, local_date := as.Date(date + lubridate::hours(round(lon / 15)))]

    # ---- 2. Expand to local_date-1 .. local_date+1 per station so every UTC
    #         detection is guaranteed to fall inside a window computed from the
    #         correct local day, even if it wraps past UTC midnight. ----
    df_times_expanded <- unique(df_times[, .(station_name, lon, lat, local_date)])
    df_times_expanded <- df_times_expanded[
      , .(local_date = seq(min(local_date) - 1, max(local_date) + 1, by = "day")),
      by = .(station_name, lon, lat)
    ]

    # ---- 3. Single vectorized suncalc call on the corrected dates ----
    sun_times <- suncalc::getSunlightTimes(
      data = df_times_expanded[, .(date = local_date, lat, lon)],
      keep = c("nauticalDawn", "nauticalDusk"),
      tz = "UTC"
    )
    df_times_expanded[, `:=`(dawn = sun_times$nauticalDawn, dusk = sun_times$nauticalDusk)]

    # ---- 4. Build one daylight-window interval [dawn, dusk] per station/day ----
    day_intervals <- df_times_expanded[!is.na(dawn) & !is.na(dusk) & dawn <= dusk,
                                        .(station_name, start = dawn, end = dusk)]
    data.table::setkey(day_intervals, station_name, start, end)

    det[, row_id := .I]
    det[, `:=`(t_start = Time, t_end = Time)]

    matched <- data.table::foverlaps(
      det[, .(row_id, station_name, t_start, t_end)],
      day_intervals,
      by.x = c("station_name", "t_start", "t_end"),
      by.y = c("station_name", "start", "end"),
      type = "within",
      nomatch = NULL
    )

    det[, circadian := "night"]
    det[row_id %in% matched$row_id, circadian := "day"]

    # ---- 5. Polar/degenerate fallback: any station/date where nauticalDawn or
    #         nauticalDusk is NA, or where dawn > dusk (a degenerate suncalc
    #         result, usually near-polar), gets resolved from solar altitude
    #         instead of relying on the interval window. ----
    bad_stations_by_date <- df_times_expanded[is.na(dawn) | is.na(dusk) | dawn > dusk,
                                               .(station_name, local_date)]

    det[, local_date := as.Date(date + lubridate::hours(round(lon / 15)))]
    det[bad_stations_by_date, is_degenerate := TRUE, on = .(station_name, local_date)]

    polar_rows <- det[is_degenerate == TRUE]

    if (nrow(polar_rows) > 0) {
      pos <- suncalc::getSunlightPosition(
        data = polar_rows[, .(date = Time, lat, lon)],
        keep = "altitude"
      )
      det[row_id %in% polar_rows$row_id,
          circadian := ifelse(pos$altitude > (-12 * pi / 180), "day", "night")]
    }

    det[, c("row_id", "t_start", "t_end", "local_date", "is_degenerate") := NULL]

    # ---- Get daily data ----
    df_day <- det[
      ,
      .(lat = lat[1], lon = lon[1], detections = .N),
      by = .(date, circadian, receiver_id, type, deploy_id, station_name, Full_ID, detection_type)
    ]

  } else {

    df_day <- det[
      ,
      .(lat = lat[1], lon = lon[1], detections = .N),
      by = .(date, receiver_id, type, deploy_id, station_name, Full_ID, detection_type)
    ]
  }
  
  # Ensure numeric coordinates.
  df_day[
    ,
    `:=`(
      lat = as.numeric(lat),
      lon = as.numeric(lon)
    )
  ]

  # ==========================================================================
  # 9. ADD MISSING INTERNAL-TRANSMITTER DAYS
  # ==========================================================================

  # FIX FOR WHEN CIRCADIAN = TRUE!
  aux_trans <- df_day[
    detection_type == "Transmission"
  ]

  observed_transmitters <- unique(
    aux_trans$Full_ID
  )

  # Internal transmitters that have no transmission records at all.
  id_missing <- internal_meta[
    !transmit_id %in% observed_transmitters &
      !is.na(receiver_recovery_datetime)
  ]

  if (nrow(id_missing) > 0) {

    # Create one row per day for each missing internal transmitter.

    if (circadian) {

      id_save <- id_missing[
        ,
        {
          days <- seq(as.IDate(receiver_deployment_datetime),
                      as.IDate(receiver_recovery_datetime),
                      by = "day")
          .(date = rep(days, each = 2),
            circadian = rep(c("night", "day"), times = length(days)))
        },
        by = .(
          receiver_id,
          deploy_id,
          station_name,
          Full_ID = transmit_id,
          lat = receiver_deployment_latitude,
          lon = receiver_deployment_longitude
        )
      ]

    } else {

      id_save <- id_missing[
        ,
        .(
          date = seq(
            as.IDate(receiver_deployment_datetime),
            as.IDate(receiver_recovery_datetime),
            by = "day"
          )
        ),
        by = .(
          receiver_id,
          deploy_id,
          station_name,
          Full_ID = transmit_id,
          lat = receiver_deployment_latitude,
          lon = receiver_deployment_longitude
        )
      ]

    }
  

    # ------------------------------------------------------------------------
    # Expected number of daily transmissions
    # ------------------------------------------------------------------------
    
    delay_lookup <- id_missing[
      ,
      .(
        Full_ID = transmit_id,
        delay = delay
      )
    ]

    id_save[
      delay_lookup,
      delay := i.delay,
      on = "Full_ID"
    ]

    id_save[
      ,
      detections := data.table::fifelse(
        !is.na(delay) & delay > 0,
        round(86400 / delay),
        144
      )
    ]


    id_save[
      ,
      `:=`(
        detection_type = "Transmission",
        type = "receiver"
      )
    ]

    id_save[
      ,
      delay := NULL
    ]


    df_day <- data.table::rbindlist(
      list(
        df_day,
        id_save
      ),
      fill = TRUE,
      use.names = TRUE
    )
  }


  # ==========================================================================
  # 10. ADD SENTINEL TRANSMITTER DAILY TRANSMISSIONS
  # ==========================================================================

  if ("transmitter" %in% unique(recmeta$type)) {

    # Metadata for sentinel transmitters
    aux_trans <- recmeta[
      type == "transmitter"
    ]

    # ------------------------------------------------------------------------
    # Create ONE ROW PER SENTINEL TRANSMITTER PER DAY
    # ------------------------------------------------------------------------

    trans_day <- aux_trans[
      !is.na(receiver_deployment_datetime) &
        !is.na(receiver_recovery_datetime),

      .(
        date = seq(
          as.IDate(receiver_deployment_datetime),
          as.IDate(receiver_recovery_datetime),
          by = "day"
        )
      ),

      by = .(
        transmit_id,
        receiver_id,
        station_name,
        deploy_id,
        lat = receiver_deployment_latitude,
        lon = receiver_deployment_longitude,
        delay
      )
    ]

    # ------------------------------------------------------------------------
    # Calculate expected number of transmissions per day
    #
    # delay is assumed to be in seconds.
    #
    # e.g.:
    #   delay = 600 seconds  -> 144 transmissions/day
    #   delay = 900 seconds  -> 96 transmissions/day
    # ------------------------------------------------------------------------

    trans_day[
      ,
      delay := suppressWarnings(
        as.numeric(delay)
      )
    ]

    trans_day[
      ,
      transmissions := fifelse(
        !is.na(delay) & delay > 0,
        round(86400 / delay),
        NA_real_
      )
    ]


    # ------------------------------------------------------------------------
    # Create the daily transmission records in the same format as df_day
    # ------------------------------------------------------------------------

    sentinel_day <- trans_day[
      ,
      .(
        date = date,
        receiver_id = receiver_id,
        deploy_id = deploy_id,
        station_name = station_name,
        lat = lat,
        lon = lon,

        # IMPORTANT:
        # Full_ID must be the SENTINEL TRANSMITTER ID
        Full_ID = transmit_id,

        detection_type = "Transmission",

        # Number of expected transmissions for this transmitter on this day
        detections = transmissions,

        type = "transmitter"
      )
    ]


    # ------------------------------------------------------------------------
    # Append sentinel daily transmissions to the main daily dataset
    # ------------------------------------------------------------------------

    df_day <- data.table::rbindlist(
      list(
        df_day,
        sentinel_day
      ),
      fill = TRUE,
      use.names = TRUE
    )
  }

  # ==========================================================================
  # 11. SEPARATE TRANSMISSIONS AND DETECTIONS
  # ==========================================================================

  trans <- df_day[
    detection_type == "Transmission" &
      !is.na(detections) &
      detections > 0
  ]

  data.table::setnames(
    trans,
    old = "detections",
    new = "transmissions"
  )

  dets <- df_day[
    detection_type == "Detection"
  ]


  # ==========================================================================
  # 12. JOIN DETECTIONS TO TRANSMISSIONS
  # ==========================================================================

  data.table::setkeyv(
    trans,
    c("date", "Full_ID")
  )

  data.table::setkeyv(
    dets,
    c("date", "Full_ID")
  )


  out <- dets[
    trans,
    allow.cartesian = TRUE
  ]


  # ==========================================================================
  # 13. ADD ZERO-DETECTION RECORDS
  # ==========================================================================

  out[
    is.na(i.station_name),
    `:=`(
      station_name = "None",
      lat = NA_real_,
      lon = NA_real_,
      detections = 0
    )
  ]


  # ==========================================================================
  # 14. CALCULATE DETECTION EFFICIENCY
  # ==========================================================================

  out[
    ,
    efficiency := round(
      detections / transmissions,
      4
    )
  ]

  # Remove impossible efficiencies.
  out <- out[
    efficiency <= 1
  ]


  # ==========================================================================
  # 15. CALCULATE DISTANCE BETWEEN RECEIVERS
  # ==========================================================================

  out[
    ,
    `:=`(
      lon = suppressWarnings(as.numeric(lon)),
      lat = suppressWarnings(as.numeric(lat)),
      i.lon = suppressWarnings(as.numeric(i.lon)),
      i.lat = suppressWarnings(as.numeric(i.lat))
    )
  ]

  out[
    ,
    distance_detected :=
      data.table::fifelse(
        is.na(i.lat) | is.na(i.lon),
        NA_real_,
        round(
          geosphere::distHaversine(
            cbind(
              lon,
              lat
            ),
            cbind(
              i.lon,
              i.lat
            )
          ),
          2
        )
      )
  ]

  # ==========================================================================
  # 16. CLEAN OUTPUT
  # ==========================================================================

  # Remove deployment type columns.
  remove_cols <- intersect(
    c("type", "i.type"),
    names(out)
  )
  if (length(remove_cols) > 0) {
    out[
      ,
      (remove_cols) := NULL
    ]
  }

  # --------------------------------------------------------------------------
  # Rename columns
  # --------------------------------------------------------------------------

  rename_map <- c(
    receiver_id = "receiver_detected",
    deploy_id = "deploy_id_detected",
    station_name = "station_detected",
    lat = "lat_detected",
    lon = "lon_detected",
    detections = "number_detections",
    i.receiver_id = "receiver_transmission",
    i.deploy_id = "deploy_id_transmission",
    i.station_name = "station_transmission",
    i.lat = "lat_transmission",
    i.lon = "lon_transmission"
  )

  for (old_name in names(rename_map)) {

    if (old_name %in% names(out)) {

      data.table::setnames(
        out,
        old = old_name,
        new = rename_map[[old_name]]
      )
    }
  }
  # Remove detection-type columns.
  remove_cols <- intersect(
    c("detection_type", "i.detection_type"),
    names(out)
  )
  if (length(remove_cols) > 0) {
    out[
      ,
      (remove_cols) := NULL
    ]
  }


  # ==========================================================================
  # 17. FINAL COLUMN ORDER
  # ==========================================================================

  if (circadian) {

    output_columns <- c(
      "date",
      'circadian',
      "receiver_detected",
      "deploy_id_detected",
      "station_detected",
      "lat_detected",
      "lon_detected",
      "number_detections",
      "receiver_transmission",
      "Full_ID",
      "deploy_id_transmission",
      "station_transmission",
      "lat_transmission",
      "lon_transmission",
      "transmissions",
      "efficiency",
      "distance_detected"
    )

  } else {

    output_columns <- c(
      "date",
      "receiver_detected",
      "deploy_id_detected",
      "station_detected",
      "lat_detected",
      "lon_detected",
      "number_detections",
      "receiver_transmission",
      "Full_ID",
      "deploy_id_transmission",
      "station_transmission",
      "lat_transmission",
      "lon_transmission",
      "transmissions",
      "efficiency",
      "distance_detected"
    )

  }

  output_columns <- intersect(
    output_columns,
    names(out)
  )

  out <- out[
    ,
    ..output_columns
  ]

  if (circadian)
    out$circadian <- as.factor(out$circadian)

  # ==========================================================================
  # 18. RETURN
  # ==========================================================================

  return(out)
}