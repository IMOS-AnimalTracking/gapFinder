#' @title Match receiver and internal transmitter metadata
#'
#' @description This function matches receiver and internal transmitter information, when data is available across different files
#'
#' @param path_deploy Path to receiver deployment metadata
#' @param path_receiver Path to receiver metadata contaning internal transmitter IDs
#' @param transmitter_id Column name for internal transmitter ID (in path_receiver)
#' @param receiver_id Column name identifying receiver ID (note: must have same name in both files)
#' 
#' @return Data frame containing the respective receiver internal transmitter IDs
#'  
#' @export
#' 
match_rec_trans <- function(path_deploy, 
  path_receiver, 
  transmitter_id, 
  receiver_id) {
    recmeta <- suppressWarnings(utils::read.csv(path_deploy))
    rectrans <- suppressWarnings(utils::read.csv(path_receiver))
    # Rename transmitter name to facilitate matching
    names(rectrans)[which(names(rectrans) == transmitter_id)] <- "transmitter_id"
    names(rectrans)[which(names(rectrans) == receiver_id)] <- "receiver_id"
    names(recmeta)[which(names(recmeta) == receiver_id)] <- "receiver_id"
    # Match internal transmitters
    recmeta$transmitter_id <- rectrans$transmitter_id[match(recmeta$receiver_id, rectrans$receiver_id)]
    names(recmeta)[which(names(recmeta) == "transmitter_id")] <- transmitter_id
    names(recmeta)[which(names(recmeta) == "receiver_id")] <- receiver_id
    return(recmeta)
  }