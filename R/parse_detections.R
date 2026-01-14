#' @title Help function to load FathomR files
#'
#' @description Temporary function - will be included in future FathomR update.
#'
#' @param directory_path Path to where Fathom files are stored on disk. Can either be a 
#' a folder with all files, or multiple subfolders for the data to be processed in batches 
#' (useful for long-term studies to avoid memory kill)
#' 
#' @return List of loaded Fathom files.
#' 
#' @export
#' 
parse_detections <- function(directory_path) {
	# Load files, in batches if present, to avoid memory kill for large datasets
	subfold <- list.dirs(directory_path, full.names = TRUE)[-1]
	if (length(subfold) == 0) {
		fathom_list <- FathomR::parse_fathom_files(directory_path = directory_path,
			vars = c("det", "cfg_transmitter"))
	} else {
		for (i in 1:length(subfold)) {
			message(paste("Loading files in subfolder:", i))
			fathom_list <- FathomR::parse_fathom_files(directory_path = subfold[i],
				vars = c("det", "cfg_transmitter"))
			if (i == 1) {
				fathom_save <- fathom_list
			} else {
				fathom_save[[1]] <- rbind(fathom_save[[1]], fathom_list[[1]])
				fathom_save[[2]] <- rbind(fathom_save[[2]], fathom_list[[2]])
			}
		}
	}
	return(fathom_save)
}
