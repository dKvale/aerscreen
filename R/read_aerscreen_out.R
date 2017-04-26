#' Read AERSCREEN output file
#'
#' Read an aerscreen.out file into an AERSCREEN results table.
#' @param path A path to a file or data as a character string.
#' @keywords read aerscreen load output results
#' @export
#' @examples
#' \dontrun{
#' read_aerscreen_out(path = "aerscreen.out")
#' }
# 
#
read_aerscreen_out <- function(path) {
  
  if("character" %in% class(path)) {
    
    if(!"\n" %in% path) out <- readLines(path)
    
  } else stop("'file' is of class", class(path), ". Enter a file path")
  
  out <- out[!grepl("[**] | --", out)]
  
  out <- subset(out, !out %in% c("", "\n"))

  # Read dispersion results
  options(digits = 10)
  
  start <- grep("PROCEDURE", out)[1] + 1
    
  end   <- start + 1
    
  df <- gsub("[[:space:]]+", ",", out[start])
    
  df <- read.csv(textConnection(df), header = FALSE, stringsAsFactors = FALSE)
    
  df <- df[ , -c(1:3)]
    
  names(df) <- c("max_1hr_conc", "max_3hr_conc", "max_8hr_conc", "max_24hr_conc", "max_annual_conc")
  
  df$conc_units <- "ug/m3"
  
  ## Add receptor distance
  receptor_distance <- strsplit(out[end], " ")[[1]]
  
  receptor_distance <- receptor_distance[grepl("[.]", receptor_distance)]
  
  df$receptor_distance <- as.numeric(receptor_distance)
  
  df$dist_units <- "meters"
  
  # Get AERSCREEN version
  cat(paste0("Model version:", out[1], "\n\n"))
  
  # Read AERSCREEN messages
  #cat(paste0(out[(grep("Summary of Total", out)[2]) : (grep("FATAL ERROR", out)[2] - 3)], collapse = "\n"), "\n")
  
  return(df)
}