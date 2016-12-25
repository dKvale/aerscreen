#' Read AERSCREEN input file
#'
#' Read an aerscreen.inp file or text into an AERSCREEN input table.
#' @param path A path to a file or data as a character string. 
#' @param as_one_df \code{TRUE} or \code{FALSE}. 
#'                  Return all inputs in a single wide data frame. 
#'                  If \code{FALSE}, 4 data frames are returned in a list: 
#'                         (1) control
#'                         (2) sources
#'                         (3) buildings
#'                         (4) surface
#' @keywords read load aerscreen input
#' @export
#' @examples
#' \dontrun{
#' aerscreen_inp <- new_aerscreen()
#' 
#' write_aerscreen(aerscreen_inp, "aerscreen.inp")
#' 
#' read_aerscreen_inp(file = "aerscreen.inp")
#' }
# 
read_aerscreen_inp <- function(path, 
                               as_one_df   = TRUE) {
  
  if("character" %in% class(path)) {
    
    if(!"\n" %in% path) inp <- readLines(path)
    
  } else stop("'file' is of class", class(path), ". Enter a file path")
  
  
  inp <- inp[grepl("[**]", inp) | grepl("TITLEONE", inp)]
  
  # SOURCE parameters
  start <- grep("STACK DATA", inp)[1] + 1
  end   <- start
  
  df <- gsub("[[:space:]]+", ",", inp[start:end])
  
  df <- utils::read.csv(textConnection(df), header = FALSE, stringsAsFactors = FALSE)
  
  so <- tibble::tibble(emit_gs      = df[ , 2],
                       height_m     = df[ , 3],
                       temp_k       = df[ , 4],
                       velocity_ms  = df[ , 5],
                       diameter_m   = df[ , 6],
                       urban_pop    = 0)
  
  # BUILDING parameters
  start <- grep("BUILDING DATA", inp)[1] + 1
  end   <- start
  
  df <- gsub("[[:space:]]+", ",", inp[start:end])
  
  df <- utils::read.csv(textConnection(df), header = FALSE, stringsAsFactors = FALSE)
  
  bu <- tibble::tibble(bpip_run            = df[ , 2],
                       bld_height          = df[ , 3],
                       long_side           = df[ , 4],
                       short_side          = df[ , 5],
                       bld_rotation        = df[ , 6],
                       dist_from_source    = df[ , 8],
                       angle_from_source   = df[ , 7])
                       
  # SURFACE Characteristics
  start <- grep("MAKEMET DATA", inp)[1] + 1
  end   <- start
  
  df <- gsub("[[:space:]]+", ",", inp[start:end])
  
  df <- utils::read.csv(textConnection(df), header = FALSE, stringsAsFactors = FALSE)
  
  su <- tibble::tibble(min_temp_k       = df[ , 2],
                       max_temp_k       = df[ , 3],
                       min_wind_speed   = df[ , 4],
                       anem_height      = df[ , 5],
                       surface_profile  = df[ , 6],
                       climate_profile  = df[ , 7],
                       albedo           = df[ , 8],
                       bowen            = df[ , 9],
                       z_length         = df[ , 10])
  
  # CONTROL OPTIONS
  ## ADJUST U*
  start <- grep("ADJUST U", inp)
  end   <- start
  
  df <- gsub("[[:space:]]+", ",", inp[start:end])
  
  df <- utils::read.csv(textConnection(df), header = FALSE, stringsAsFactors = FALSE)
  
  co <- tibble::tibble(near_receptor   = 1,
                       far_receptor    = 500,
                       adjust_ustar    = df[ , 4],
                       flagpole_height = 0,
                       debug_opt       = "N")
  
  ## Far receptor / modeling domain
  start <- grep("TERRAIN DATA", inp) + 1
  end   <- start
  
  df <- gsub("[[:space:]]+", ",", inp[start:end])
  
  df <- utils::read.csv(textConnection(df), header = FALSE, stringsAsFactors = FALSE)
  
  co$far_receptor <- df[ , 7]
  
  ## POPULATION & FLAGPOLE
  start <- grep("UNITS/POPULATION", inp) + 1
  end   <- start
  
  df <- gsub("[[:space:]]+", ",", inp[start:end])
  
  df <- utils::read.csv(textConnection(df), header = FALSE, stringsAsFactors = FALSE)
  
  so$urban_pop       <- df[ , 4]
  
  co$near_receptor   <- df[ , 5]
  
  co$flagpole_height <- df[ , 6]
  
  ## DEBUG On/Off
  start <- grep("DEBUG OPTION", inp) + 1
  end   <- start
  
  df <- gsub("[[:space:]]+", ",", inp[start:end])
  
  df <- utils::read.csv(textConnection(df), header = FALSE, stringsAsFactors = FALSE)
  
  co$debug_opt    <- df[ , 2]
  
  
  ## TEMPORAL sector
  
  
  # COMBINE input tables
  if(as_one_df) {
    aerscreen_inp <- cbind(co, so, bu, su)
  } else {
    aerscreen_inp <- list(control     = co,
                          sources     = so,
                          buildings   = bu,
                          surface     = su)
  }
  
  return(aerscreen_inp)
  
}