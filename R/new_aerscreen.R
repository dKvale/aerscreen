#' Create AERSCREEN input tables
#'
#' Create default AERSCREEN input data frames: 
#' (1) Control options
#' (2) Source parameters
#' (3) Building parameters
#' (4) Surface characteristics
#' @param aerscreen Name for AERSCREEN input data frame added to Global Environment. 
#'                  Default is "aerscreen_inp". Ignored if \code{as_one_df} is \code{FALSE}. 
#' @param as_one_df \code{TRUE} or \code{FALSE}. 
#'                  Return all inputs in a single wide data frame. 
#'                  If \code{FALSE}, return 4 data frames in a list:
#'                      (1) control 
#'                      (2) sources
#'                      (3) buildings
#'                      (4) surface
#' @param add_to_envir \code{TRUE} or \code{FALSE}. 
#'                     Exports tables directly to the Global Environment.
#' @param control Name for control options data frame added to Global Environment. 
#'                Default is "sources". Ignored if \code{as_one_df} is \code{TRUE}.
#' @param sources Name for emission source data frame added to Global Environment. 
#'                Default is "sources". Ignored if \code{as_one_df} is \code{TRUE}.
#' @param buildings Name for buildings data frame added to Global Environment. 
#'                  Default is "buildings". Ignored if \code{as_one_df} is \code{TRUE}.
#' @param surface Name for surface characteristics data frame added to Global Environment. 
#'                Default is "surface". Ignored if \code{as_one_df} is \code{TRUE}.
#' @keywords aerscreen input new tables
#' @export
#' @examples
#' input_tbl <- new_aerscreen(as_one_df = TRUE)
#' 
#' new_aerscreen(as_one_df = FALSE, add_to_envir = TRUE, surface = "surface_chars")
#' 
#' input_list <- new_aerscreen(as_one_df = FALSE)
# 
#
new_aerscreen <- function(aerscreen     = "aerscreen_inp",
                          as_one_df     = TRUE,
                          add_to_envir  = FALSE,
                          control       = "control",
                          sources       = "sources",
                          buildings     = "buildings",
                          surface       = "surface") {
  
  # Create tables
  co <- tibble::tibble(near_receptor   = 1,
                       far_receptor    = 500,
                       flagpole_height = as.numeric(NA),
                       debug_opt       = "N")
  
  so <- tibble::tibble(emit_gs       = as.numeric(NA),
                       height_m      = as.numeric(NA),
                       temp_k        = as.numeric(NA),
                       velocity_ms   = as.numeric(NA),
                       diameter_m    = as.numeric(NA),
                       urban_pop     = as.numeric(NA))
  
  bu <- tibble::tibble(bld_height          = as.numeric(NA),
                       width_x             = as.numeric(NA),
                       length_y            = as.numeric(NA),
                       bld_rotation        = as.numeric(NA),
                       dist_from_source    = as.numeric(NA),
                       angle_from_source   = as.numeric(NA))
  
  su <- surface_df() 
  
  # Output
  if(as_one_df) {
    
    aerscreen_inp <- tibble::as_data_frame(cbind(co, so, bu, su))
    
    if(add_to_envir) assign(aerscreen, aerscreen_inp, pos = 1)
    
  } else {
    
    if(add_to_envir) {
      
      # 1 - Control
      assign(control, co, pos = 1)
      
      # 2 - Sources
      assign(sources, so, pos = 1)
      
      # 3- Buildings
      assign(buildings, bu, pos = 1)
      
      # 4- Surface characteristics
      assign(surface, su, pos = 1)
      
    }
    
    aerscreen_inp <- list(control    = co,
                          sources    = so,
                          buildings  = bu,
                          surface    = su)
    
    names(aerscreen_inp) <- c(control, sources, buildings, surface)
  }
  
  return(aerscreen_inp)
  
}