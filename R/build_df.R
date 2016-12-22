#' Build a data frame of building parameters
#'
#' Create an input table of building parameters for AERSCREEN.
#' @param run_bpip Building ID or name.
#' @param height Height of building in meters. 
#' @param width_x Width of building from East to West in meters. 
#' @param length_y Length of building from North to South in meters. 
#' @param bld_rotation Building rotation from North in degrees. 
#' @param dist_from_source Distance from center of source to center of building in meters. 
#' @param angle_from_source Angle between center of building and center of source relative to North.
#'                          Units in degrees. 
#'                          Straight North is "0" degrees.
#'                          Straight East is "90" degrees.  
#' @keywords building bpip input aerscreen
#' @export
#' @examples
#' build_df(run_bpip = "Y")
# 
# 
build_df <- function(run_bpip           = "N",
                     height             = as.numeric(NA),
                     width_x            = as.numeric(NA),
                     length_y           = as.numeric(NA),
                     bld_rotation       = as.numeric(NA),
                     dist_from_source   = as.numeric(NA),
                     angle_from_source  = as.numeric(NA)) {

df <- tibble::tibble(run_bpip            = run_bpip,
                     height              = height,
                     width_x             = width_x,
                     length_y            = length_y,
                     bld_rotation        = bld_rotation,
                     dist_from_source    = dist_from_source,
                     angle_from_source   = angle_from_source)

return(df)
}

##