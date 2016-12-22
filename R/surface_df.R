#' Surface characteristics
#'
#' Create a data frame of AERSCREEN surface characteristics used by MAKEMET.
#' @param min_temp_k Minimum surface temperature in Kelvin. Defaults to \code{235}.
#' @param max_temp_k Maximum surface temperature in Kelvin. Defaults to \code{315}.
#' @param min_wind_speed Minimum wind speed in units of \code{m/s}. 
#'                       Requires a value of \code{0.5} or greater. 
#'                       Defaults to \code{0.5}.
#' @param anem_height Anemometer height in meters. Defaults to \code{10}.
#' @param surface_profile Dominant surface characteristics profile. Accepts integers between [0, 3].
#'                     Enter \code{0} if providing site specific albedo, Bowen ratio and surface roughness.
#'                     Defaults to \code{7}. Options: 
#'                     (0) None. User specified characteristics will be used.
#'                     (1) Water
#'                     (2) Deciduous forest
#'                     (3) Coniferous forest
#'                     (4) Swamp
#'                     (5) Cultivated land
#'                     (6) Grassland
#'                     (7) Urban
#'                     (8) Desert shrubland
#' @param climate_profile Dominant climate moisture profile. Accepts integers between [0, 3]. 
#'                        Enter \code{0} if providing site specific albedo, Bowen ratio and surface roughness. 
#'                        Defaults to \code{1}. Options: 
#'                        (0) None. User specified characteristics will be used. 
#'                        (1) Average moisture
#'                        (2) Wet conditions
#'                        (3) Dry conditions
#' @param albedo Albedo. The average proportion of incident light reflected at noontime.
#'               Accepts values between [0, 1].
#'               Ignored by AERSCREEN unless \code{surface_profile} and \code{climate_profile} are set to \code{0}.
#' @param bowen Bowen ratio. The average midday ratio of the sensible heat flux (\code{H}) 
#'              to the evaporative heat flux (\code{E}). 
#'              The greater the value of \code{H/E} the dryer the conditions.
#'              Accepts values between [-10 and 10].
#'              Ignored by AERSCREEN unless \code{surface_profile} and \code{climate_profile} are set to \code{0}.
#' @param z_length Surface roughness length. 
#'                 The height at  which the  mean  horizontal  wind  speed  approaches  zero. 
#'                 Accepts values between [0.001 and 2].
#'                 Ignored by AERSCREEN unless \code{surface_profile} and \code{climate_profile} are set to \code{0}.
#' @keywords surface meteorology weather aerscreen input
#' @export
#' @examples
#' surface_df()
# 
#
surface_df <- function(min_temp_k       = 235,
                       max_temp_k       = 315,
                       min_wind_speed   = 0.5,
                       anem_height      = 10,
                       surface_profile  = 7,
                       climate_profile  = 1,
                       albedo           = as.numeric(NA),
                       bowen            = as.numeric(NA),
                       z_length         = as.numeric(NA)
) {
  
  # Value checks
  if(min_wind_speed < 0.5) stop("'min_wind_ms' must be at least 0.5.")
  
  if(!surface_profile %in% 0:8) stop("'surface_profile' must be an integer between [0 and 8].")
  
  if(!climate_profile %in% 0:3) stop("'climate_profile' must be an integer between [0 and 3].")
  
  if(!is.null(albedo) && !is.na(albedo) && (albedo < 0 || albedo > 1)) stop("'albedo' must be a value between [0 and 1].")
  
  if(!is.null(bowen) && !is.na(bowen) && (bowen < -10 || bowen > 10)) stop("'bowen' must be a value between [-10 and 10].")
     
  if(!is.null(z_length) && !is.na(z_length) && (z_length < 0.001 || z_length > 2)) stop("'z_length' must be a value between [0.001 and 2].")
  
  
  # Assign data frame
  df <- tibble::tibble(min_temp_k       = min_temp_k,
                       max_temp_k       = max_temp_k,
                       min_wind_speed   = min_wind_speed,
                       anem_height      = anem_height,
                       surface_profile  = surface_profile,
                       climate_profile  = climate_profile,
                       albedo           = albedo,
                       bowen            = bowen,
                       z_length         = z_length
                       )
  return(df)
}

##
