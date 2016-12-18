#' Create AERSCREEN input tables
#'
#' Create default AERSCREEN input data frames: 
#' (1) Control options 
#' (2) Source parameters
#' (3) Receptor parameters
#' (4) Surface parameters
#' (5) Meteorology options
#' (6) Output options
#' @param as_one_df \code{TRUE} or \code{FALSE}. 
#'                  Return all inputs in a single wide data frame. 
#'                  If \code{FALSE}, return 6 data frames in a list: 
#'                      (1) control
#'                      (2) sources
#'                      (3) receptors
#'                      (4) surface
#'                      (5) meteorology
#'                      (6) output
#' @param add_to_envir \code{TRUE} or \code{FALSE}. 
#'                     Exports tables directly to the Global Environment.
#' @param input_df Name for the joined input data frame added to Global Environment. 
#'                 Default is "aermod_inp". Ignored if \code{as_one_df} is \code{FALSE}. 
#' @param control Name for control options data frame added to Global Environment. 
#'                Default is "control". Ignored if \code{as_one_df} is \code{TRUE}. 
#' @param sources Name for emission source data frame added to Global Environment. 
#'                Default is "sources". Ignored if \code{as_one_df} is \code{TRUE}.
#' @param receptors Name for receptor data frame added to Global Environment. 
#'                  Default is "receptors". Ignored if \code{as_one_df} is \code{TRUE}.
#' @param surface Name for surface characteristics data frame added to Global Environment. 
#'                Default is "surface". Ignored if \code{as_one_df} is \code{TRUE}.
#' @param met Name for meteorology options data frame added to Global Environment. 
#'            Default is "met". Ignored if \code{as_one_df} is \code{TRUE}.
#' @param out Name for output options data frame added to Global Environment. 
#'            Default is "output". Ignored if \code{as_one_df} is \code{TRUE}.
#' @keywords aerscreen input new start tables
#' @export
#' @examples
#' input_tbl <- new_aerscreen(as_one_df = TRUE)
#' 
#' new_aerscreen(as_one_df = FALSE, add_to_envir = TRUE, surf = "surface_chars")
#' 
#' input_list <- new_aerscreen(as_one_df = FALSE)
# 
#


new_aerscreen <- function(input_df      = "aerscreen_inp",
                          as_one_df     = TRUE,
                          control       = "control",
                          sources       = "sources",
                          receptors     = "receptors",
                          surface       = "surface",
                          met           = "met",
                          out           = "out",
                          add_to_envir  = FALSE) {
  
  co <- control_tbl(model_opt = c("CONC", "SCREEN",  "FLAT"),
                    avg_time  = 1)
  
  so <- source_tbl(group_id = "ALL")
  
  re <- receptor_tbl() 
  
  su <- surface_tbl() 
  
  me <- met_tbl(surf_file      = "aerscreen_01_01.sfc  FREE",
                surf_site_info = "11111   2010  SCREEN",
                prof_file      = "aerscreen_01_01.pfl  FREE",                                                                                                                                                                                                                    
                upper_air_info = "22222   2010  SCREEN",                                                                                                                                                                                                                         
                base_elev_m    =  "0.0")
  
  ou <- out_tbl(rect_table = c("1", "FIRST"),
                max_table  = c("ALLAVE", "50"),
                file_form  = "EXP",
                rank_file  = c("1", "10", "AERSCREEN.FIL"),
                plot_file  = c("1", "ALL", "FIRST", "AERSCREEN.PLT"))
  
  if(as_one_df) {
    
    aerscreen_inp <- cbind(co, so, re, su, me, ou)
    
    if(add_to_envir) assign(input_df, aerscreen_inp, pos = 1)
    
  } else {
    
    if(add_to_envir) {
      
      # 1 - CONTROL OPTIONS
      assign(control, co, pos = 1)
      
      # 2 - SOURCES
      assign(sources, so, pos = 1)
      
      # 3- RECEPTORS
      assign(receptors, re, pos = 1)
      
      # 4- SURFACE CHARACTERISTICS
      assign(surf, su, pos = 1)
      
      # 5- METEOROLOGY OPTIONS
      assign(met, me, pos = 1)
      
      # 5- OUTPUT OPTIONS
      assign(out, ou, pos = 1)
    }
    
    aerscreen_inp <- list(control    = co,
                          sources    = so,
                          receptors  = re,
                          surface    = su,
                          met        = me,
                          out        = ou)
    
    names(aerscreen_inp) <- c(control, sources, receptors, surface, met, out)
  }
  
  return(aerscreen_inp)
  
}