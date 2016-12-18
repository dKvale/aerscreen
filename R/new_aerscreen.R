#' Create AERSCREEN input tables
#'
#' Create default AERSCREEN input data frames: 
#' (1) Control options 
#' (2) Source parameters
#' (3) Receptor parameters
#' (4) Surface parameters
#' (5) Output options
#' @param as_one_df \code{TRUE} or \code{FALSE}. 
#'                  Return all inputs in a single wide data frame. 
#'                  If \code{FALSE}, 5 data frames are returned in a list: 
#'                      (1) control
#'                      (2) sources
#'                      (3) receptors
#'                      (4) surface
#'                      (5) output
#' @param add_to_envir \code{TRUE} or \code{FALSE}. 
#'                     Exports tables directly to the Global Environment.
#' @param input_df Name for the joined input data frame added to Global Environment. Default is "aermod_inp". Ignored if \code{as_one_df} is \code{FALSE}. 
#' @param control Name for control options data frame added to Global Environment. Default is "control". Ignored if \code{as_one_df} is \code{TRUE}. 
#' @param sources Name for emission source data frame added to Global Environment. Default is "sources". Ignored if \code{as_one_df} is \code{TRUE}.
#' @param receptors Name for receptor data frame added to Global Environment. Default is "receptors". Ignored if \code{as_one_df} is \code{TRUE}.
#' @param surf Name for surface characteristics data frame added to Global Environment. Default is "surf". Ignored if \code{as_one_df} is \code{TRUE}.
#' @param out Name for output options data frame added to Global Environment. Default is "output". Ignored if \code{as_one_df} is \code{TRUE}.
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
                          surf          = "surf",
                          out           = "out",
                          add_to_envir  = FALSE) {
  
  co <- control_tbl(model_opt = c("CONC", "SCREEN",  "FLAT"),
                    avg_time  = 1)
  
  so <- source_tbl()
  
  re <- receptor_tbl() 
  
  su <- surf_tbl() 
  
  ou <- out_tbl()
  
  if(as_one_df) {
    
    aerscreen_inp <- cbind(control_tbl(), 
                           source_tbl(), 
                           receptor_tbl(), 
                           surf_tbl(), 
                           out_tbl())
    
    if(add_to_envir) assign(input_df, aerscreen_inp, pos = 1)
    
  } else {
    
    if(add_to_envir) {
      
      # 1 - CONTROL OPTIONS
      assign(control, control_tbl(), pos = 1)
      
      # 2 - SOURCES
      assign(sources, source_tbl(), pos = 1)
      
      # 3- RECEPTORS
      assign(receptors, receptor_tbl(), pos = 1)
      
      # 4- SURFACE CHARACTERISTICS
      assign(surf, surf_tbl(), pos = 1)
      
      # 5- OUTPUT OPTIONS
      assign(out, out_tbl(), pos = 1)
    }
    
    aerscreen_inp <- list(control   = control_tbl(),
                         sources    = source_tbl(),
                         receptors  = receptor_tbl(),
                         surf       = surf_tbl(),
                         out        = out_tbl())
    
    names(aerscreen_inp) <- c(control, sources, receptors, surf, out)
  }
  
  return(aerscreen_inp)
  
}