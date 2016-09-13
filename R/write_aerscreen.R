#' Save a data frame to an input text file for the AERSCREEN model.
#'
#' Save a data frame to an input text file for the AERSCREEN model.
#' @param x A data frame to write to disk
#' @param path Path to write to.
#' @keywords aerscreen aermod write save
#' @export
#' @examples
#' write_aerscreen(x = aerscreen_inp, path = "Inputs/aerscreen.inp")
# 
#


write_aerscreen <- function(x, path) {
  
  if(is.null(x)) stop('argument "x" is missing')
  
  if(is.na(x)) stop('argument "x" is missing')
  
  if(nrow(x) < 1) stop("The data frame is empty.")
  
  
  writeLines(x, path)
  
  
}