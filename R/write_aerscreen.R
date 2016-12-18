#' Write a data frame to an AERSCREEN input file
#'
#' Output an AERSCREEN input file from a data frame of AERSCREEN parameters.
#' @param data Data frame of AERSCREEN modeling parameters.
#' @param path Path to write to. Defaults to "aerscreen.inp".
#' @param control Data frame of control parameters. If blank, control parameters from \code{data} are used. 
#' @param sources Data frame of source parameters. If blank, source parameters from \code{data} are used.
#' @param receptors Data frame of receptor parameters. If blank, receptor parameters from \code{data} are used.
#' @param surface Data frame of surface characteristics. If blank, surface characteristics from \code{data} are used. 
#' @param met Data frame of meteorology parameters. If blank, meteorology parameters from \code{data} are used. 
#' @param out Data frame of output parameters. If blank, output parameters from \code{data} are used. 
#' @keywords aerscreen write save input
#' @importFrom receptors "fw"
#' @export
#' @examples
#' aerscreen_inp <- new_aerscreen()
#' 
#' write_aerscreen(data = aerscreen_inp, path = "aerscreen.inp")
# 
# 
write_aerscreen <- function(data      = NULL, 
                            path      = NULL,
                            control   = NULL,
                            sources   = NULL,
                            receptors = NULL,
                            surface   = NULL,
                            met       = NULL,
                            out       = NULL
) {
  
  # Data tests    
  if((is.null(data) || nrow(data) < 1) & (is.null(sources) || nrow(sources) < 1)) {
    stop("Data frame is empty. AERMOD requires at least 1 emission source.")
  }
  
  ## Use pathway specific tables if provided
  if(is.null(control) || !is.data.frame(control) || nrow(control) < 1) {co <- data[1, ]} else {co <- control[1, ]}
  
  if(is.null(sources) || !is.data.frame(sources) || nrow(sources) < 1) {so <- data} else {so <- sources}
  
  if(is.null(receptors) || !is.data.frame(receptors) || nrow(receptors) < 1) {re <- data[1, ]} else {re <- receptors[1, ]}
  
  if(is.null(met) || !is.data.frame(met) || nrow(met) < 1) {me <- data[1, ]} else {me <- met[1, ]}
  
  if(is.null(out) || !is.data.frame(out) || nrow(out) < 1) {ou <- data[1, ]} else {ou <- out[1, ]}
  
  
  # Replace `NA` values with blanks to avoid printing 'NA'
  co[is.na(co)] <- ""  
  
  so[is.na(so)] <- ""  
  
  re[is.na(re)] <- ""  
  
  me[is.na(me)] <- ""  
  
  ou[is.na(ou)] <- ""  
  
  
  # Create text file
  
  ## Header
  inp_text <- paste0("**\n",
                     "** AERMOD input file\n",
                     "** Created: ", format(Sys.Date(), "%m/%d/%Y"), "\n",
                     "**\n")  
  
  ## New section function
  new_section  <- function(section_code   = section, 
                           section_header = section_head,
                           comment_line   = paste(paste(rep("*", 40), collapse = ""), "\n**")) {
    
    paste0(comment_line, 
           " ", section_header, "\n", 
           comment_line, "\n",
           section_code, " STARTING \n")
  }
  
  ## Test length function
  is_min_length <- function(x, length = 1) {
    
    if(is.null(x) || is.na(x)) return(FALSE) 
    
    if(is.character(x)) return(nchar(x) >= length)
    
    if(is.data.frame(x)) return(nrow(x) >= length)
  }
  
  # Create AERMOD section
  aermod_inp <- write_aermod(data      = data, 
                             path      = NULL,
                             control   = control,
                             sources   = sources,
                             receptors = receptors,
                             met       = met,
                             out       = out)
  
  # Drop AERMOD header
  aermod_inp <-  strsplit(aermod_inp, "\n")[[1]][-c(1:4)]
  
  # Combine AERMOD section & AERSCREEN section  
  inp_text   <- paste0(inp_text, aermod_inp)
  
  # Return results
  cat("\nGenerated input file: \n\n")
  invisible(writeLines(inp_text))
  
  if(!is_min_length(path)) {
    
    return(inp_text)
    
  } else  writeLines(inp_text, path)
  
}

