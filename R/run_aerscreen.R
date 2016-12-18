#' Run AERSCREEN
#'
#' Run aerscreen.exe with designated input file.
#' @param input  AERSCREEN input file. Defaults to aerscreen_inp. A file path may be used (ex. "aerscreen.inp"). 
#' @param out_file File name for saving model output. 
#'                Defaults to value of \code{title} in the provided input file.
#' @param exe_folder Folder containing \code{aerscreen.exe}. 
#' @keywords aerscreen run dispersion model
#' @export
#' @examples
#' run_aerscreen(input    = "inputs/aerscreen.inp", 
#'               out_file = "output_test1")
# 
#

run_aerscreen <- function(input      = "aerscreen.inp", 
                          out_file   = input$TITLEONE,
                          exe_folder = "aerscreen/") {
  
 # Check for required EPA models in exe_folder
  check_aerscreen <- "aerscreen.exe" %in% tolower(list.files(exe_folder))
  
  check_aermod    <- "aermod.exe" %in% tolower(list.files(exe_folder))
  
  check_makemet   <- "makemet.exe" %in% tolower(list.files(exe_folder))
  
  check_bpip      <- "bpipprm .exe" %in% tolower(list.files(exe_folder))
  
  if(!check_aerscreen) {
    warning("aerscreen.exe was not found in ", exe_folder)
    stop()
  }
  
  # Check for aermod.exe in exe_folder
  check_aermod <- "aerscreen.exe" %in% tolower(list.files(exe_folder))
  
  if(!check_aermod) {
    warning("aermod.exe was not found in ", exe_folder)
    stop()
  }
  
  # Copy input file to folder
  if(is.null(input)) stop('"input" is missing')
  
  if(is.na(input)) stop('"input" is missing')
  
  if(class(input) == "character") {
    
      writeLines(readLines(input), paste0(exe_folder, "/aerscreen.inp"))
    
  } else if("data.frame" %in% class(input)) {
    
      #write_aerscreen(input, paste0(exe_folder, "/aerscreen.inp")) 
     
      writeLines(print(input), paste0(exe_folder, "/aerscreen.inp"))
    
  } else {
    
      stop(paste0('The entered "input" is ', class(input), '. The "input" must be a character string or data.frame.'))
    
    }
    
  # Write prompts.txt file
  writeLines("y \n ", paste0(exe_folder, "/prompts.txt"))  
    
  
  # Write AERSCREEN batch file
  writeLines(paste("ECHO", "aerscreen.exe < prompts.txt", sep = "\n"), 
             paste0(exe_folder, "/aerscreen_run.bat"))  
  
    
  # Shell command to run AERSCREEN
  relocate <- paste0("CD ", substring(getwd(), 1, 2), " & CD ", paste0('"', getwd(), '/', exe_folder, '"'))
  
  shell(paste(relocate, "& aerscreen_run.bat"))
  
  
  # Copy output file to designated file
  #shell(paste0(relocate, ' & COPY aerscreen.out "', paste0('"', getwd(), '/', out_file, '.out"')))
  
  
  # Read modeling results
  if(!grepl("[.]out", out_file)) out_file <- paste0(out_file, ".out")

  invisible(readLines(paste0(exe_folder, "/", out_file)))
  
}