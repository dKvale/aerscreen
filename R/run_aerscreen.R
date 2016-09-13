#' Run AERSCREEN.exe with designated input file
#'
#' Run AERSCREEN.exe with designated input file.
#' @param input  AERSCREEN input data.frame. Defaults to aerscreen_inp. If a file path is used (e.g. "inputs/aerscreen.inp"), the model will be directed to use the input file. 
#' @param output  File name for model output. Defaults to "aerscreen_results".
#' @param exe_folder  aerscreen.exe location. Defaults to "aerscreen/". 
#' @keywords aerscreen aermod run model
#' @export
#' @examples
#' run_aerscreen(input = "inputs/aerscreen.inp", output = "output_test1")
# 
#

run_aerscreen <- function(input      = aerscreen_inp, 
                          output     = "aerscreen_results",
                          exe_folder = "aerscreen") {
  
 # Check for aerscreen.exe in exe_folder
  check_aerscreen <- "aerscreen.exe" %in% tolower(list.files(exe_folder))
  
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
  #shell(paste0(relocate, ' & COPY aerscreen.out "', paste0('"', getwd(), '/', output, '.out"')))
  
  
  # Read modeling results
  if(!grepl("[.]out", output)) output <- paste0(output, ".out")

  invisible(readLines(paste0(exe_folder, "/", output)))
  
}