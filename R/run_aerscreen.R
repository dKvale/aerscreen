#' Run AERSCREEN
#'
#' Run aerscreen.exe with designated input file.
#' @param input  AERSCREEN input file. Defaults to aerscreen_inp. A file path may be used (ex. "aerscreen.inp"). 
#' @param results_file File name for saving model output. 
#'                     If blank, the designated OUT file in the input file will be used.
#' @param exe_folder Folder containing \code{aerscreen.exe}. 
#' @keywords aerscreen run dispersion model
#' @export
#' @examples
#' #run_aerscreen(input    = "inputs/aerscreen.inp", 
#'  #              out_file = "output_test1")
# 
#
run_aerscreen <- function(input          = "aerscreen.inp", 
                          results_file   = as.character(NA),
                          exe_folder     = "aerscreen/") {
  
 # Check for required EPA models in exe_folder
  exe_check <- function(file, folder) {
    
    if(!file %in% tolower(list.files(folder))) stop(paste(file, "not found in", folder))
    
  }
  
  sapply(c("aerscreen.exe", "aermod.exe", "makemet.exe", "bpipprm.exe"), function(x) exe_check(x, exe_folder))
  
  
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