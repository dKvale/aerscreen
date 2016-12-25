#' Run AERSCREEN
#'
#' Run aerscreen.exe on input file.
#' @param input An AERSCREEN input file or file path. Defaults to "aerscreen.inp". 
#'              File is written to \code{exe_folder} as "aerscreen.inp". 
#' @param results_file File name for saving model output. 
#'                     If blank, the designated OUT file in the input file will be used.
#' @param exe_folder Folder path containing \code{aerscreen.exe}. 
#' @keywords aerscreen run dispersion model
#' @export
#' @examples
#' \dontrun{
#' run_aerscreen(input      = "inputs/aerscreen.inp", 
#'               out_file   = "output_test1",
#'               exe_folder = "EPA")
#' }
# 
#
run_aerscreen <- function(input          = "aerscreen.inp", 
                          out_file       = as.character(NA),
                          exe_folder     = "aerscreen") {
  
 # Check for required EPA models in exe_folder
  exe_check <- function(file, folder) {
    
    #folder <- paste0(getwd(), "/", folder, "/")
    
    if(!file %in% tolower(list.files(folder))) {
      stop(paste0(file, " not found in ", folder))
    }
  }
  
  sapply(c("aerscreen.exe", "aermod.exe", "makemet.exe", "bpipprm.exe"), function(x) exe_check(x, exe_folder))
  
  # Copy input file to exe_folder
  if(!is_valid(input, 1)) stop('"input" file not found')
  
  # Update out file for results
  if(!grepl("[.]out", out_file)) out_file <- paste0(out_file, ".out")
  
  if("character" %in% class(input)) {
    
      if("\n" %in% input) writeLines(input, paste0(exe_folder, "//aerscreen.inp"))
      else writeLines(readLines(input), paste0(exe_folder, "//aerscreen.inp"))
     
      aer_input <- readLines(paste0(exe_folder, "//aerscreen.inp"))
      
      aer_input[grepl("OUTPUT FILE", aer_input)] <- paste0("** OUTPUT FILE \"", out_file, "\" \n")
      
      writeLines(aer_input, paste0(exe_folder, "//aerscreen.inp"))
    
  } else if("data.frame" %in% class(input)) {
    
      write_aerscreen(input, paste0(exe_folder, "//aerscreen.inp"), out_file = out_file) 
    
  } else {
    
      stop(paste0('The entered "input" is of class ', class(input), '. \n 
                  The "input" should be a text string, a path to an input file, or a data frame.'))
    
    }
    
  # Write prompts.txt file
  writeLines("y \n \n", paste0(exe_folder, "/prompts.txt"))  
    
  
  # Write AERSCREEN batch run file
  writeLines(paste("ECHO", "aerscreen.exe < prompts.txt", sep = "\n"), 
             paste0(exe_folder, "/aerscreen_run.bat"))  
  
    
  # Shell command to run AERSCREEN
  relocate <- paste0(substring(getwd(), 1, 2), " & CD ", paste0('"', getwd(), '/', exe_folder, '"'))
  
  shell(paste(relocate, "& aerscreen_run.bat"))
  
  
  # Copy output file to designated file location
  shell(paste0(relocate, ' & COPY "', out_file, '" ', paste0('"', getwd(), '/', out_file)))
  
  # Read modeling results
  return(readLines(out_file))
  
}