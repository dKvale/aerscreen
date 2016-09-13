#' Download AERSCREEN.exe from EPA
#'
#' Download AERSCREEN.exe from EPA. Option to install AERMAP, AERMOD, BPIPPRM, and MAKEMET.
#' @param folder  Download location. Default is the current working directory.
#' @param aermap  Download AERMAP? A logical value indicating whether to download EPA's terrain preprocessor AERMAP.
#' @param bpip  Download BPIPPRM? A logical value indicating whether to download EPA's downwash calculator BPIPPRM.
#' @param makemet  Download MAKEMET? A logical value indicating whether to download EPA's simplified meteorology preprocessor MAKEMET.
#' @keywords download install aerscreen aermod bpip makemet
#' @export
#' @examples
#' install_aerscreen(folder = "my_facility")
# 
#

install_aerscreen <- function(folder  = getwd(),
                              aermap  = TRUE,
                              aermod  = TRUE,
                              bpip    = TRUE,
                              makemet = TRUE) {
 
  setwd(folder)
  
  tf <- tempfile("aerscreen", fileext = ".zip")
  
  
  download.file("https://www3.epa.gov/ttn/scram/models/screen/aerscreen_code.zip", tf)
  
  unzip(tf, exdir = "aerscreen")
  
  if(aermap){
    tf <- tempfile("aermap", fileext = ".zip")
    
    download.file("https://www3.epa.gov/ttn/scram/models/aermod/aermap/aermap_exe.zip", tf)
    
    unzip(tf, exdir = "aerscreen")
  }
  
  if(aermod){
    tf <- tempfile("aermod", fileext = ".zip")
  
    download.file("http://www.epa.gov/ttn/scram/models/aermod/aermod_exe.zip", tf)
  
    unzip(tf, exdir = "aerscreen")
  }
  
  if(bpip){
    tf <- tempfile("bpip", fileext = ".zip")
    
    download.file("https://www3.epa.gov/ttn/scram/models/relat/bpipprime.zip", tf)
    
    unzip(tf, exdir = "aerscreen")
  }
  
  if(makemet){
    tf <- tempfile("makemet", fileext = ".zip")
    
    download.file("https://www3.epa.gov/ttn/scram/models/screen/makemet_code.zip", tf)
    
    unzip(tf, exdir = "aerscreen")
  }
  
  
}