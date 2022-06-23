#' Write a data frame to an AERSCREEN input file
#'
#' Output an AERSCREEN input file from a data frame of AERSCREEN parameters.
#' @param data Data frame of AERSCREEN modeling parameters.
#' @param path Path to write input file to. If blank, returns a character string.
#'             Defaults to "aerscreen.inp".
#' @param out_file File to save AERSCREEN modeling results to. 
#'                 Defaults to "aerscreen.out".           
#' @param control Data frame of control parameters. If blank, control parameters from \code{data} are used. 
#' @param sources Data frame of source parameters. If blank, source parameters from \code{data} are used.
#' @param buildings Data frame of building parameters for BPIP. If blank, building parameters from \code{data} are used.
#' @param surface Data frame of surface characteristics. If blank, surface characteristics from \code{data} are used. 
#' @keywords aerscreen write save input
#' @export
#' @examples
#' aerscreen_inp <- new_aerscreen()
#' 
#' write_aerscreen(data = aerscreen_inp, path = "aerscreen.inp")
# 
# 
write_aerscreen <- function(data            = NULL,
                            path            = NULL,
                            out_file        = "aerscreen.out",
                            control         = NULL,
                            sources         = NULL,
                            buildings       = NULL,
                            surface         = NULL
                          
) {
  
  # Data tests    
  if((is.null(data) || nrow(data) < 1) & (is.null(sources) || nrow(sources) < 1)) {
    stop("Data frame is empty. AERMOD requires at least 1 emission source.")
  }
  
  ## Use pathway specific tables if provided
  if(is.null(control) || !is.data.frame(control) || nrow(control) < 1) {
      co <- data
  } else {
      co <- control
    }
  
  if(is.null(sources) || !is.data.frame(sources) || nrow(sources) < 1) {
      so <- data
  } else {
      so <- sources
    }
  
  if(is.null(buildings) || !is.data.frame(buildings) || nrow(buildings) < 1) {
      bu <- data[1, ]
  } else {
      bu <- buildings[1, ]
    }

  if(is.null(surface) || !is.data.frame(surface) || nrow(surface) < 1) {
      su <- data[1, ]
  } else {
      su <- surface[1, ]
    }
  
  # Check for building
  #bld_cols <- c("bld_height", "width_x", "length_y", "bld_rotation", "angle_from_source", "dist_from_source")
  #bpip_run <- ifelse(sum(sapply(unlist(bu[ , bld_cols]), function(x) aermod::is_valid(x, 1))) > 5, "Y", "N")
  
  ## Check if urban population at least 10,000
  if(aermod::is_valid(so$urban_pop) && so$urban_pop < 10000) {
    
    so$urban_pop <- NA
    
    warning("FYI: The source is assigned an 'urban_pop' value below 100, it will be modeled using rural dispersion coefficients.")
  }
  
  # Replace `NA` values with Zeros to avoid printing 'NA'
  co[is.na(co)] <- 0
  
  so[is.na(so)] <- 0 
  
  bu[is.na(bu)] <- 0
  
  su[is.na(su)] <- 0  
  
  # Create text file
  
  ## AERSCREEN header
  inp_text <- paste0("**\n",
                     "** AERSCREEN input file\n",
                     "** Created: ", format(Sys.Date(), "%m/%d/%Y"), "\n",
                     "**\n\n")  
  
  ## STACK section
  inp_text <- paste0(inp_text, 
                     "** STACK DATA         Rate    Height     Temp.  Velocity     Diam.     Flow\n",
                     "**              ",
                     receptors::fw(so$emit_gs, 13),
                     receptors::fw(so$height_m, 9),
                     receptors::fw(so$temp_k, 11),
                     receptors::fw(so$velocity_ms, 11),
                     receptors::fw(so$diameter_m, 10),
                     "000000.\n\n", collapse = "") 

  ## BUILDING section  
  inp_text <- paste0(inp_text, 
                     "** BUILDING DATA   BPIP    Height  Max dim.  Min dim.   Orient.   Direct.    Offset\n",                                                                                                                                                                       
                     "**                  ",
                     receptors::fw(bu$bpip_run, 6), 
                     receptors::fw(as.numeric(bu$bld_height), 10),
                     receptors::fw(as.numeric(bu$long_side), 11),
                     receptors::fw(as.numeric(bu$short_side), 10),
                     receptors::fw(as.numeric(bu$bld_rotation), 10),
                     receptors::fw(as.numeric(bu$angle_from_source), 10),
                     as.numeric(bu$dist_from_source),
                     "\n\n", collapse = "")
  
                     #Y     10.0000   10.0000   10.0000   10.0000   10.0000   10.0000                                                                               
  
  ## SURFACE section 
  inp_text <- paste0(inp_text,
                     "** MAKEMET DATA    MinT    MaxT Speed   AnemHt Surf Clim  Albedo   Bowen  Length  SC FILE\n",                                                                                                                                                                 
                     "**               ",
                     receptors::fw(su$min_temp_k, 8),
                     receptors::fw(su$max_temp_k, 9),
                     receptors::fw(su$min_wind_speed, 6),
                     receptors::fw(su$anem_height, 10),
                     receptors::fw(su$surface_profile, 5),
                     receptors::fw(su$climate_profile, 4),
                     receptors::fw(su$albedo, 9), 
                     receptors::fw(su$bowen, 9),
                     receptors::fw(su$z_length, 8),
                     '"NA"',
                     "\n\n", collapse = "")
#235.00  315.00   0.5   10.000    0    0   0.1000   0.0100   1.0020  "NA"                                                                                                                                                                 
  
  
  ## ADJUST U* section 
  inp_text <- paste0(inp_text,
                     "** ADJUST U*      ",                                                                                                                                                                                                                                
                     co$adjust_ustar[1], 
                     "\n\n", collapse = "")
  
  ## TERRAIN section 
  inp_text <- paste0(inp_text,
                     "** TERRAIN DATA   Terrain    UTM East   UTM North  Zone  Nada     Probe     PROFBASE  Use AERMAP elev\n",                                                                                                                                                     
                     "**                   ",
                     "N            0.0         0.0     0     0        ",
                     receptors::fw(co$far_receptor[1], 16), 
                     "0.00         N",
                     "\n\n", collapse = "")
  
         
  ## RECEPTOR section 
  inp_text <- paste0(inp_text,
                     "** DISCRETE RECEPTORS  Discflag   Receptor file\n",                                                                                                                                                                                                          
                     "**                      ",
                     'N        "NA"', 
                     "\n\n", collapse = "")
     
  ## URBAN and FLAGPOLE section 
  inp_text <- paste0(inp_text,
                      "** UNITS/POPULATION   Units   R/U  Population      Amb. dist.   Flagpole    Flagpole height\n",                                                                                                                                                              
                      "**                      ",
                      "M     ",
                      receptors::fw(ifelse(so$urban_pop > 99, "U", "R"), 8),
                      receptors::fw(so$urban_pop, 17),
                      receptors::fw(co$near_receptor, 13),
                      receptors::fw(ifelse(co$flagpole_height > 0, "Y", "N"), 10),
                      co$flagpole_height,
                     "\n\n", collapse = "")
  
 ## FUMIGATION section 
 inp_text <- paste0(inp_text,
                    "** FUMIGATION        Inversion Break-up  Shoreline  Distance    Direct  Run AERSCREEN\n",                                                                                                                                                                    
                    "**                         ",
                    "N                  N         0.00      0.0     Y ",
                    "\n\n", collapse = "")
                                                                                                                                                                                     
  ## DEBUG section 
  inp_text <- paste0(inp_text,
                     "** DEBUG OPTION      Debug\n",                                                                                                                                                                                                                                
                     "**                     ",
                     co$debug_opt[1], 
                     "\n\n", collapse = "")                                                                                                                                                                                                                                  
  
  ## OUTPUT section 
  inp_text <- paste0(inp_text, 
                     '** OUTPUT FILE "', gsub("[.]out", "", out_file), '.out"',
                     "\n\n\n\n", collapse = "")
  
  ## TEMPORAL section 
  inp_text <- paste0(inp_text, "** Temporal sector: \n\n\n\n", collapse = "")                                                                                                                                                                                                                   
  
                     
  # Create AERMOD section
  aermod_inp <- aermod::write_aermod(data      = NULL, 
                                     path      = NULL,
                                     control   = aermod::control_df(title = gsub("[.]out", "", out_file)),
                                     sources   = aermod::source_df(),
                                     receptors = aermod::receptor_df(),
                                     met       = aermod::met_df(),
                                     out       = aermod::out_df())
                                       
  #out_df(rect_table = c("1", "FIRST"),
  #       max_table  = c("ALLAVE", "50"),
  #       file_form  = "EXP",
  #       rank_file  = c("1", "10", "AERSCREEN.FIL"),
  #       plot_file  = c("1", "ALL", "FIRST", "AERSCREEN.PLT"))
                             
  # Drop AERMOD header
  #aermod_inp <-  strsplit(aermod_inp, "\n")[[1]][-c(1:4)]
  
  # Combine AERMOD section & AERSCREEN section  
  inp_text   <- paste0(inp_text, aermod_inp, collapse = "")
  
  # Return results
 # cat("\nGenerated input file: \n\n")
 # invisible(writeLines(inp_text))
  
  if(!aermod::is_valid(path)) {
    
    return(inp_text)
    
  } else  {
    
    con <- file(path)
    
    writeLines(inp_text, con)
  
    close(con)
  }
}

