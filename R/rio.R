#' @name first_nonblank_line
#' @title first_nonblank_line
#'  Return first non-blank 80 character line in file
#' @param f file connection to file the you want to read
#' @param max_lines, INTEGER, maximum number of blank lines
#' @return FIRST NON BLANK LINE IN RINEX
#'
#' @examples
#' ##
first_nonblank_line <- function(f, max_lines = 10) {
  line <- ""
  
  if (max_lines < 1) {
    stop("must read at least one line")
  }
  found <- FALSE
  for (i in 1:max_lines) {
    line <- substr(readLines(f, i), 1, 81)
    if (nchar(trimws(line)) > 2) {
      found <- TRUE
      break
    }
  }
  
  if (!found) {
    warning("could not find first valid header line in file ", f)
    return(NULL)
  }
  
  return(line)
  
}

#' @name rinex_version
#' @title rinex_version
#'  Return RINEX version
#' @param s string with first non blank line of  RINEX file
#' @return  A list with information on some examples RINEX files.
#' \itemize{
#'   \item version, numeric ,  RINEX/SP3 file version
#'   \item is_crinex, boolean, is it a Compressed RINEX CRINEX Hatanaka file
#' }
#' @examples
#' ##
rinex_version <- function(s) {
  if (!is.character(s)) {
    message("Need input as string of RINEX/SP3 file")
    return(NULL)
  }
  if (nchar(s) < 2) {
    message("Cannot decode RINEX/SP3 version from line:\n", s)
    return(NULL)
  }
  # %% .sp3 file
  if (substr(s, 1, 1) == "#") {
    supported_versions <- c("a", "c", "d")
    
    if (!is.element(substr(s, 2, 2), supported_versions)) {
      message("SP3 versions of SP3 files currently handled: ",
              supported_versions)
      return(NULL)
    }
    return(list(version = paste0("sp3", substr(s, 2, 2)), is_crinex = FALSE))
  }
  
  
  
  # %% typical RINEX files
  if (nchar(s) >= 80) {
    if (!is.element(substr(s, 61, 80),
                    c("RINEX VERSION / TYPE", "CRINEX VERS   / TYPE"))) {
      message("The first line of the RINEX file header is corrupted.")
      return(NULL)
    }
    
    
    vers <- suppressWarnings((as.numeric(
      scan(
        text = s,
        what = "numeric",
        strip.white = TRUE,
        quiet = TRUE
      )
    )))
    
    vers <- vers[!is.na(vers)]
    
    if (length(vers) != 1) {
      message("Could not determine file version from line ", s)
    }
    
    is_crinex <- substr(s, 21, 40) == "COMPACT RINEX FORMAT"
    
    return(list(version = vers, is_crinex = is_crinex))
  }
}




#' @name rinexinfo
#' @title rinexinfo
#'  Return RINEX version
#' @param f file connection to RINEX file (not zip/tar/gz/z compressed)
#' @return  A list with information on some examples RINEX files.
#' @export
#' @examples
#' ##
rinexinfo <- function(f) {
  line <- first_nonblank_line(f)  # don't choke on binary files
  if(is.null(line)){
    return(NULL)
  }
  
  if (is.element(substr(line, 1, 2), c("#a", "#c", "#d"))) {
    return(list("version" = substr(line, 1, 2), "rinextype" = "sp3"))
  }
  version <- rinex_version(line)
  file_type <- substr(line, 21, 21)
  if (as.integer(version$version) == 2) {
    if (file_type == "N") {
      system <- "G"
    }
    else if (file_type == "G") {
      system <- "R"
    }
    else if (file_type == "E") {
      system <- "E"
    } else {
      system <- substr(line, 41, 41)
    }
  } else {
    system <- substr(line, 41, 41)
  }
  
  if (is.element(file_type, c("O", "C"))) {
    rinex_type <- "obs"
  } else if (file_type == "N" || substr(line, 21, 23) == "NAV") {
    rinex_type = "nav"
  } else{
    rinex_type = file_type
  }
  
  info <- append(version,
                 list(
                   "filetype" = file_type,
                   "rinextype" = rinex_type,
                   "systems" = system
                 ))
  return(info)
}


#' @name opener
#' @title  Decompress Hatanaka-compressed RINEX format
#' @description Decompress Hatanaka-compressed RINEX format.

#'
#' @param f path to file that is RINEX format
#' @param header boolean,  DEFAULT FALSE  read also head in  RINEX format
#' @param verbose boolean, DEFAULT FALSE  to output extra messages
#' @return  A connection in read-only mode to RINEX file
#' @export
#'
#' @examples
#' ##
opener <- function(f = NULL, header = FALSE, verbose=FALSE) {
  # if(is.character(f)){
  #   return(f)
  # }
  
  if (is.null(f) || !file.exists(f)) {
    stop("File ", f," does not exist")
  }
  
  finf.st_size <- file.size(f)
  if (finf.st_size > 100E6) {
    message("opening ", finf.st_size / 1E6 , " MByte in file ", f)
  }
  filepaths <- decompress(f, verbose=FALSE)
  info <- rinexinfo(filepaths[[1]])
  if(info$is_crinex){
    filepaths <- crx2rnx(filepaths[[1]])
    if(verbose) message("Is Hatanaka compressed, uncompressing")
    info <- rinexinfo(filepaths[[1]])
  } 
  
  if(verbose) message( sprintf("%s=%s  - ", paste(names(info)), paste(info) ) )
  
  con<-tryCatch( file(filepaths[[1]], "rb"), error=function(e){
    message(e)
  })
  con
}
