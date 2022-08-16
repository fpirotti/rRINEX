#' @name first_nonblank_line
#' @title first_nonblank_line
#'  Return first non-blank 80 character line in file
#' @param f full path to rinex file
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
  
  nl<-fpeek::peek_count_lines(f)
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
                   "systems" = system,
                   "filepath" = f,
                   "nLines" = nl
                 ))
  return(info)
}


#' @name opener
#' @title  Decompress ZIP/GZ/TAR.. and Hatanaka-compressed RINEX format
#' @description Decompress ZIP/GZ/TAR.. and  Hatanaka-compressed RINEX format.

#'
#' @param f path to  RINEX format file 
#' @param header list with header info - if not found it will try to read by calling the
#' corresponding header. 
#' @param verbose boolean, DEFAULT FALSE  to output extra messages
#' @return  A connection in read-only mode to RINEX file with pointer 
#' right after the header, so it is ready to be read.
#' @export
#'
#' @examples
#' ##
opener <- function(f = NULL, header = NA, verbose=FALSE) {

  if (is.null(f) || !file.exists(f)) {
    stop("File ", f," does not exist")
  }
  
  finf.st_size <- file.size(f)
  if (finf.st_size > 100E6) {
    message("opening ", finf.st_size / 1E6 , " MByte in file ", f)
  }
  
  if(is.na(hdr) ) {
    info <- rinexinfo(f)
    if(info$rinextype=="obs"){
      info <- obsheader3(info$filepath)
    } else {
      message("Not yet implemented for files of type ", info$rinextype)
      return (NULL)
    }
  }
  
  if(verbose) message( sprintf("%s=%s  - ", paste(names(info)), paste(info) ) )
  
  con<-tryCatch({ 
    ff <- file(info$filepath, "r")
    fff<-readLines(ff, info$linesInHeader)
    if(!grepl("END OF HEADER", fff[[length(fff)]])){
      message("Something is wrong, last line of header seems to be:\n",  
              trimws(fff[[length(fff)]]))
      return(NULL)
    }
    ff
  }, error=function(e){
    message(e)
    return(NULL)
  })
  con
}
