#'  The operating system name 
#'
#' @return The operating system: one of "windows", "linux", "osx" or "solaris"
#' @export
#'
#' @examples 
#' get_os()
get_os <- function(){
  
  if(.Platform$OS.type=="windows"){
    return("windows")
  }
  
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
    if ( grepl('SunOS',os) )
      os <- "solaris"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux" 
  }
  tolower(os)
}


#' Get number of bits in your system architecture
#'
#' @return The operating system 64 or 32 bits as integer
#' 
#' @export
#'
#' @examples 
#' get_bits()
get_bits <- function(){
  bb<-Sys.info()["machine"]
  if(grepl("_64$", bb)){
    bits<-64
  } else {
    bits<-32
  }
  bits
}



#' Get number of bits in your system architecture
#'
#' @return The operating system 64 or 32 bits as text "64bits" or "32bits"
#' 
#' @export
#'
#' @examples 
#' get_bitsChar()
get_bitsChar <- function(){
  paste(get_bits(), "bits", sep="")
}