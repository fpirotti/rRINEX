#'  The operating system name 
#'
#' @return The operating system: one of "windows", "linux", "osx" or "solaris"
get_os <- function(){
  
  if(.Platform$OS.type=="windows"){
    return(c(sysname="windows") )
  }
  
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <-c(sysname="osx") 
    if ( grepl('SunOS',os) )
      os <-c(sysname="solaris")  
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <-c(sysname="osx") 
    if (grepl("linux-gnu", R.version$os))
      os <-c(sysname="linux") 
  }
  tolower(os)
}


#' Get number of bits in your system architecture
#'
#' @return The operating system 64 or 32 bits as integer
#'   
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
get_bitsChar <- function(){
  paste(get_bits(), "bits", sep="")
}


#' Get number of bits in your system architecture
#'
#' @return The operating system 64 or 32 bits as text "64bits" or "32bits"
#' @export
#' @examples 
#' get_os_info()
get_os_info <- function(){
  list(bits=get_bits(), system= get_os()[[1]]  )
}