#' Gets path to RTLIB
#'
#' @return path to RTKLIB set by user interactively with setRTKLIB()
#' @export
#'
#' @examples getRTKLIB()
getRTKLIB<-function( ){
  Sys.getenv("RTKLIB_rnx2rtkp")
}  

#' Sets path to RTLIB
#'
#' @param RTKLIB_bindir path to RTKLIB bin directory. Default is NULL, 
#' if not set then interactively asks user-input. 
#'
#' @return message
#' @export
#'
#' @examples setRTKLIB("/usr/bin")
setRTKLIB<-function(RTKLIB_bindir=NULL){
  
  if(!is.null(RTKLIB_bindir)){
    Sys.setenv(RTKLIB_bindir =  RTKLIB_bindir)
  }
  
  mess<-""
  RTKLIB_rnx2rtkp<-Sys.getenv("RTKLIB_rnx2rtkp")
  RTKLIB_bindir<-Sys.getenv("RTKLIB_bindir")
  #cat(.rRINEX.ENV.VARS[[1]]) 
  if (is.null(RTKLIB_rnx2rtkp) || !file.exists(RTKLIB_rnx2rtkp) ) { 
    
    fn<- readline(prompt="Enter path to RTKLIB bin folder if you do now know what RTKLIB is please check documentation: ")
    
    if(dir.exists(fn)) { 
      if(file.exists(file.path(fn, "rnx2rtkp")))  
        RTKLIB_rnx2rtkp <-  file.path(fn, "rnx2rtkp")
      
      if(file.exists(file.path(fn, "rnx2rtkp.exe"))) 
        RTKLIB_rnx2rtkp <- file.path(fn, "rnx2rtkp.exe") 
      
      Sys.setenv(RTKLIB_rnx2rtkp =  RTKLIB_rnx2rtkp )
      
      if(!is.null(RTKLIB_rnx2rtkp)) Sys.setenv(RTKLIB_bindir =  fn)
      
    } 
    
    if(is.null(RTKLIB_rnx2rtkp) || !file.exists(RTKLIB_rnx2rtkp)) {
      mess<-paste("The  path ",  RTKLIB_rnx2rtkp , 
      " does not seem to exist OR does not contain RTKLIB files:
package loading will proceed, but reload package to try and reset the path, or call this function")
    } else {
      mess<-paste("Found RTKLIB executable rnx2rtkp in path ", RTKLIB_rnx2rtkp)
    }
    
  } else {
    if(file.exists(RTKLIB_rnx2rtkp)){
      mess<-paste("Recorded path to rnx2rtkp: ", RTKLIB_rnx2rtkp )      
    } else {
      mess<-paste("Recorded path to rnx2rtkp: ", RTKLIB_rnx2rtkp, " BUT does not exist anymore!" )
    }
  }  
  mess
}

.onAttach <- function(libname, pkgname) {
  # to show a startup message
  mess<-setRTKLIB()
  packageStartupMessage(mess)
}

 
.onLoad <- function(libname, pkgname) {
   #packageStartupMessage("dddd")
  
}

