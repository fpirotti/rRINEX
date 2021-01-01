

.onAttach <- function(libname, pkgname) {
  # to show a startup message
  RTKLIB_rnx2rtkp<-Sys.getenv("RTKLIB_rnx2rtkp")
  RTKLIB_bin<-Sys.getenv("RTKLIB_bin")
  #cat(.rRINEX.ENV.VARS[[1]]) 
  if (is.null(RTKLIB_rnx2rtkp) || !file.exists(RTKLIB_rnx2rtkp) ) { 
    
    fn<- readline(prompt="Enter path to RTKLIB bin folder - if you do now know what RTKLIB is please check documentation: ")
    
    packageStartupMessage("Directory \"", fn , "\"")
    
    if(dir.exists(fn)) {
      
      packageStartupMessage("Directory exists... checking for RTKLIB executables")
      if(file.exists(file.path(fn, "rnx2rtkp")))  Sys.setenv(RTKLIB_rnx2rtkp =  file.path(fn, "rnx2rtkp") )
      if(file.exists(file.path(fn, "rnx2rtkp.exe"))) Sys.setenv(RTKLIB_rnx2rtkp =  file.path(fn, "rnx2rtkp") )

      if(!is.null(RTKLIB_rnx2rtkp)) Sys.setenv(RTKLIB_bin =  fn)
      
    } 
    
    if(is.null(RTKLIB_rnx2rtkp)) {
      mess<-"The inserted path does not seem to exist, OR does not contain executable rnx2rtkp:
will proceed, but reload package to try and reset the path"
    } else {
      mess<-paste("Found RTKLIB executable rnx2rtkp in path ", RTKLIB_rnx2rtkp)
    }
    
    packageStartupMessage(mess)
    
  } else {
    if(file.exists(RTKLIB_rnx2rtkp)){
      packageStartupMessage("Recorded path to rnx2rtkp: ", RTKLIB_rnx2rtkp )      
    } else {
      packageStartupMessage("Recorded path to rnx2rtkp: ", RTKLIB_rnx2rtkp, " BUT does not exist anymore!" )
    }
  }  
}

 
.onLoad <- function(libname, pkgname) {
   #packageStartupMessage("dddd")
  
}

