
.onAttach <- function(libname, pkgname) {
  # to show a startup message
  mess<-getRTKLIB()
  packageStartupMessage(mess)
}

 
.onLoad <- function(libname, pkgname) {
   #packageStartupMessage("dddd")
  
}

