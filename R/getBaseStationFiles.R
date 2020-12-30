

#' timestamp2parts
#'
#' @param timestamp date and time to decompose.
#'
#' @return list with year, yearNc (last two digits), month, day, hour, julianDay
#' @export
#'
#' @examples
#' timestamp2parts(Sys.time())
timestamp2parts<-function(timestamp){
  list(
    year=format(timestamp, "%Y"),
    yearNc=format(timestamp, "%y"),
    month=format(timestamp, "%m"),
    day=format(timestamp, "%d"),
    hour=format(timestamp, "%H"),
    julianDay=format(timestamp, "%j")
  )
  
}

#' getFile.liguria
#'
#' @description  Downloads automatically a base station's observation 
#' and navigation file for a specific time and date. Liguria Network.   
#' 
#' See (\href{http://gnss.regione.liguria.it/}{http://gnss.regione.liguria.it/})
#' 
#'
#' @param timestamp date and time at which the file should be searched for
#' @param station name of station of the Veneto Region
#'
#' @return file path to downloaded observation and navigation files
#'
#' @examples
getFile.liguria<-function(timestamp, station){
  
  ts<-timestamp2parts(timestamp)
  lett<-letters[(as.integer(ts$hour)+1)] 
  url<-sprintf("http://gnss.regione.liguria.it/data/%s/rinex/1sec/%d/%d/%d/%s%d%s.%sd.Z",
               toupper(station), as.integer(ts$year), as.integer(ts$month),
               as.integer(ts$day), tolower(station), as.integer(ts$julianDay), lett,
               ts$yearNc )
  
  fn<-sprintf("%s%d%s.%sd.Z", tolower(station), as.integer(ts$julianDay), lett, ts$yearNc)
  
  download.error<-tryCatch( utils::download.file(fn, fn), error=function(e){
    return(e)
  })
  
  if(download.error==0){
    return(fn)
  } else {
    return(download.error)
  }
}

#' getFile.veneto
#' @description  Downloads automatically a base station's observation 
#' and navigation file for a specific time and date. Veneto Network.   
#' 
#' See (\href{http://retegnssveneto.cisas.unipd.it}{http://retegnssveneto.cisas.unipd.it})
#' 
#'
#' @param timestamp date and time at which the file should be searched for
#' @param station name of station of the Veneto Region
#'
#' @return file path to downloaded observation and navigation files
#' 
getFile.veneto<-function(timestamp, station){
  ts<-timestamp2parts(timestamp)
  lett<-letters[(as.integer(ts$hour)+1)] 
  
  url<-sprintf(
    "http://retegnssveneto.cisas.unipd.it/Dati/Rinex/%s/1sec/%d/%d/%s%d%s.%sd.Z",
    toupper(station), as.integer(ts$year), as.integer(ts$julianDay), 
    tolower(station), as.integer(ts$julianDay), lett,
    ts$yearNc )
  
  fn<-sprintf("%s%d%s.%sd.Z", tolower(station), as.integer(ts$julianDay), lett, ts$yearNc)
  
  download.error<-tryCatch( utils::download.file(fn, fn), error=function(e){
    return(e)
  })
  
  if(download.error==0){
    return(fn)
  } else {
    return(download.error)
  }
}
