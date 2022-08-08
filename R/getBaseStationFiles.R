#' timestamp2parts
#'
#' @param timestamp date and time to decompose.
#'
#' @return list with parts useful to replace variables in URL (loosely adopted from URL_LIST.txt of RTKLIB) 
#' \itemize{
#' \item Y -> yyyy    : year (4 digits) (2000-2099)
#' \item y -> yy      : year (2 digits) (00-99)
#' \item m -> mm      : month           (01-12)
#' \item d -> dd      : day of month    (01-31)
#' \item h -> hh      : hours           (00-23)
#' \item H -> a       : hour code       (a-x)
#' \item M -> mm      : minutes         (00-59)
#' \item n -> ddd     : day of year  -julianday    (001-366)
#' \item W -> wwww    : gps week        (0001-9999)
#' \item D -> d       : day of gps week (0-6) 
#' } 
#' @export
#' 
#' @examples
#' timestamp2parts( (Sys.time() - 14*60*60*24) )
timestamp2parts<-function(timestamp){
  list(
    Y = format(timestamp, "%Y"),
    y = format(timestamp, "%y"),
    m = format(timestamp, "%m"),
    d = format(timestamp, "%d"),
    h = format(timestamp, "%H"),
    H = letters[(as.integer(format(timestamp, "%H"))+1)],
    M = format(timestamp, "%M"),
    n = format(timestamp, "%j"),
    W = format(timestamp, "%W"),
    D = format(timestamp, "%V") 
  )
  
}

#' formatURL
#'
#' @param url the URL string (see example)
#' @param tms time of survey in RINEX file
#' @param stationname Name of the station e.g. for Padova (Italy) station name is PADO 
#'
#' @return url string with substitutions of the following
#' @export
#'
#' @examples   
#' url<-"ftp://gdc.cddis.eosdis.nasa.gov/gps/data/campaign/mgex/hourly/rinex3/%n/%h/%s%n%H.%yd.Z"
#' formatURL(url, Sys.time(), "PADO" )
#' 
formatURL<-function(url, tms, stationname=NA){
  
  nn <- timestamp2parts(tms)
  if(is.na(stationname) ){
    warning("Station name missing. Stopping here")
    return(NULL)
  }
  if(!is.na(stationname) ) {
    
    if(nchar(stationname)!=4){
      warning("Station name should be 4 characters. \"", stationname, "\" (", nchar(stationname), " characters) were found. Stopping here")
      return(NULL)
    }
    
    nn <- c(nn, s=tolower(stationname), S=toupper(stationname), r=stationname )
    
  } 
  
  toMatch <- sprintf("%%%s",  names(nn)  )
  url<-mgsub::mgsub(string = url, pattern = toMatch, replacement = unlist(nn) )
  
  ww<-which(strsplit(url, "")[[1]]=="%")
  if( length(ww)>0 ){
    warning("Some elements were not substituted: ", url)
  }
  
  return(url)
}

#' getFile.liguria
#'
#' @description  Downloads automatically a base station's observation 
#' and navigation file for a specific time and date. IGS framework stations, see
#' (\href{https://www.igs.org/maps/}{https://www.igs.org/maps/}).      
#' 
#' See (\href{http://www.epncb.oma.be/_networkdata/data_access/highrate/}{http://www.epncb.oma.be/_networkdata/data_access/highrate/})
#' 
#'
#' @param tms date and time at which the file should be searched for
#' @param station name of station of the EUREF Region
#' @param onlycheck (default FALSE) do not download the file, just check if it exists and 
#' return TRUE or FALSE
#' @param type  either "highrate" or other 
#'
#' @return file path to downloaded observation and navigation files if download is successful,
#' or logical FALSE if not, or  if only checking logical TRUE or FALSE
#' @export
#' @examples 
#' ## download file from 2 weeks before today from Genova's baset station
#' ## 
#' filepath<-get.IGS( as.POSIXct(Sys.time() - 14*(60*60*24) ), "GENU" )
#' print(filepath)
#' if(is.character(filepath)) file.remove(filepath)
get.IGS<-function(tms, station, onlycheck=F, type="highrate"){
  
  base<-"igs.bkg.bund.de"
  
  url.string<-paste0("https://",base,"/root_ftp/IGS/highrate/%Y/%n/%H/%S00")
  url.tot<-formatURL(url.string, tms, station)
  
  url.string<- paste0("https://",base,"/root_ftp/IGS/highrate/%Y/%n/%H/")
  url.base<-formatURL(url.string, tms, station)

  isup <- pingr::is_up(base)  && RCurl::url.exists(url.base)
  
  if(onlycheck) return(isup)
  else if (!onlycheck && !isup) return(FALSE) 
  
  result <- RCurl::getURL(url.base, verbose=FALSE,
                          ftp.use.epsv=TRUE, 
                          dirlistonly = TRUE)
  
  htmlinks<-XML::getHTMLLinks(result)
  htmlinks
  # grep(sprintf("^%s", station), htmlinks)
  # grep(sprintf("ITA_"), htmlinks, value = T)
  
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
#' @param onlycheck (default FALSE) do not download the file, just check if it exists and 
#' return TRUE or FALSE
#'
#' @return file path to downloaded observation and navigation files if download is successful,
#' or logical FALSE if not, or  if only checking logical TRUE or FALSE
#' @export
#' @examples 
#' ## download file from 2 weeks before today from Genova's baset station
#' filepath<-getFile.liguria( as.POSIXct(Sys.time() - 14*(60*60*24) ), "GENU" )
#' print(filepath)
#' if(is.character(filepath)) file.remove(filepath)
getFile.liguria<-function(timestamp, station, onlycheck=F){
  
  ts<-timestamp2parts(timestamp) 
  
  base<-"gnss.regione.liguria.it"
  url<-sprintf("http://%s/data/%s/rinex/1sec/%d/%d/%d/%s%d%s.%sd.Z",
               base,
               toupper(station), as.integer(ts$Y), as.integer(ts$m),
               as.integer(ts$d), tolower(station), as.integer(ts$n), ts$H,
               ts$y )
  
  fn<-sprintf("%s%d%s.%sd.Z", tolower(station), as.integer(ts$n), ts$H, ts$y)
  
  isup<-pingr::is_up(base, timeout = 2) &&  RCurl::url.exists(url = url)
  
  if(onlycheck) return(isup)
  else if (!onlycheck && !isup) return(F) 
  
  download.error<-tryCatch( utils::download.file(url, fn), error=function(e){
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
#' @param onlycheck (default FALSE) do not download the file, just check if it exists and 
#' return TRUE or FALSE
#'
#' @return file path to downloaded observation and navigation files if download is successful,
#' or logical FALSE if not, or  if only checking logical TRUE or FALSE
#' @export
#' @examples 
#' ## download file from 2 weeks before today from Padova's base GNSS station
#' filepath<-try( getFile.Veneto( as.POSIXct(Sys.time() - 14*(60*60*24) ), "PADO" ) )
#' print(filepath)
getFile.Veneto<-function(timestamp, station, onlycheck=F){
  ts<-timestamp2parts(timestamp)
 
  url<-sprintf(
    "http://retegnssveneto.cisas.unipd.it/Dati/Rinex/%s/1sec/%d/%d/%s%d%s.%sd.Z",
    toupper(station), as.integer(ts$Y), as.integer(ts$n), 
    tolower(station), as.integer(ts$n), ts$H,
    ts$y
    )
  message(url)
  fn<-sprintf("%s%d%s.%sd.Z", tolower(station), as.integer(ts$julianDay), ts$h, ts$yearNc)
  
  isup<-pingr::is_up(url, timeout = 2)
  
  if(onlycheck) return(isup)
  else if (!onlycheck && !isup) return(F) 
  
  download.error<-tryCatch( {utils::download.file(url, fn) }, error=function(e){
    return(e)
  })
  
  if(download.error==0){
    return(fn)
  } else {
    return(download.error)
  }
}





#' Get closest permenent base station with GNSS 
#'
#' @param rinexFile can be either:
#' \itemize{
#'   \item path to a RINEX file - (must have APPROX POSITION XYZ information in header, check (\href{https://gage.upc.edu/sites/default/files/gLAB/HTML/Observation_Rinex_v3.01.html}{https://gage.upc.edu/sites/default/files/gLAB/HTML/Observation_Rinex_v3.01.html})  )
#'   \item  an XY coordinate in  longitude and latitude  e.g. c(12.432, 45.542)
#'   \item an \code{sf} object
#' } 
#'   
#' @param nStations number of closest stations to find. Default=3 
#' 
#' @return An sf object with closest stations and distances.

#' 
#' @export
#' 
#' @examples 
#' ef<-rRINEX::example.files
#' getClosestStations(ef[["obs.rover"]])
getClosestStations<-function(rinexFile, nStations=3){
  
  if(is.character(rinexFile)) {
    point<- getApproxPositionFromRINEX.OBS.header(rinexFile, class = "sf") 
  }
  else if(length(rinexFile)==2 && 
          is.numeric(rinexFile[[1]]) &&
          is.numeric(rinexFile[[2]])
  ) {
    
    pos<-list(latlong=rinexFile)
    if(length(pos$latlong)!=2){
      warning("Something wrong with lat and long values, ", 
              pos$latlong[[1]], ", ", pos$latlong[[2]])
      return(NULL)
    }
    point<- sf::st_sfc(sf::st_point(pos$latlong), crs = 9000)
    
  }
  else if( is.element("sfc", class(rinexFile)  )  ) 
  {
    point<-rinexFile
  }
  else{
    warning("Something wrong with the input value ", 
            rinexFile)
    return(NULL)
  }

  stz<-stazioniGNSS$IGSNetwork
  
  st.near.station<-NULL
  for(i in 1:nStations){
    
    stz1<-sf::st_nearest_feature(point, stz)
    if(is.null(st.near.station)) {
      st.near.station<-stz[stz1,]
    } else {
      st.near.station<-rbind(st.near.station, stz[stz1,])
    }
    stz<-stz[-stz1,]
    
  }
  
  st.near.station$distance <- sf::st_distance(st.near.station, point)
  st.near.station
}


#' Show closest GNSS stations in mapview 
#'
#' @param rinexFile observation RINEX file  
#' @param nStations number of closest stations. Default is 3.
#' @param interactive Uses \code{tmap} package, sets interactive or static.
#'
#' @return Shows interactive or static map of points.
#' 
#' @export
#' @examples 
#'  ef<-rRINEX::example.files
#'  ## plotClosestStation(ef$obs.rover)
plotClosestStations<-function(rinexFile, nStations=3, interactive=F){
  point<- rRINEX::getApproxPositionFromRINEX.OBS.header(rinexFile, class="sf") 
  sf.close<-getClosestStations(point, nStations)
  if(interactive){
    tmap::tmap_mode("view")
    tmap::tm_shape(sf.close) +
      tmap::tm_symbols(col="blue", scale=.99) +
      tmap::tm_shape(point) +
      tmap::tm_symbols(col="red", scale=1.5) 
  } else { 
    tmap::tmap_mode("plot")
    tmap::tm_shape(sf.close) +
      tmap::tm_symbols(col="blue", scale=.99) +
      tmap::tm_shape(point) +
      tmap::tm_symbols(col="red", scale=1.5) 
  }
  
}
