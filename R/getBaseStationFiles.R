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
#' @param timestamp time of survey in RINEX file
#' @param stationname Name of the station e.g. for Padova (Italy) station name is PADO 
#'
#' @return url string with substitutions of the following
#' @export
#'
#' @examples   
#' url<-"ftps://gdc.cddis.eosdis.nasa.gov/gps/data/campaign/mgex/hourly/rinex3/%n/%h/%s%n%H.%yd.Z"
#' formatURL(url, (Sys.time() - 14*60*60*24) )
#' 
formatURL<-function(url, timestamp, stationname=NA){
  
  nn<-timestamp2parts(timestamp)
  toMatch <- sprintf("%%%s", names(nn) )
 
  url<-mgsub::mgsub(string = url, pattern = toMatch, replacement = unlist(nn) )
  if(!is.na(stationname)) {
    url <- gsub("%s", stationname, url )
  }
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
#' @param timestamp date and time at which the file should be searched for
#' @param station name of station of the EUREF Region
#' @param onlycheck (default FALSE) do not download the file, just check if it exists and 
#' return TRUE or FALSE
#' @param type  either "highrate" or other 
#'
#' @return file path to downloaded observation and navigation files if download is successful,
#' or logical FALSE if not, or  if only checking logical TRUE or FALSE
#'
#' @examples 
#' ## download file from 2 weeks before today from Genova's baset station
#' filepath<-getFile.liguria( as.POSIXct(Sys.time() - 14*(60*60*24) ), "GENU" )
#' print(filepath)
#' file.remove(filepath)
get.IGS<-function(timestamp, station, onlycheck=F, type="highrate"){
  
  ts<-timestamp2parts(timestamp)
  
  base<-"igs.bkg.bund.de"
  url<-sprintf("http://%s/root_ftp/IGS/%s/%s/%s/%s", base, type,
               ts$Y, ts$n,
               ts$H )
  
  fn<-sprintf("%s%s%s.%sd.Z", tolower(station), (ts$n), ts$h, ts$y)
  
  isup<-pingr::is_up(base)  && RCurl::url.exists(url)
  
  if(onlycheck) return(isup)
  else if (!onlycheck && !isup) return(F) 
  
  result <- RCurl::getURL("https://igs.bkg.bund.de/root_ftp/IGS/highrate/2020/263/l/",verbose=F,
                          ftp.use.epsv=TRUE, dirlistonly = TRUE)
  htmlinks<-XML::getHTMLLinks(result)
  
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
#'
#' @examples 
#' ## download file from 2 weeks before today from Genova's baset station
#' filepath<-getFile.liguria( as.POSIXct(Sys.time() - 14*(60*60*24) ), "GENU" )
#' print(filepath)
#' file.remove(filepath)
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
#' 
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
#' @param rinexFile can be either a RINEX file 
#' (must have APPROX POSITION XYZ information in header, check (\href{https://gage.upc.edu/sites/default/files/gLAB/HTML/Observation_Rinex_v3.01.html}{https://gage.upc.edu/sites/default/files/gLAB/HTML/Observation_Rinex_v3.01.html})  ) 
#' OR
#' can be an XY coordinate in  longitude and latitude  e.g. c(12.432, 45.542)
#' OR 
#' can be an (sf) object (not yet implemented)
#' @param nStations number of closest stations to find. Default=3 
#' 
#' @return An sf object with three closest stations and distances.

#' 
#' @export
#' 
#' @examples 
#' ef<-paths.to.example.files() 
#' getClosestStations(ef[["obs.rover"]])
getClosestStations<-function(rinexFile, nStations=3){
  
  if(is.character(rinexFile)) pos<-getInfoFromRINEX.OBS.header(rinexFile) 
  else if(length(rinexFile)==2 && 
          is.numeric(rinexFile[[1]]) &&
          is.numeric(rinexFile[[2]])
  ) {
    pos<-list(latlong=rinexFile)
  }
  else{
    warning("Something wrong with lat and long values, ", 
            rinexFile)
    return(NULL)
  }
  if(length(pos$latlong)!=2){
    warning("Something wrong with lat and long values, ", 
            pos$latlong[[1]], ", ", pos$latlong[[2]])
    return(NULL)
  }
  point<- sf::st_sfc(sf::st_point(pos$latlong), crs = 9000)
  
  stazioniGNSSita<-stazioniGNSS[["ITA"]]
  
  st.near.station<-NULL
  for(i in 1:nStations){
    
    stz1<-sf::st_nearest_feature(point, stazioniGNSSita)
    if(is.null(st.near.station)) {
      st.near.station<-stazioniGNSSita[stz1,]
    } else {
      st.near.station<-rbind(st.near.station, stazioniGNSSita[stz1,])
    }
    stazioniGNSSita<-stazioniGNSSita[-stz1,]
    
  }
  
  st.near.station$distance <- sf::st_distance(st.near.station, point)
  st.near.station
}


#' Show closest GNSS stations in mapview 
#'
#' @param rinexFile observation RINEX file 
#'
#' @return Shows mapview of points.
#' 
#' @export
#' @examples 
#'  ef<-paths.to.example.files() 
#'  ## plotClosestStation(ef$obs.rover)
plotClosestStation<-function(rinexFile){
  pos<-getInfoFromRINEX.OBS.header(rinexFile) 
  closestStations<-getClosestStations(pos)
  mapview::mapview()
  + 
    mapview::mapview(closestStations, label=closestStations$nid ) +
    mapview::mapview(pos, label="RINEX", color="red" ) 
  
}
