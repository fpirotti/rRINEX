#' Get time and space information from RINEX Observaton file
#'
#' @param rinexFile observation RINEX file 
#'
#' @return A list with coordinates and timestamp.
#' \itemize{
#'   \item latlong - Latitude and longitude in GRS80 datum.
#'   \item timestamp - timestamp in POSIX.ct
#' } 
#' 
#' @export
#' 
getInfoFromRINEX.OBS.header<-function(rinexFile){
  lines<-readLines(rinexFile, 20 )
  nm<-substr(lines, 61,80)
  vv<-substr(lines, 1,60)
  vv<-stats::setNames(vv, nm)
  latlong<-stats::na.omit(as.numeric(strsplit(vv["APPROX POSITION XYZ"], " ")[[1]]))

  sc<-proj4::ptransform(data =  t(cbind(latlong)), 
                        src.proj = "+proj=geocent +ellps=GRS80 +units=m +no_defs", 
                        dst.proj = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
  
  latlong<-sc[1:2]/pi*180
  
  date.time<-(strsplit(vv["PGM / RUN BY / DATE"], " "))[[1]]
  ll<-length(date.time)
  timestamp<-as.POSIXct(paste(date.time[c((ll-2):ll)], collapse =" "),  
                        format="%Y%m%d %H%M%S", tz=date.time[[ll]] )
  
  if(is.na(vv["APPROX POSITION XYZ"])){
    warning("no approximate position is found in RINEX file")
    latlong<-NA
  }
  
  list(latlong=latlong, timestamp=timestamp)
} 


#' Get closest permenent base station with GNSS 
#'
#' @param rinexFile observation RINEX file 
#'
#' @param nStations number of closest stations to find. Default=3 
#' 
#' @return An sf object with three closest stations and distances.
 
#' 
#' @export
#' @example 
#' rinexFile<-system.file("extdata", "example.20o", package = "rRINEX")
getClosestStations<-function(rinexFile, nStations=3){
  pos<-getInfoFromRINEX.OBS.header(rinexFile) 
  if(length(pos$latlong)!=2){
    return(NULL)
  }
  point<- sf::st_sfc(sf::st_point(pos$latlong), crs = 4326)
  
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
#' @example 
#' rinexFile<-system.file("extdata", "example.20o", package = "rRINEX")
plotClosestStation<-function(rinexFile){
  closestStations<-getClosestStations(rinexFile)
  mapview::mapview() + mapview::mapview(closestStations, label=closestStations$nid )
}