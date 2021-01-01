#' Get time and space information from RINEX Observaton file
#'
#' @param rinexFile observation RINEX file 
#'
#' @return  latlong values as c(longitude, latitude)
#' 
#' @export
#' 
#' @examples 
#' rinexFile<-paths.to.example.files()[["obs.rover"]]
#' getInfoFromRINEX.OBS.header(rinexFile)
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
#' @examples 
#' rinexFile<-paths.to.example.files()[["obs.rover"]]
#' getInfoFromRINEX.OBS.header(rinexFile)
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

