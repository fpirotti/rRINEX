#' Get approximate position  from RINEX Observaton file
#' @description  Get approximate position ONLY if is in the header of the RINEX observation file
#'
#' @param rinexFile observation RINEX file 
#' @param class set either "numeric" (default), or "sf"   to return these classes.  
#'
#' @return  latlong values as \code{numeric} c(longitude, latitude) or \code{sf}  objects
#' 
#' @export
#' 
#' @examples 
#' rinexFile<-rRINEX::example.files[["obs.rover"]]
#' pos<-getApproxPositionFromRINEX.OBS.header(rinexFile)
#' print(pos)
#' pos<-getApproxPositionFromRINEX.OBS.header(rinexFile, "sf")
getApproxPositionFromRINEX.OBS.header<-function(rinexFile, class="numeric"){
  lines<-readLines(rinexFile, 64 )
  nm<-substr(lines, 61,80)
  approx<-which(nm=="APPROX POSITION XYZ")
  vv<-substr(lines[approx], 1,60)
  vv2<-  strsplit(vv, " ")
  geoc<-NA
  for(x in vv2){ 
    nn<-(as.numeric(x))
    nn<- nn[!is.na(nn)]
    if(sum(nn==0)==3) next
    geoc<-nn
  }
  
  sc<-proj4::ptransform(data =  t(cbind(geoc)), 
                        src.proj = "+proj=geocent +ellps=GRS80 +units=m +no_defs", 
                        dst.proj = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
  
  latlong<-sc[1:2]/pi*180
    
  if(class=="sf")    return(sf::st_sfc( sf::st_point(latlong), crs=9000 ))
  return(latlong)
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
#' rinexFile<-rRINEX::example.files[["obs.rover"]]
#' getInfoFromRINEX.OBS.header(rinexFile)
getInfoFromRINEX.OBS.header<-function(rinexFile){
  lines<- trimws(readLines(rinexFile, 20 ))
  nm<- trimws(substr(lines, 61,80))
  vv<- trimws(substr(lines, 1,60))
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

