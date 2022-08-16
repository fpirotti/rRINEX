#' getApproxPositionFromRINEX.OBS.header
#'  
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
#' # rinexFile<-rRINEX::example.files[["obs.rover"]]
#' # pos<-getApproxPositionFromRINEX.OBS.header(rinexFile)
#' # print(pos)
#' # pos<-getApproxPositionFromRINEX.OBS.header(rinexFile, "sf")
getApproxPositionFromRINEX.OBS.header<-function(rinexFile, class="numeric"){
  hdr <- obsheader3(rinexFile)
  latlong <- hdr[["APPROX.POSITION.LATLONG"]]
  if(is.null(latlong)){ 
    message("No appproximate position found!")
    return(NULL) 
  } 
  # if(class=="sf")    return(sf::st_sfc( sf::st_point(latlong), crs=9000 ))
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
#' #rinexFile<-rRINEX::example.files[["obs.rover"]]
#' #getInfoFromRINEX.OBS.header(rinexFile)
getInfoFromRINEX.OBS.header<-function(rinexFile){
  hdr <- obsheader3(rinexFile)
  latlong <- hdr[["APPROX.POSITION.LATLONG"]]
  if(is.null(latlong)){ 
    message("No appproximate position found!")
    return(NULL) 
  }
  latlong <- hdr[["APPROX.POSITION.LATLONG"]]
  list(latlong=latlong, timestamp=hdr[["TIME.OF.FIRST.OBS"]])
} 

