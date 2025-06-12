#' rtklib.readrnxh
#' @description read RINEX header
#' @param f RINEX filename 
#' 
#' @return list with a tue items:
#' \itemize{
#'  \item APPROX.POSITION.XYZ - Cartesian coordinate in ECEF system of approximate 
#'  position
#'  \item APPROX.POSITION.LATLONG - Geographic coordinate in longitude and latitude
#' } 
#' @export
#'
#' @examples
#' f <- rRINEX::example.files$obs.crx.rover
#' f <- "data-raw/pado346k.20d.Z"
#' f <- "data-raw/BZRG00ITA_S_20203461015_15M_01S_MO.rnx"
#' # rinexobs3(f, verbose=TRUE, tlim1=Sys.time(), tlim2=NA)
readrnxhApproxPosition <- function(f=NULL){
  if (is.null(f) || !file.exists(f)) {
    message("File ", f," does not exist")
    return(NULL)
  }
  hdr <- list(dec='.')
  oo<-tryCatch({ 
    file(f, "r")
  }, error=function(e){
    message(e)
    return(NULL)
  })
  if(is.null(oo)){
    # return error
    return(NULL)
  }
  count <- 0 
  while(TRUE){
    
    ln <- readLines(oo,  1 )
    hd = substr(ln,61,80)
    cc = substr(ln,1,60)
    count <- count +1
    if(count > 300){
      warning("End of max lines (300) reached!")
      break
    }
    
    if(grepl("END OF HEADER", ln, fixed=TRUE)){
      break
    }
    
    
    if(grepl("APPROX POSITION XYZ", ln, fixed=TRUE)){
      coords <- tryCatch( scan(text=cc, what = numeric(), quiet=TRUE, dec = hdr[["dec"]]),
                          error=function(e){ 
                            NULL
                          })
      ## WTF comma as separator???
      if(is.null(coords)) {
        if (hdr[["dec"]] == ".") {
          hdr[["dec"]] <<- ","
        } else {
          hdr[["dec"]] <<- "."
        }
        coords <- tryCatch( scan(text=cc, what = numeric(), quiet=TRUE, dec = hdr[["dec"]]),
                            error=function(e){ 
                              message(e)
                            })
      }      
      if(!is.null(coords)){
        if(!all(coords==0)){ 
          hdr[["APPROX.POSITION.XYZ"]]<-coords
          hdr[["APPROX.POSITION.LATLONG"]]<- cartesian2geographic(coords )
        } else { 
          if(verbose) message("approximage xyz position is zero \n", ln)
        }
      } else {
        message("Cannot decode coordinates of start position from line\n", ln)
      }
      break
    }
  }
  close(oo)
  return(hdr)
}

#' rtklib.readrnxh
#' @description read RINEX header
#' @param f RINEX filename 
#' 
#' @return list of processed files
#' @export
#'
#' @examples
#' f <- rRINEX::example.files$obs.crx.rover
#' f <- "data-raw/pado346k.20d.Z"
#' f <- "data-raw/BZRG00ITA_S_20203461015_15M_01S_MO.rnx"
#' # rinexobs3(f, verbose=TRUE, tlim1=Sys.time(), tlim2=NA)
rtklib.readrnxh <- function(f){
  hdr <- readrnxhApproxPosition(f)
  
  print(hdr)
  hdr2 <- rtklibR_readrnx(f)
  print(hdr2)
}