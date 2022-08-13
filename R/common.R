
#' make_time_interval
#' @title make_time_interval
#' @description creates a time interval
#' @param time1 POSIXlt date/time object
#' @param time2 POSIXlt date/time object 
#'
#' @return a nanotime::nanoduration object 
#' @export
#'
#' @examples
#' #
make_time_interval<-function(time1, time2 ) {
  if( inherits(time1, "POSIXlt") && inherits(time2, "POSIXlt") ){ 
    return(nanotime::nanoduration(nanotime::nanotime(time1), nanotime::nanotime(time2)))
  }
  
  if(  inherits(time1, "nanotime") && inherits(time2, "nanotime")   ){
    return(nanotime::nanoduration(nanotime::nanotime(time1), nanotime::nanotime(time2)))
  } 
  
  message("Arguments are not  in POSIX or nanotime format!")
  return(NULL)

  
}

#' check_time_interval
#' @title  check_time_interval
#' @description Checks if it is a time interval or creates one from an integer and a unit
#' 
#' @param interval character value, (not numeric to allow sub-microsecond precision) 
#'  - can reach nanosecond precision
#' by using for example 3.000000008
#' @param units charact, for now only  "secs" supported, in future also , "mins",
#'  "hours", "days", "weeks" 
#'
#' @return a "nanotime::nanoduration" object
#' @export
#'
#' @examples
#' # check_time_interval
check_time_interval<-function(interval, units="secs" ) {
  if( inherits(interval, "nanoduration") ){  
    return(interval) 
  }
  if( !is.character(interval) ){ 
    message("time interval must be a character value of numbers, see examples")
    return(NULL) 
  }
  components <- list(hours=0, minutes=0, seconds=0, nanonseconds=0)
  if(units=="secs"){
    components$seconds<-trunc(as.numeric(interval))
    components$nanonseconds<-as.integer(decimalsAfterPrecision(interval,0,9))
  } else {
    message("time interval must be in seconds")
    return(NULL) 
  }
  # if(is.numeric(interval)||is.integer(interval)){
    if( components$seconds < 0){
      message("time interval must be non-negative")
      return(NULL)
    }
    # df<-as.difftime(interval, format = "%X", units = units, tz = tz)
    
    df<-nanotime::nanoduration(hours = 0, minutes = 0, 
                               seconds = components$seconds ,
                               nanoseconds=  components$nanonseconds)
    
  # }
  
  df
  
}

#' Truthy and falsy values
#' FROM SHINY! https://github.com/rstudio/shiny 
#' The terms "truthy" and "falsy" generally indicate whether a value, when
#' coerced to a [base::logical()], is `TRUE` or `FALSE`. We use
#' the term a little loosely here; our usage tries to match the intuitive
#' notions of "Is this value missing or available?", or "Has the user provided
#' an answer?", or in the case of action buttons, "Has the button been
#' clicked?".
#'
#' For example, a `textInput` that has not been filled out by the user has
#' a value of `""`, so that is considered a falsy value.
#'
#' To be precise, a value is truthy *unless* it is one of:
#'
#' * `FALSE`
#' * `NULL`
#' * `""`
#' * An empty atomic vector
#' * An atomic vector that contains only missing values
#' * A logical vector that contains all `FALSE` or missing values
#' * An object of class `"try-error"`
#' * A value that represents an unclicked [actionButton()]
#'
#' Note in particular that the value `0` is considered truthy, even though
#' `as.logical(0)` is `FALSE`.
#'
#' @param x An expression whose truthiness value we want to determine
#' @export
isTruthy <- function(x) {
  if (inherits(x, 'try-error'))
    return(FALSE)
  
  if (!is.atomic(x))
    return(TRUE)
  
  if (is.null(x))
    return(FALSE)
  if (length(x) == 0)
    return(FALSE)
  if (all(is.na(x)))
    return(FALSE)
  if (is.character(x) && !any(nzchar(stats::na.omit(x))))
    return(FALSE)
  if (inherits(x, 'shinyActionButtonValue') && x == 0)
    return(FALSE)
  if (is.logical(x) && !any(stats::na.omit(x)))
    return(FALSE)
  
  return(TRUE)
}

#' initializeNetCDF
#'
#' @param filepath optional path to file. If not provided, a temporary file will be created
#' @param overwrite boolean value, DFAULT=FALSE 
#' @param verbose boolean value, DFAULT=FALSE
#' @return boolean FALSE in case of errors, or list with 
#' \itemize{
#' \item path2file :path to NetCDF file
#' \item netcdf: Object of class "NetCDF" which points to the NetCDF dataset    
#' } 
#' @export
#'
#' @examples
#' #
initializeNetCDF<-function(filepath=NA, overwrite=FALSE, verbose=FALSE){
  if(is.na(filepath)) {
    tempfile<-tempfile(fileext = ".nc")
  } else {
    if(dir.exists(filepath)){
      if(verbose) message("Filepath  is a directory, a NetCDF file will be created here")
      tempfile<-tempfile( tmpdir=filepath, fileext = ".nc")
    }
    ext<- tools::file_ext(filepath)
    if(!nzchar(ext) || ext!="nc"){
      message("File name does not have .nc extension, it will be added.")
      tempfile <- sprintf("%s.nc", filepath)
    }
    if(file.exists(filepath) && !overwrite){
      message("File ", filepath ,"  exists, please use overwrite=TRUE if you want to cancel existing file and replace it with this filepath")
      return(FALSE)
    }
    tempfile <- filepath
  }
  
  dataArray<- tryCatch({
    
    dataArray<-RNetCDF::create.nc(filename = tempfile, persist = TRUE) 
    
    RNetCDF::att.put.nc(dataArray, "NC_GLOBAL", "title", "NC_CHAR", "RINEX file")
    RNetCDF::att.put.nc(dataArray, "NC_GLOBAL", "history", "NC_CHAR", "None")
    RNetCDF::att.put.nc(dataArray, "NC_GLOBAL", "institution", "NC_CHAR", "RINEX")
    RNetCDF::att.put.nc(dataArray, "NC_GLOBAL", "source", "NC_CHAR", "rRINEX package")
    RNetCDF::att.put.nc(dataArray, "NC_GLOBAL", "comment", "NC_CHAR", "No comment")
    RNetCDF::att.put.nc(dataArray, "NC_GLOBAL", "references", "NC_CHAR", "https://github.com/fpirotti/rRINEX")

    
    RNetCDF::dim.def.nc(dataArray, "time", unlim=TRUE)    
    RNetCDF::var.def.nc(dataArray, "time", "NC_INT", "time")  
    RNetCDF::att.put.nc(dataArray, "time", "long_name", "NC_CHAR", "TIME_SYSTEM")
    RNetCDF::att.put.nc(dataArray, "time", "unit", "NC_CHAR", "unitless")
    
    
    dataArray
    # According to the CF-standard, a
    # variable should have at least the attributes ‘long_name’ (e.g., ‘measured air temperature’), ‘units’
    # (e.g., ‘degrees_celsius’), and ‘standard_name’ (e.g., ‘air_temperature’) 
  },
  error=function(e){
    message(e)
    return(FALSE)
  })

  if(!dataArray){ 
    return(FALSE)
  }
  return(list(path2file=tempfile, netcdf=dataArray))
}

#' cartesian2geographic
#' @description converts Cartesian coordinates to Spherical coordinates
#' in WGS84 datum 
#' @param x numeric, X coordinate OR a vector of length=3 with x, y, z values 
#' @param y numeric, Y coordinate
#' @param z numeric, Z coordinate
#'
#' @return longitude and latitude and ellipsoid height e.g. c(long=3, lat=45.6, h=44)
#' @export
#'
#' @examples
#' cartesian2geographic(-1079348.2174506688, 6121287.922664762, 1425405.2088229598 )
cartesian2geographic <- function(x,y=NA,z=NA, useproj4=FALSE, returnsf=FALSE){
  
  if(is.na(y)){
    if(length(x)!=3){
      message("Need at least first argument to be a length=3 vector with X Y and Z, or all arguments have to be provided")
      return (NA)
    }
    y<-x[[2]]; z<-x[[3]]; x<-x[[1]]
  } 
  if(useproj4==TRUE && nzchar(system.file(package = "proj4")) ){
    sc<-proj4::ptransform(data =  rbind(c(x,y,z)), 
                          src.proj = "+proj=geocent +ellps=GRS80 +units=m +no_defs", 
                          dst.proj = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
    
    latlong<-sc[1:2]/pi*180
    
    if(returnsf &&  nzchar(system.file(package = "sf")) )    {
      return(sf::st_sfc( sf::st_point(latlong), crs=9000 ))
    }
    return(c(long=latlong[[1]], lat=latlong[[2]]))
  } else {
    r = sqrt(x*x + y*y + z*z)
    h = r -  6378137 
    long <- atan2(y, x) * 360 / (2*pi) 
    lat =  asin(z / r) * 360 / (2*pi) 
    return(c(long=long, lat=lat, h=h))    
  }
  message("should not be here!")
  
}

#' decimalsAfterPrecision
#'
#' @param x character with number
#' @param n integer number, DEFAULT=0
#' @param limit integer - maximum digits, DEFAULT=Inf
#'
#' @return a character string with the decimals after truncation
#' @export
#'
#' @examples
#' decimalsAfterPrecision(pi, 0)
#' decimalsAfterPrecision(pi, 2)
decimalsAfterPrecision <- function(x, n=0, limit=NA) {
  if(is.na(limit)) limit <- .Machine$integer.max
  # if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    su<-sub('0+$', '', as.character(x))
    if(su=="."){
      return("0")
    }
    ss<-(strsplit(su, ".", fixed = TRUE)[[1]][[2]])
     substr(ss, n+1, limit)  
  # } else {
    # return(NA)
  # }
  # ss.int
}

#' decimalplaces
#'
#' @param x numeric number
#'
#' @return an integer with the number of decimal places
#' @export
#'
#' @examples
#' decimalplaces(pi)
#' decimalplaces(456.345263645)
decimalplaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

