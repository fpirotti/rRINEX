
#' make_time_interval
#' @title make_time_interval
#' @description creates a time interval
#' @param time1 POSIXlt date/time object
#' @param time2 POSIXlt date/time object
#' @param tz time zone "" is the current time zone, and "GMT" is UTC
#'
#' @return a difftime object 
#' @export
#'
#' @examples
#' #
make_time_interval<-function(time1, time2, tz="UTC") {
  if( !inherits(time1, "POSIXlt") ){
      message("first argument is not in POSIXlt format, please provide both arguments as POSIXlt objects.")
    return(NULL)
  }
  if( !inherits(time2, "POSIXlt") ){
    message("second argument is not in POSIXlt format, please provide both arguments as POSIXlt objects.")
    return(NULL)
  } 

  difftime(time1, time2)
  
}

#' check_time_interval
#' @title  check_time_interval
#' @description Checks if it is a time interval or creates one from an integer and a unit
#' 
#' @param interval numeric value of interval
#' @param units string one of "secs", "mins", "hours", "days", "weeks"
#' @param tz time zone "" is the current time zone, and "GMT" is UTC
#'
#' @return a "difftime" object
#' @export
#'
#' @examples
#' #
check_time_interval<-function(interval, units="secs", tz="UTC") {
  if( inherits(interval, "difftime") ){
    return(interval)
  }
  
  if(is.numeric(interval)||is.integer(interval)){
    if(interval < 0){
      message("time interval must be non-negative")
      return(NULL)
    }
    df<-as.difftime(interval, format = "%X", units = units, tz = tz)

  }
  
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



#' cartesian2geographic
#' @description converts Cartesian coordinates to Spherical coordinates
#' in WGS84 datum 
#' @param x numeric, X coordinate
#' @param y numeric, Y coordinate
#' @param z numeric, Z coordinate
#'
#' @return longitude and latitude and ellipsoid height e.g. c(long=3, lat=45.6, h=44)
#' @export
#'
#' @examples
#' cartesian2geographic(-1079348.2174506688, 6121287.922664762, 1425405.2088229598 )
cartesian2geographic <- function(x,y,z){
  r = sqrt(x*x + y*y + z*z)
  h = r -  6378137 
  long <- atan2(y, x) * 360 / (2*pi) 
  lat =  asin(z / r) * 360 / (2*pi) 
  return(c(long=long, lat=lat, h=h))
}
