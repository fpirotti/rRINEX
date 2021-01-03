#' rtklib.checkIsWorking
#' @description  Checks if RTKLIB path to rnx2rtkp executable is working
#' @return TRUE or FALSE depending if RTKLIB path to rnx2rtkp executable is working
#' @export
#'
#' @examples rtklib.checkIsWorking()
rtklib.checkIsWorking <- function(){
  rtklib<-getRTKLIB()
  tf<-file.exists(rtklib)
  if(tf) tf <- nchar(Sys.which(rtklib)) > 0
  if(tf) tf <- length(system2(rtklib, "--h",  stdout = T, stderr = T)) > 40
  tf
}

#' rtklib.checkIsWorking
#'
#' @param type - see RTKLIB's RTKPOST options. Files with .conf extension.
#'
#' @description  Applies post-processing
#' @return TRUE or FALSE depending if RTKLIB path to rnx2rtkp executable is working
#' @export
#'
#' @examples system
rtklib.apply <- function(type="single"){
  #starttime<-ts %s -te %s  #2019/09/05 19:13:56
  
  
  cc<-paste("rnx2rtkp -k "," -o out.pos ebay.obs zdv13480.15o ebay.nav")
}


#'  rnx2rtkp From RTKLIB:
#'  
#'  @usage: rnx2rtkp [option]... file file [...]
#' 
#'  Read RINEX OBS/NAV/GNAV/HNAV/CLK, SP3, SBAS message log files and ccompute 
#'  receiver (rover) positions and output position solutions.
#'  The first RINEX OBS file shall contain receiver (rover) observations. For the
#'  relative mode, the second RINEX OBS file shall contain reference
#'  (base station) receiver observations. At least one RINEX NAV/GNAV/HNAV
#'  file shall be included in input files. To use SP3 precise ephemeris, specify
#'  the path in the files. The extension of the SP3 file shall be .sp3 or .eph.
#'  All of the input file paths can include wild-cards (*). To avoid command
#'  line deployment of wild-cards, use "..." for paths with wild-cards.
#'  Command line options are as follows ([]:default). With -k option, the
#'  processing options are input from the configuration file. In this case,
#'  command line options precede options in the configuration file.
#'  
#' @param  k file   input options from configuration file [off]
#' @param  o file   set output file [stdout]
#' @param  ts ds ts start day/time (ds=y/m/d ts=h:m:s) [obs start time]
#' @param  te de te end day/time   (de=y/m/d te=h:m:s) [obs end time]
#' @param  ti tint  time interval (sec) [all]
#' @param  p mode   mode (0:single, 1:dgps, 2:kinematic, 3:static, 4:moving-base,
#'                 5:fixed, 6:ppp-kinematic, 7:ppp-static) [2]
#' @param  m mask   elevation mask angle (deg) [15]
#' @param  f freq   number of frequencies for relative mode (1:L1,2:L1+L2,3:L1+L2+L5) [2]
#' @param  v thres  validation threshold for integer ambiguity (0.0:no AR) [3.0]
#' @param  b        backward solutions [off]
#' @param  c        forward/backward combined solutions [off]
#' @param  i        instantaneous integer ambiguity resolution [off]
#' @param  h        fix and hold for integer ambiguity resolution [off]
#' @param  e        output x/y/z-ecef position [latitude/longitude/height]
#' @param  a        output e/n/u-baseline [latitude/longitude/height]
#' @param  n        output NMEA-0183 GGA sentence [off]
#' @param  g        output latitude/longitude in the form of ddd mm ss.ss' [ddd.ddd]
#' @param  t        output time in the form of yyyy/mm/dd hh:mm:ss.ss [sssss.ss]
#' @param  u        output time in utc [gpst]
#' @param  d col    number of decimals in time [3]
#' @param  s sep    field separator [' ']
#' @param  r x y z  reference (base) receiver ecef pos (m) [average of single pos]
#'           rover receiver ecef pos (m) for fixed or ppp-fixed mode
#' @param  l lat lon hgt reference (base) receiver latitude/longitude/height (deg/m)
#'           rover latitude/longitude/height for fixed or ppp-fixed mode
#' @param  y level  output soltion status (0:off,1:states,2:residuals) [0]
#' @param  x level  debug trace level (0:off) [0]
#' @description  Applies post-processing using RTKLIB executable 'rnx2rkp'
#' @return TRUE or FALSE depending if RTKLIB path to rnx2rtkp executable is working
#' @export
#'
#' @examples system
rtklib.rnx2rtkp <- function(k=NA, o="", ts, te, ti=NA, p=2, m=15,
                            f=2,  v =3.0,  b=F, c=NA, i=NA ,h=NA,
                            e=NA, a=NA, n=NA, g=NA,t=NA,u=NA, 
                            d=3, s=' ', r=NA , l=NA , y=0, x=0){
  #starttime<-ts %s -te %s  #2019/09/05 19:13:56
  
  
  cc<-paste("rnx2rtkp -k "," -o out.pos ebay.obs zdv13480.15o ebay.nav")
}

