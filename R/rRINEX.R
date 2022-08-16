#' @title Create RINEX object
#' 
#' @param filepath file path to RINEX file.
#' @description  This class creates a RINEX object thus allows 
#' to open and process RINEX files. Was tested in Linux, not tested in Windows,
#' please send feedback
#' 
#' @keywords RINEX
#' @export
#' @examples
#' #rinex.file.path<-system.file("extdata", "example.20o", package = "rRINEX")
#' #rinex.obj<-RINEX(rinex.file.path) 
RINEX<-function(filepath)
{ 
  
  if(is.na(filepath) || !file.exists(filepath)){
    warning("File does not exist")
    return(NULL)
  }

   

  MAXOBSTYPE  <- 64
  NUMSYS      <- 6                   # number of systems */
  MAXRNXLEN   <- (16*MAXOBSTYPE+4)   # max rinex record length */
  MAXPOSHEAD  <- 1024                # max head line position */
  MINFREQ_GLO <- -7                  # min frequency number glonass */
  MAXFREQ_GLO <- 13                  # max frequency number glonass */
  NINCOBS     <- 262144              # inclimental number of obs data */
  
  # satellite systems */
  navsys<-c(          
      "SYS_GPS","SYS_GLO","SYS_GAL","SYS_QZS","SYS_SBS","SYS_CMP","SYS_IRN","0" 
  )
 # G: GPS
 # R: GLONASS
 # E: Galileo
 # J: QZSS
 # S: SBAS payload
 # C: BDS
 # I: IRNSS
 # M: Mixed 

  
 
  # syscodes<-"GREJSC" # satellite system codes */
  syscodes<- names(rRINEX::vocab$rinex.satellite_system)
  
  # obscodes<-"CLDS"   # obs type codes */
  obscodes<- names(rRINEX::vocab$rinex.type)   # obs type codes */
  
  # frqcodes<-"125678"   #frequency codes */
  frqcodes<- names(rRINEX::vocab$rinex.band)  
   
    
}