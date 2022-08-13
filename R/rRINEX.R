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

   
  file.extension<- tools::file_ext(filepath)
  
  if( is.element(tolower(file.extension),c("z","gz", "zip")) ){
    filepath<-rRINEX::decompress(filepath)
    file.extension<- tools::file_ext(filepath)
    
  }
   
  if( grepl(".??d$", file.extension) || file.extension=="crx" ){
    warning("Your file seems to be in Hatanaka-compressed ASCII format version,
         but the crx2rnx executable for decompressing is NOT found
         in your system. rRINEX will try to use the self-compiled crx2rnx executable
         in the package accorrding to your  platform")
    filepath<-rRINEX::crx2rnx(filepath)
    file.extension<- tools::file_ext(filepath)
 }

  MAXOBSTYPE  <- 64
  NUMSYS      <-6                   # number of systems */
  MAXRNXLEN   <-(16*MAXOBSTYPE+4)   # max rinex record length */
  MAXPOSHEAD  <-1024                # max head line position */
  MINFREQ_GLO <--7                  # min frequency number glonass */
  MAXFREQ_GLO <-13                  # max frequency number glonass */
  NINCOBS     <-262144              # inclimental number of obs data */
  
  # satellite systems */
  navsys<-c(          
      "SYS_GPS","SYS_GLO","SYS_GAL","SYS_QZS","SYS_SBS","SYS_CMP","0"
  )
 # G: GPS
 # R: GLONASS
 # E: Galileo
 # J: QZSS
 # C: BDS
 # I: IRNSS
 # S: SBAS payload
 # M: Mixed 
  syscodes<-"GREJSC" # satellite system codes */

    
  obscodes<-"CLDS"   # obs type codes */
      
  frqcodes<-"125678"   #frequency codes */
  
  
  ura_eph<-c(    # ura values (ref [3] 20.3.3.3.1.1) */
            2.4,3.4,4.85,6.85,9.65,13.65,24.0,48.0,96.0,192.0,384.0,768.0,1536.0, 3072.0,6144.0,0.0
          )
  
  # type definition -----------------------------------------------------------*/
  sigind_t<-list(                        # signal index type */
              n=NA,                              # number of index */
              frq=NA,               # signal frequency (1:L1,2:L2,...) */
              pos=NA,               # signal index in obs data (-1:no) */
              pri <-NA,    # signal priority (15-0) */
              type<-NA,    # type (0:C,1:L,2:D,3:S) */
              code<-NA,    # obs code (CODE_L??) */
              shift<-NA        # phase shift (cycle) */
            )
          

  
  # decode_obsh<-function(filepath)
  # {
  #   
  #   header<-list()
  #   if(is.na(filepath) || !file.exists(filepath)){
  #     warning("File does not exist")
  #     return(NULL)
  #   }
  #   # default codes for unknown code */
  #     defcodes<-c(
  #       "CWX   ",   # GPS: L125___ */
  #         "CC    ",   # GLO: L12____ */
  #         "X XXXX",   # GAL: L1_5678 */
  #         "CXXX  ",   # QZS: L1256__ */
  #         "C X   ",   # SBS: L1_5___ */
  #         "X  XX "    # BDS: L1__67_ */
  #     )
  #     
  #     con = file(filepath, "r")
  #     cc<-0
  #     while ( TRUE ) {
  #       cc<-cc+1
  #       print(cc)
  #       if(cc > 84) break
  #       line = readLines(con, n = 1)
  #       if ( nchar(line) < 61 ) {
  #         next
  #       }
  #       label<- substr(line, 61, 80)
  #       content<- substr(line, 0, 60)
  #       tmp<-(strsplit(content,"  "))[[1]]
  #       tmp<-trimws(tmp[tmp!=""])
  #       header[label]<- list(tmp)
  #         
  #       if(label == "END OF HEADER") break
  #     }
  #     
  #     close(con)
  #     header
  # }

  header<-getInfoFromRINEX.OBS.header(filepath)
  
}