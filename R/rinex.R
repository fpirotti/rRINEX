# RINEX parser
# A lot of code from RTKLIB! 
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# decode obs header ---------------------------------------------------------*/

#' Create RINEX object
#' 
#' @param FILE.fp file path to RINEX file.
#'
#' @keywords RINEX
#' @export
#' @examples
#' RINEX(NA) 
RINEX<-function(FILE.fp)
{ 
  
  if(is.na(FILE.fp) || file.exists(FILE.fp)){
    warning("File does not exist")
    return(NULL)
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
        
 
  decode_obsh<-function(FILE.fp)
  {
    if(is.na(FILE.fp) || file.exists(FILE.fp)){
      warning("File does not exist")
      return(NULL)
    }
    # default codes for unknown code */
      defcodes<-c(
        "CWX   ",   # GPS: L125___ */
          "CC    ",   # GLO: L12____ */
          "X XXXX",   # GAL: L1_5678 */
          "CXXX  ",   # QZS: L1256__ */
          "C X   ",   # SBS: L1_5___ */
          "X  XX "    # BDS: L1__67_ */
      )
      
      con = file(FILE.fp, "r")
      cc<-0
      while ( TRUE ) {
        cc<-cc+1
        print(cc)
        if(cc > 60) break
        line = readLines(con, n = 1)
        if ( nchar(line) < 61 ) {
          next
        }
        label<- substr(line, 61, 80)
        print(line)
        if(label == "END OF HEADER") break
      }
      
      close(con)
      
  }

}