#' @name opener
#' @title  Decompress Hatanaka-compressed RINEX format    
#' @description Decompress Hatanaka-compressed RINEX format.    

#'
#' @param f file path to file that is RINEX format
#' @param header read also head in  RINEX format
#' @return ASCII data with RINEX info
#' @export
#' 
#' @examples
#' ##

opener<-function(f=NULL, header=FALSE){
  
  if(is.character(f)){
    return(f)
  }
  
  if(is.null(f) || !file.exists(f)){
    stop("File does not exist")
  }
  
  finf.st_size<-file.size(f)
  if (finf.st_size > 100E6){
    message("opening ", finf.st_size/1E6 ," MByte in file ", f)
  }
  rRINEX::decompress(f)
}

#' @name first_nonblank_line
#' @title first_nonblank_line   
#'  Return first non-blank 80 character line in file
#' @param f path to file that is Hatanaka-compressed RINEX format
#' @param max_lines, INTEGER, maximum number of blank lines
#' @return FIRST NON BLANK LINE IN RINEX
#' 
#' @examples
#' ##
first_nonblank_line<-function(f, max_lines = 10) {
  line <- ""

  if( max_lines < 1){
    stop("must read at least one line")
  }
  found <- FALSE
  for(i in 1:max_lines){
    line<-substr(readLines(f, i), 1, 81)
    if(nchar(trimws(line)) > 2) {
      found <- TRUE
      break
      }
  }
  
  if(!found){
    warning("could not find first valid header line in file ", f)
  }  
  return(line)
}



