library("tools")
library("R.utils")

#' @name crx2rnx
#' @title  Decompress Hatanaka-compressed RINEX format    
#' @description Decompress Hatanaka-compressed RINEX format.    
#' 
#' Uses compiled code from crx2rnx.c 
#' Original C code is from https://terras.gsi.go.jp/ja/crx2rnx.html
#' Please check license  
#' 
#' @references LICENSE:   
#//' (\href{https://terras.gsi.go.jp/ja/crx2rnx/LICENSE.txt}{https://terras.gsi.go.jp/ja/crx2rnx/LICENSE.txt})
#' See the following paper for more detail of the compression format and the tools:    
#//' 
#' @references PAPER:  
#//' Hatanaka, Y. (2008): A Compression Format and Tools for GNSS Observation Data,
#' Bulletin of the Geographical Survey Institute, 55, 21-30,
#' (\href{http://www.gsi.go.jp/ENGLISH/Bulletin55.html}{hhttp://www.gsi.go.jp/ENGLISH/Bulletin55.html})
#'
#'
#' @param filepath path to file that is Hatanaka-compressed RINEX format
#' @return character string of path to decompressed file
#'
#' @example  crx2rnx(system.file("extdata", "pado348o.20d.Z", package = "rRINEX"))
crx2rnx<-function(filepath){
  if(!file.exists(filepath)){
    stop("File does not exist")
  }
  
  path2crx2rnx <- system.file("bin", "crx2rnx", package = "rRINEX")
  if(path2crx2rnx==""){
    stop("crx2rnx executable not found in package, please contact developer")
  }
  res<-tryCatch( 
      system(paste(path2crx2rnx, filepath), intern=T ) 
     ,
    error = function(e) {
      e
    },
    warning = function(e) {
      e
    }
  )
  if(is.element("warning", class(res)) || is.element("error", class(res))){
    warning("Something did not work in crx2rnx decompression, check warning messages.")
    return(NULL)
  }
  if(!is.character(res)){
    warning("Something did not work in crx2rnx decompression, check warning messages.")
    return(NULL)
  }
  return(res)
  
}





#' Decompress .Z files (Linux only for now)
#'
#'
#' @param filepath path of file to be decompressed
#'
#' @return path to decompressed file or NULL on error
#' @export
#'
#' @examples decZ(system.file("extdata", "pado348o.20d.Z", package = "rRINEX"))
decZ<-function(filepath){
  if(!file.exists(filepath)){
    warning("File ", filepath, " does not exist") 
    return(NULL)
  } 
  file.extension<- tools::file_ext(filepath)
  sy<-get_os()
  
  if(toupper(file.extension)=="Z"){
    message("Extracting compressed file") 
    if(sy!="windows"){
      system(sprintf('uncompress -k %s',filepath))
    } else {
      stop("Sorry, Windows is not yet supported. Please use software like 7zip or WinZip to un compress the .Z file and then use the uncompressed file.")
    }
    filepath<-gsub(sprintf("[.]%s$", file.extension), "", 
                   filepath, ignore.case = TRUE)
    
  } else {
    warning("Does not have .Z extension, stopping") 
    filepath<-NULL
  } 
  
  filepath
}


#' decGZip
#'
#' @param filepath path of file to be decompressed
#'
#' @return path to decompressed file or NULL on error
#' @export 
decGZip<-function(filepath){
  if(!file.exists(filepath)){
    warning("File ", filepath, " does not exist") 
    return(NULL)
  } 
  file.extension<- tools::file_ext(filepath)
  if(toupper(file.extension)=="GZ"){
    message("Extracting GZIP compressed file")
    R.utils::gunzip(filepath, remove=F, overwrite=T)
    filepath<-gsub(sprintf("[.]%s$", file.extension), "", 
                   filepath, ignore.case = TRUE) 
  }  else {
    warning("Does not have .gz extension, will not decompress") 
    filepath<-NULL
  } 
  filepath
}



#' Decompress
#' @description  Decompress .Z or .gz files
#' @param filepath path of file to be decompressed
#'
#' @return path to decompressed file or NULL on error
#' @export 
decompress<-function(filepath){
  if(!file.exists(filepath)){
    warning("File ", filepath, " does not exist") 
    return(NULL)
  } 
  file.extension<- tools::file_ext(filepath)
  if(toupper(file.extension)=="GZ"){
    filepath<- decGZip(filepath)
  }
  if(toupper(file.extension)=="Z"){
    filepath<- decZ(filepath)
  }
}

