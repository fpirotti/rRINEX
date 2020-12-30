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
    path2crx2rnx <- system.file("bin", "crx2rnx64.exe", package = "rRINEX")
  }
  if(path2crx2rnx==""){
    path2crx2rnx <- system.file("bin", "crx2rnx32.exe", package = "rRINEX")
  }
  if(path2crx2rnx==""){
    path2crx2rnx <- system.file("bin", "crx2rnx.exe", package = "rRINEX")
  }
  if(path2crx2rnx==""){
    stop("crx2rnx executable not found in package, please contact developer.")
  }
  # res<-tryCatch( 
  ret<-system(paste(path2crx2rnx, "-f", filepath), intern=T) 
  
  if(!is.null(attr(ret,"status"))){
    warning("Something did not work in crx2rnx decompression, check warning messages.")
    return(NULL)
  }

  
  file.extension<- tools::file_ext(filepath)
  type<-substr(file.extension, nchar(file.extension), nchar(file.extension))
  
  if(tolower(type)=="d") filepath.new<-gsub("[dD]$", 
                                            "o", 
                                            filepath)
  
  if(tolower(file.extension)=="crx") filepath.new<-gsub(".crx$", ".rnx", filepath)
  
  filepath.new
  
}





#' Decompress .Z files (Linux only for now)
#'
#'
#' @param filepath path of file to be decompressed
#'
#' @return path to decompressed file or NULL on problem
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
      warning("Sorry, Windows is not yet supported. Please use software like 7zip or WinZip to un compress the .Z file and then use the uncompressed file.")
      return(NULL)
    }
    filepath<-gsub(sprintf("[.]%s$", file.extension), "", 
                   filepath, ignore.case = TRUE)
    
  } else {
    warning("Does not have .Z extension, stopping") 
    return(NULL)
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



#' decZip
#' @title Unzip files and return RINEX files in ZIP archive
#' @description  Unzip files and return a character vector with names of RINEX files
#' @param filepath path of file to be decompressed
#'
#' @return character vector with names of RINEX files or NULL on error or on missing RINEX files
#' @export 
decZip<-function(filepath){
  file.extension<- tools::file_ext(filepath)
  if(toupper(file.extension)=="ZIP"){
    message("Found ZIP file, will uncompress and try to find a file with any of 
            the following extensions:
            YYo, YYn, YYd, .Z, or .gz and further process any of them")
    filepaths<- utils::unzip(filepath,list = T)
    
    
    filepaths.keep<- file.path(dirname(filepath),
                               grep("(\\.[0-9][0-9][gGdDoOzZnN]$|\\.crx$|\\.Z$|\\.gz$)", 
                                    filepaths$Name, value = T, ignore.case = T ))
    
    if(length(filepaths.keep)==0){
      warning("No RINEX files found inside ZIP files!")
      return(NULL)
    }
    
    utils::unzip(filepath,files = basename(filepaths.keep) , 
          exdir = dirname(filepath))
    
    filepaths.keep
  }
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


  
  file.extension<- tools::file_ext(filepath)
  
  if(toupper(file.extension[[1]])=="GZ"){
    filepath<- decGZip(filepath)
  }
  if(toupper(file.extension)=="Z"){
    filepath<- decZ(filepath)
  }
}

