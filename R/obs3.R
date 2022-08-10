# https://github.com/mvglasow/satstat/wiki/NMEA-IDs 

SBAS <- 100  # offset for ID
GLONASS <- 37
QZSS <- 192
BEIDOU <- 0



#' rinexobs3
#'
#' @param f RINEX OBS 3 filename
#' @param use character vector, single value, e.g. 'G'  
#' or many values, e.g. c('G', 'R') see 
#' [https://files.igs.org/pub/data/format/rinex_clock304.txt](https://files.igs.org/pub/data/format/rinex_clock304.txt)
#' @param tlim1 POSIXct, read only after this time 
#' (if tlim2 is provided, must be that  tlim1 < tlim2)
#' @param tlim2 POSIXct, read only before this time  
#' (if tlim1 is provided, must be that  tlim2 > tlim1)
#' @param useindicators SSI, LLI are output
#' @param meas 'L1C'  or  c('L1C', 'C1C') or similar
#' @param verbose boolean, DEFAULT FALSE,    output extra messages
#' @param interval allows decimating file read by time e.g. every 5 seconds.
#'  Useful to speed up reading of very large RINEX files
#'
#' @return HDF array???
#' @export
#'
#' @examples
#' f <- rRINEX::example.files$obs.crx.rover
#' rinexobs3(f, verbose=TRUE, tlim1=Sys.time(), tlim2=NA)
rinexobs3<-function(
  f,
  use = NA,
  tlim1 = NA,
  tlim2 = NA,
  useindicators = FALSE,
  meas = NA,
  verbose = FALSE,
  interval = .0 ){
  
  interval <- check_time_interval(interval)
  tempfile<-tempfile(fileext = ".nc")
  nc<-RNetCDF::create.nc(filename = tempfile, persist = TRUE)
  
  # if(!is.null(tlim1) && !is.null(tlim2)){
  #   interval <- make_time_interval(tlim1, tlim2)
  # } else {
  #   interval <- NULL
  # }
  
  oo<-opener(f)
  if(is.null(oo) || !inherits(oo, "file") || 
     !tryCatch(isOpen(oo), error=function(e){ FALSE }) ){
    message("Problem readine the input file!")
    return(NULL)
  }
  
  hdr <- obsheader3(oo, use=use, meas=meas, verbose=verbose)
  
  ## here we initialize the vector 
  ## for better performance
  time_offset <- c()
  seek(oo)
  bench::bench_time(
  while(TRUE){
    ln <- readLines(oo, 1)
    if(!isTruthy(ln) || substr(ln, 1, 1)!=">"){
      break
    }

    time <- as.POSIXct(ln, format="> %Y %m %d %H %M %OS", tz="GPS" )
    if (is.na(time)) next
    
    offset<-as.numeric(substr(ln, 42, 56))
    if(!is.na(offset)) time_offset<-c(time_offset, offset)
    
    svn<-as.integer(substr(ln, 34, 35))
    sv <- character(svn)
    raw <- ""
    # Number of visible satellites this time %i3  pg. A13
    # for(i in 1:svn){
      ln <- readr::read_delim(oo, n_max = svn, delim = " ", col_names=FALSE ) 
      sv[[i]] <- substr(ln,1,3)
      raw<- paste0(raw, substr(ln,4,nchar(ln))) 
    # }
    
    if( isTruthy(tlim1) ){
      if(time < tlim1){
        next
      }
    }
    if( isTruthy(tlim2) ){
      if(time > tlim2){
        break
      }
    }
    
    if( isTruthy(interval)){
      if(!exists("last_epoch")){
        last_epoch <- time
      } else {
        if( (time - last_epoch) < interval){
          next
        } else{
          last_epoch <- last_epoch + interval    
        }
      }
    }
    # data <- epoch(data, raw, hdr, time, sv, useindicators, verbose)
   }
  )
} 


#' obsheader3
#'
#' @description  get RINEX 3 OBS types, for each system type
#' optionally, select system type and/or measurement type to greatly
#' speed reading and save memory (RAM, disk)
#' @param f file 
#' @param use default NA, single character or vector of systems 'G'  or c('G', 'R') or similar
#' @param meas default NA, single character or vector of measurments to considre, 
#' e.g.   'L1C'  or  c('L1C', 'C1C') or similar 
#' @param verbose boolean, DEFAULT FALSE,    output extra messages
#'
#' @return  a list with all header information from RINEX 
#' @export
#'
#' @examples
#' #
obsheader3<-function(f, use = NA, meas = NA, verbose=FALSE){
  if(is.character(f)) f<-opener(f)

  fields <- list()
  Fmax <- 0
  seek(f, 0)
  hdr <- rinexinfo(f)
  hdr[["hasPositionXYZ"]]<-FALSE
  hdr[["sepIsComma"]]<-FALSE
  # lines <- readLines(f,  1, skipNul =  TRUE)
  # lines <- lines[-1]
  addsys <- FALSE
  if(verbose) message("Reading ",length(lines)," lines.")

  ## 
  for(i in 1:100){
    
    ln <- readLines(f,  1)
    
    if(grepl("END OF HEADER", ln, fixed=TRUE)){
      break
    }
    

    
    hd = substr(ln,61,80)
    cc = substr(ln,1,60)
    
    if(grepl("APPROX POSITION XYZ", ln, fixed=TRUE)){
      coords <- tryCatch( scan(text=cc, what = numeric(), quiet=TRUE, dec = "."),
                          error=function(e){ 
                            NULL
                          })
      ## WTF comma as separator???
      if(is.null(coords)) {
        hdr[["sepIsComma"]]<-TRUE
        coords <- tryCatch( scan(text=cc, what = numeric(), quiet=TRUE, dec = ","),
                          error=function(e){ 
                            message(e)
                          })
      }      
      if(!is.null(coords)){
        if(!all(coords==0)){
          hdr[["hasPositionXYZ"]]<-TRUE
          hdr[["XYZ"]]<-coords
        }
      }
    }
    
    if(grepl("SYS / # / OBS TYPES", ln, fixed=TRUE)){
      # message(ln)
      if(!addsys){ 
        k <- substr(cc, 1,1)
        nn <- as.integer(substr(cc, 4,6))
        if(is.na(nn)){
          message("Problem reading header at line:\n", ln)
          return(NULL)
        }
        Fmax = max(nn, Fmax)
        if(nn>13){
          addsys<-TRUE
        }
        ss<-strsplit(substr(cc,7,60), " ",  fixed = TRUE)
        fields[[k]] <- ss[[1]][ss[[1]]!=""] 
        
      } else { 
        ss<-strsplit(substr(cc,7,60), " ",  fixed = TRUE)
        fields[[k]] <- append(fields[[k]], ss[[1]][ss[[1]]!=""] )
        addsys<-FALSE
      }
      next
    }
     
    if( is.null(hdr[[trimws(ln)]]) ) hdr[[trimws(hd)]] <- cc
    else hdr[[trimws(hd)]] <- paste(hdr[[trimws(hd)]],  cc)
 
    
  }
  
  t0s <- hdr["TIME OF FIRST OBS"][[1]]
  if(!is.null(t0s)){
    z <- strsplit(t0s, " ")[[1]] 
    z<-z[z!=""]
    tz <- ifelse(z[[length(z)]]=="GPS", "UTC", z[[length(z)]])
    hdr[["t0"]] <- as.POSIXct(t0s, format="%Y %m %d %H %M %OS", tz=tz)
  }
  if(!is.null(hdr[["INTERVAL"]]) ){
    hdr[["INTERVAL"]] <- as.numeric(substr(hdr[["INTERVAL"]][[1]], 1, 10) )
  }
  
  if(is.character(use)){
    if( !any(use%in%names(fields) ) ){
      message("system type c(", paste(use, collapse=", "), ") not found in RINEX file")
      return(NULL)
    }
    fields <- fields[  intersect(use, names(fields))  ]
  }
  
  
  sysind <- list()
  if(is.character(meas)){
    for(sk in names(fields) ){
 
      ind <- grepl(pattern=paste0(meas, collapse="|"), fields[[sk]]  )
      
      fields[[sk]] <- fields[[sk]][ind]
      sysind[[sk]] <- rep(NA, Fmax * 3)  # *3 due to LLI, SSI
      if(any(ind)) {
        sysind[[sk]][1:(length(ind)*3) ] <- rep(ind, each=3)
      }
    }
  }
  hdr[["fields"]] <- fields
  hdr[["fields_ind"]] <- sysind
  hdr[["Fmax"]] <- Fmax
  hdr
}



#' epoch
#' @description block processing of each epoch (time step)
#' @param data 
#' @param raw 
#' @param hdr 
#' @param time 
#' @param sv 
#' @param useindicators 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
epoch <- function(data, raw, hdr, time, sv, useindicators, verbose){
  darr = np.atleast_2d(
    np.genfromtxt(io.BytesIO(raw.encode("ascii")), 
                  delimiter=(14, 1, 1) * hdr["Fmax"])
  )
}