# https://github.com/mvglasow/satstat/wiki/NMEA-IDs 

SBAS <- 100  # offset for ID
GLONASS <- 37
QZSS <- 192
BEIDOU <- 0



#' rinexobs3
#' @description from [http://acc.igs.org/misc/rinex304.pdf](http://acc.igs.org/misc/rinex304.pdf)
#' decodes RINEX 3.04 version files and creates a NetCDF or data.table
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
#' @param udt  boolean, DEFAULT FALSE,   stands for use data.table - this creates and object 
#' NOT of NetCDF but of a long table with columns - easier to understand for some
#'
#' @return HDF5 array or data.table object if udt=TRUE
#' @export
#'
#' @examples
#' f <- rRINEX::example.files$obs.crx.rover
#' f <- "data-raw/BZRG00ITA_S_20203461015_15M_01S_MO.rnx"
#' rinexobs3(f, verbose=TRUE, tlim1=Sys.time(), tlim2=NA)
rinexobs3<-function(
  f,
  use = NA,
  tlim1 = NA,
  tlim2 = NA,
  useindicators = FALSE,
  meas = NA,
  verbose = FALSE,
  interval = .0,
  udt = FALSE){
  
  interval <- check_time_interval(interval)

  # if(!is.null(tlim1) && !is.null(tlim2)){
  #   interval <- make_time_interval(tlim1, tlim2)
  # } else {
  #   interval <- NULL
  # }
  close(oo)
  oo<-opener(f)
  if(is.null(oo) || !inherits(oo, "file") || 
     !tryCatch(isOpen(oo), error=function(e){ FALSE }) ){
    message("Problem readine the input file!")
    return(NULL)
  }
  
  hdr <- obsheader3(oo, use=use, meas=meas, verbose=verbose)
  tt<-initializeNetCDF()
  dataArray<-tt$netcdf
  
  
  RNetCDF::dim.def.nc(dataArray, "sv", dimlength = length(unlist(hdr$fields)))    
  RNetCDF::var.def.nc(dataArray, "sv", "NC_CHAR", "sv")  
  RNetCDF::att.put.nc(dataArray, "sv", "long_name", "NC_CHAR", "unitless")
  RNetCDF::att.put.nc(dataArray, "sv", "unit", "NC_CHAR", "Satellite Vector Names")
  
  
  RNetCDF::att.put.nc(dataArray, "time", "TIME_SYSTEM", "NC_CHAR", hdr$tz)
  RNetCDF::att.put.nc(dataArray, "NC_GLOBAL", "start_timestamp", "NC_UINT64", 
                      bit64::integer64(hdr$TIME.OF.FIRST.OBS))
  
  RNetCDF::close.nc(dataArray)
  # dataArray <- RNetCDF::open.nc(tt$path2file, write = TRUE)
  
  ## here we initialize the vector 
  ## for better performance
  time_offset <- c()
  obsList <- list()
  starts <- seek(oo)
  bench::bench_time(
   while(TRUE){

    epochHeader <- scan(
      oo,
      what = list(
        cc = character(),
        year = character(),
        month = character(),
        day = character(),
        hour = character(),
        minute = character(),
        second = character(), 
        flag = integer(), ## double check here, x2i1 
        nsats = integer(), 
        receivClockOffset = numeric() 
      ),
      quiet = TRUE,
      fill = TRUE,
      dec = hdr[["dec"]],
      nlines = 1
    )
    
    if(!is.na(epochHeader$receivClockOffset)) time_offset<-c(time_offset, offset)
    
    timeString <- sprintf("%s-%s-%sT%s:%s:%2.7f+00:00", 
                          epochHeader$year, 
                          epochHeader$month,
                          epochHeader$day,
                          epochHeader$hour,
                          epochHeader$minute,
                          as.numeric(epochHeader$second)) 

    time <- nanotime::as.nanotime(timeString)

    if(epochHeader$flag!=0){
      message("Epoch flag is not 0 , not handled yet" )
      break
    }
    
    
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
    obsList[[timeString]] <- decodeEpochObs3(oo, epochHeader,  hdr, time, sv, useindicators, verbose)
     
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
  hdr[["APPROX.POSITION.XYZ"]]<-NA
  hdr[["tz"]]<-"GPS"
  hdr[["TIME.OF.FIRST.OBS"]]<-NA
  hdr[["dec"]]<-'.'
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
    # cc<-"  2022    08    08    05    49   39,0130000     GPS         "
    if (grepl("TIME OF FIRST OBS", ln, fixed = TRUE)) {
      timeFirstObs <- tryCatch(
         scan(
          text = cc,
          what = list(
            year = integer(),
            month = integer(),
            day = integer(),
            hour = integer(),
            minute = integer(),
            second = numeric(),
            TT = character()
          ),
          quiet = TRUE,
          dec = hdr[["dec"]],
          nmax = 1
        ),
        error = function(e) {
          if (hdr[["dec"]] == ".") {
            hdr[["dec"]] <<- ","
          } else {
            hdr[["dec"]] <<- "."
          }
          tryCatch(
            scan(
              text = cc,
              what = list(
                year = character(),
                month = character(),
                day = character(),
                hour = character(),
                minute = character(),
                second = character(),
                TT = character()
              ),
              quiet = TRUE,
              dec = hdr[["dec"]],
              nmax = 1
            ),
            error = function(e) {
              message("Cannot decode time of first obs from line\n", ln)
              NULL
            }
          ) 
        }
      )
      
      if(is.null(timeFirstObs)) {
        break
      } 
      
      if(timeFirstObs$TT=="GLO") timeFirstObs$TT<-"UTC"
      
      hdr[["TIME.OF.FIRST.OBS"]] <- nanotime::as.nanotime(sprintf("%s-%s-%sT%s:%s:%2.7f+00:00", 
                                            timeFirstObs$year, 
                                            timeFirstObs$month,
                                            timeFirstObs$day,
                                            timeFirstObs$hour,
                                            timeFirstObs$minute,
                                            as.numeric(timeFirstObs$second)) )
      
      
      hdr[["tz"]]<-timeFirstObs$TT
    }
    if(grepl("APPROX POSITION XYZ", ln, fixed=TRUE)){
      coords <- tryCatch( scan(text=cc, what = numeric(), quiet=TRUE, dec = hdr[["dec"]]),
                          error=function(e){ 
                            NULL
                          })
      ## WTF comma as separator???
      if(is.null(coords)) {
        if (hdr[["dec"]] == ".") {
          hdr[["dec"]] <<- ","
        } else {
          hdr[["dec"]] <<- "."
        }
        coords <- tryCatch( scan(text=cc, what = numeric(), quiet=TRUE, dec = hdr[["dec"]]),
                          error=function(e){ 
                            message(e)
                          })
      }      
      if(!is.null(coords)){
        if(!all(coords==0)){ 
          hdr[["APPROX.POSITION.XYZ"]]<-coords
          hdr[["APPROX.POSITION.LATLONG"]]<- cartesian2geographic(coords )
        } else { 
          if(verbose) message("approximage xyz position is zero \n", ln)
        }
      } else {
        message("Cannot decode coordinates of start position from line\n", ln)
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
  # browser()
 
  if(!is.na(hdr[["TIME.OF.FIRST.OBS"]][[1]])){
    hdr[["t0"]] <- hdr[["TIME.OF.FIRST.OBS"]][[1]]
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
  if(length(sysind)==0) sysind <- NA
  hdr[["fields"]] <- fields
  hdr[["fields_ind"]] <- sysind
  hdr[["Fmax"]] <- Fmax
  ll<-list(satID = character() )
  for(i in unique(unlist(hdr$fields)) ){
    ll[[i]]<-numeric()
  }
  
  hdr[["scanList"]] <- ll
  hdr
}


#' decodeEpochObs3
#' @description block processing of each epoch chunk (time step)
#' @param oo  connection to file that is being read  
#' @param epochHeader header with info from epoch
#' @param hdr header with list returning from function "obsheader3"
#' @param time 
#' @param sv 
#' @param useindicators 
#' @param verbose 
#'
#' @return parsed epoch as matrix with nrow=number of satellites from epochHeader
#' @export
#'
#' @examples
#' #
decodeEpochObs3 <- function(oo,  epochHeader, hdr, time, sv, useindicators, verbose=FALSE){

  epochData <- scan(
    oo,
    # what = character(),
    # sep=";" , #face separator
    what = hdr$scanList,
    quiet = TRUE,
    fill = TRUE,
    dec = hdr$dec,
    nlines = epochHeader$nsats
  )
  
  df <- matrix( unlist(epochData[-1]), nrow=epochHeader$nsats)
  rownames(df)<- epochData$satID
  df
  
}
