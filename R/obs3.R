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
  interval = ".0" 
  ){
  
  
  nl<-fpeek::peek_count_lines(f)
  
  if(verbose) message("Reading ", nl, " lines")
  
  interval <- check_time_interval(interval)
 
  close(oo)
  oo<-opener(f)
  if(is.null(oo) || !inherits(oo, "file") || 
     !tryCatch(isOpen(oo), error=function(e){ FALSE }) ){
    message("Problem readine the input file!")
    return(NULL)
  }
   
  time_offset <- c()
  
  hdr <- obsheader3(oo, use=use, meas=meas, verbose=verbose)
  obsList <- list()
  starts <- seek(oo) 
  bench::bench_time({
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
    if(length(epochHeader$cc)==0) break
    
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
    
    obs <- decodeEpochObs3(oo, epochHeader,  hdr, time, sv, useindicators, verbose)
    
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
    
    # if( isTruthy(interval)){
    #   if(!exists("last_epoch")){
    #     last_epoch <- time
    #   } else {
    #     if( (time - last_epoch) < interval){
    #       next
    #     } else{
    #       last_epoch <- last_epoch + interval    
    #     }
    #   }
    # }
    # RNetCDF::var.put.nc(dataArray, "obs", obs )
    obsList[[timeString]] <- obs
    
   }
   # hdr <- obsheader3(oo, use=use, meas=meas, verbose=verbose)
   #  #  
   # close(oo)
   finalt <- data.table::rbindlist(obsList, idcol = "Time" )
   data.table::setkey(final, type, system, idx)
   final <- hdr$fieldsNamesMatrix.m[finalt]
  })
  
  sss <- sapply(1:ncol(final), function(x){ length(which(!is.na(final[,x]))) })
  
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
  ## first read the n. of lines
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

  i <- 0
  ## 
  while(TRUE){
    
    ln <- readLines(f,  1)
    
    if(nchar(ln)>0){
      i <- i+1
    } else {
      break
    } 
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
  
  
  start <- 4; tmpi<-4
  from <- c(1)
  to <- c(3)
  for(i in 1:Fmax ){
    ll[[as.character(i)]]<-numeric()
    from <- c(from,  tmpi, tmpi+14, tmpi+15)
    to <- c(to, tmpi+13, tmpi+14, tmpi+15)
    tmpi<-tmpi+16
  }
  
  fieldsNames <- list()
  fieldsNamesMask <- list()
  namesMatrix <- data.frame(id=1:(Fmax*3))
  for(sk in names(fields) ){
    fieldsNames[[sk]] <- paste( fields[[sk]], rep( c("obs", "LLI", "SSI"), each=length(fields[[sk]])), sep=".")
    if(length(fieldsNames[[sk]])!=Fmax*3) {
      rem <- Fmax*3 - length(fieldsNames[[sk]])
      # rem2 <- Fmax  - length(fields[[sk]])
      
      namesMatrix[[sk]] <- c( rep(fields[[sk]], each=3), rep(NA, rem) ) 
      fieldsNamesMask[[sk]]<- c(rep(TRUE, length(fieldsNames[[sk]])), rep(FALSE, rem) )
    } else {
      namesMatrix[[sk]] <- rep(fields[[sk]], each=3)
      fieldsNamesMask[[sk]]<- rep(TRUE, Fmax*3)
    }
  }
  namesMatrix$id<-rep(1:(nrow(namesMatrix)/3), each=3)
  namesMatrix$type=c("obs","LLI","SSI")
  namesMatrix.m <- data.table::melt( data.table::as.data.table(namesMatrix),
                                     id.vars= c("type","id" ) )
  
  
  
  names(namesMatrix.m)<-c("type","idx", "system", "code")
  data.table::setkey(namesMatrix.m, type,  system, idx)
  
  hdr[["linesInHeader"]] <- i
  hdr[["scanList"]] <- ll
  hdr[["fieldsNames"]] <- fieldsNames
  hdr[["fieldsNamesMatrix"]] <- namesMatrix
  hdr[["fieldsNamesMatrix.m"]] <- namesMatrix.m
  hdr[["fieldsNamesMask"]] <- fieldsNamesMask
  hdr[["substr"]] <- list(from=from, to=to)
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
  hdr$linesInHeader
  epochData <- readLines(oo, n = epochHeader$nsats)
  # epochData <- scan(
  #   oo,
  #   what = character(),
  #   # sep=";" , #face separator
  #   what = hdr$scanList,
  #   quiet = TRUE,
  #   fill = TRUE,
  #   dec = hdr$dec,
  #   nlines = epochHeader$nsats
  # )
  # res <- lapply(epochData, function(str){
  #   substring(str, hdr$substr$from, hdr$substr$to)
  # })
  
  
  st <- stringr::str_sub(epochData, rep(hdr$substr$from, each=epochHeader$nsats), 
                                    rep(hdr$substr$to,each=epochHeader$nsats) )
 
 
  sv <- st[1:epochHeader$nsats]
  
  system <- stringr::str_sub(sv, rep(1,epochHeader$nsats), rep(1,epochHeader$nsats) )
  
  # unname(unlist((hdr$fieldsNamesMatrix[ , rep(system, each=3)])))
  raw <- as.numeric(st[-(1:epochHeader$nsats)])
  keep <- ( !is.na(dff$raw) )
  dff<-list(raw=raw[keep] )
  
  dff$type <- rep(rep( c("obs", "LLI","SSI"), each=epochHeader$nsats ), hdr$Fmax)[keep] 
  dff$system <- rep(system, hdr$Fmax*3)[keep]
  dff$idx <- rep(1:hdr$Fmax, each=(epochHeader$nsats*3))[keep]
  dff
  # dff2<-
  # data.table::setkey(dff2, type, idx, system)
  # 
  # dff3 <- hdr$fieldsNamesMatrix.m[dff2]
  # dff3
}
