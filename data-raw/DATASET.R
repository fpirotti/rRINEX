## code to prepare `DATASET` dataset goes here
######### STAZIONI ITA --------

stazioni <- read.table("data-raw/a.csv")

sc<-proj4::ptransform(stazioni[, c("V4","V5","V6")], 
                      src.proj = "+proj=geocent +ellps=GRS80 +units=m +no_defs", 
                      dst.proj = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

latlong<-as.data.frame(sc)/pi*180
df<-cbind( latlong[,1:2], stazioni[,2] )
names(df)<-c("x","y", "name")
# stazioniGNSS.ITA<-sf::st_as_sf(df, coords=c("x","y"), crs=9000) 
stazioniGNSS.ITA<- df 


######### STAZIONI IGS -----
url<-"https://files.igs.org/pub/station/general/IGSNetwork.csv"
stz <- readLines(url)
stzsp <- strsplit(stz[[1]], ",")[[1]]
names(stzsp) <- gsub("#", "", stzsp)
stzsp[1:7] <- list(StationName=character(),
                        X = double(),
                        Y = double(),
                        Z = double(),
                         Latitude = double(),
                         Longitude = double(),
                         Height = double()  ) 
stazioni<- scan(text=stz, skip = 1,  what=stzsp, sep=",")

# stazioni<- readr::read_delim(url, delim = ",", trim_ws = T)
#rownames(stazioni)<-stazioni$nid
# sc<-proj4::ptransform(stazioni[, c("X","Y","Z")], 
#                       src.proj = "+proj=geocent +ellps=GRS80 +units=m +no_defs", 
#                       dst.proj = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
# 
# latlong<-as.data.frame(sc)/pi*180
# df<-cbind( latlong[,1:2], stazioni[,1] )
df <- stazioni[ c("Longitude", "Latitude", "StationName")]
names(df)<-c("x","y", "name")
# stazioniGNSS.IGSNetwork<-sf::st_as_sf(df, coords=c("x","y"), crs=9000  )
stazioniGNSS.IGSNetwork<- df 
# mapview()+
#  # mapview(stazioniGNSS.IGSNetwork)+
#   mapview(stazioniGNSS.ITA)
rinex.satellite_system <- c(
  "G"="GPS",
  "R"="GLONASS",
  "E"="Galileo",
  "J"="QZSS",
  "C"="BDS",
  "I"="IRNSS",
  "S"="SBAS payload",
  "M"="Mixed"
)

rinex.type <- c(
  "C" = "Code / Pseudorange",
  "L" = "Phase",
  "D" = "Doppler",
  "S" = "Raw signal strength(carrier to noise ratio)",
  "I" = "Ionosphere phase delay",
  "X" = "Receiver channel numbers"
)
rinex.band   <-    c("1" = "L1 (GPS, QZSS, SBAS,BDS), G1 (GLO), E1 (GAL), B1 (BDS)",
                        "2" = "L2 (GPS, QZSS), G2 (GLO), B1-2 (BDS)",
                        "4" = "G1a (GLO)",
                        "5" = "L5 (GPS, QZSS, SBAS, IRNSS), E5a (GAL), B2/B2a (BDS)",
                        "6" = "E6 (GAL), B3 (BDS), G2a (GLO)",
                        "7" = "E5b (GAL), B2/B2b (BDS)",
                        "8" = "E5a+b (GAL), B2a+b (BDS)" ) 

rinex.attribute <- c("A" = "A channel (GAL, IRNSS, GLO)", 
     "B" = "B channel (GAL, IRNSS, GLO)", 
     "C" = "C channel (GAL, IRNSS), C code-based (SBAS,GPS,GLO,QZSS)", 
     "D" = "Semi-codeless (GPS), Data Channel (BDS)", 
     "I" = "I channel (GPS,GAL, QZSS, BDS)", 
     "L" = "L channel (L2C GPS, QZSS), P channel (GPS, QZSS)", 
     "M" = "M code-based (GPS)", 
     "N" = "Codeless (GPS)", 
     "P" = "P code-based (GPS,GLO), Pilot Channel (BDS)", 
     "Q" = "Q channel (GPS,GAL,QZSS,BDS)", 
     "S" = "D channel (GPS, QZSS), M channel (L2C GPS, QZSS)", 
     "W" = "Based on Z-tracking (GPS)(see text)", 
     "X" = "B+C channels (GAL, IRNSS), I+Q channels (GPS,GAL, QZSS,BDS), M+L channels (GPS, QZSS), D+P channels (GPS, QZSS, BDS)", 
     "Y" = "Y code-based (GPS)", 
     "Z" = "A+B+C channels (GAL), D+P channels (BDS)" ) 
######################

template.rinex.data <- data.table::data.table(secondsFromStartTime=double(), 
                                              type=factor(levels = names(rinex.type)), 
                                              band=factor(levels = names(rinex.band)), 
                                              attribute=factor(levels = names(rinex.attribute)),
                                              ss=factor(levels = names(rinex.satellite_system))
                                              ) 
 

stazioniGNSS<-list(IGSNetwork=stazioniGNSS.IGSNetwork, 
                   ITA=stazioniGNSS.ITA)
example.files<- list(  obs.rover=system.file("extdata", "example.20o", package = "rRINEX"),
                       obs2.base=system.file("extdata", "pado3480.20n.Z", package = "rRINEX"),
                       obs.crx.rover=system.file("extdata", "BZRG00ITA_S_20203460000_01D_01S_MO.crx.tar.gz", package = "rRINEX"),
                                nav=system.file("extdata", "SMAR00USA_R_20203460949.20n", package = "rRINEX"),
                                obs.base=system.file("extdata", "example.20o", package = "rRINEX")
                    )

vocab <- list(
  rinex.satellite_system=rinex.satellite_system,
  rinex.band=rinex.band,
  rinex.attribute=rinex.attribute,
  rinex.type=rinex.type
)
file.exists("/archivio/R/shared/rRINEX/src/RTKLIB/src/rinex.so")
system("ls")

ffs <- list.files("src/RTKLIB-2.4.3-b34/src", pattern = "\\.c$")
system(sprintf("cd src/RTKLIB-2.4.3-b34/src; R CMD SHLIB  rtklib.h %s",
               paste0(ffs, collapse = " ") ) )

system(sprintf("cd src/RTKLIB-2.4.3-b34/src; R CMD SHLIB rtklib.h" ) )
### 1. compilare iers in lib
dyn.load("/archivio/R/shared/rRINEX/src/RTKLIB-2.4.3-b34/src/convgpx.so")
.C("double_me", x = as.integer(5))

usethis::use_data(stazioniGNSS, example.files, vocab, internal=T, overwrite = TRUE)


