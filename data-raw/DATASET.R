## code to prepare `DATASET` dataset goes here
######### STAZIONI ITA --------

stazioni <- read.table("data-raw/a.csv")



sc<-proj4::ptransform(stazioni[, c("V4","V5","V6")], 
                      src.proj = "+proj=geocent +ellps=GRS80 +units=m +no_defs", 
                      dst.proj = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

latlong<-as.data.frame(sc)/pi*180
df<-cbind( latlong[,1:2], stazioni[,2] )
names(df)<-c("x","y", "name")
stazioniGNSS.ITA<-sf::st_as_sf(df, coords=c("x","y"), crs=9000) 


######### STAZIONI IGS -----
url<-"https://files.igs.org/pub/station/general/IGSNetwork.csv"

stazioni<- readr::read_delim(url, delim = ",", trim_ws = T)
#rownames(stazioni)<-stazioni$nid
sc<-proj4::ptransform(stazioni[, c("X","Y","Z")], 
                      src.proj = "+proj=geocent +ellps=GRS80 +units=m +no_defs", 
                      dst.proj = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

latlong<-as.data.frame(sc)/pi*180
df<-cbind( latlong[,1:2], stazioni[,1] )
names(df)<-c("x","y", "name")
stazioniGNSS.IGSNetwork<-sf::st_as_sf(df, coords=c("x","y"), crs=9000  )
# mapview()+
#  # mapview(stazioniGNSS.IGSNetwork)+
#   mapview(stazioniGNSS.ITA)


######################
 

stazioniGNSS<-list(IGSNetwork=stazioniGNSS.IGSNetwork, 
                   ITA=stazioniGNSS.ITA)
example.files<- list(  obs.rover=system.file("extdata", "example.20o", package = "rRINEX"),
                                nav=system.file("extdata", "example.20o", package = "rRINEX"),
                                obs.base=system.file("extdata", "example.20o", package = "rRINEX")
)
usethis::use_data(stazioniGNSS, example.files, internal=T, overwrite = TRUE)


