library(sf)
library(proj4)
stazioni<- readr::read_delim("data-raw/stazioni.csv", delim = " ", trim_ws = T, col_names = c("id","nid", "fid", "X", "Y", "Z", "l") )
#rownames(stazioni)<-stazioni$nid
sc<-proj4::ptransform(stazioni[, c("X","Y","Z")], 
                      src.proj = "+proj=geocent +ellps=GRS80 +units=m +no_defs", 
                      dst.proj = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

latlong<-as.data.frame(sc)/pi*180
df<-cbind( latlong[,1:2], stazioni[,2:3] )
stazioniGNSSita<-sf::st_as_sf(df, coords=c("x","y") )
save(stazioniGNSSita, file="data/stazioniGNSSita.rda")
