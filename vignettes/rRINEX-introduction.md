---
title: "rRINEX-introduction"
output: 
  rmarkdown::html_vignette:
    keep_md: true 
    toc: true
self_contained: no
vignette: >
  %\VignetteIndexEntry{rRINEX-introduction}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
  \usepackage[utf8]{inputenc} 
---


  
## Get started  

You  have two choices, Italy's CORS stations, or EUREF CORS stations.  
Below is an example plotting both in red and blue respectively.





## RTKLIB

Upon loading the library , the user will be asked to specify the path were RTKLIB bin folder is located in the machine running R.  

It is strongly advised that you install  [RTKLIB](http://www.rtklib.com){target=_blank} to use all of the functionalities of rRINEX.  



```r
#library(lattice)
#library(tmap) 
library(rRINEX)
```

### Example data  

The reference to some example  RINEX files.  


```r
ef<-rRINEX::example.files
print(ef)
#> $obs.rover
#> [1] "/usr/local/lib/R/site-library/rRINEX/extdata/example.20o"
#> 
#> $nav
#> [1] "/usr/local/lib/R/site-library/rRINEX/extdata/example.20o"
#> 
#> $obs.base
#> [1] "/usr/local/lib/R/site-library/rRINEX/extdata/example.20o"
```

### Get and plot approximate  position of survey from RINEX file  

Get and plot the approximate position of the survey using the 
RINEX observation file.   

NB: --- *not* all RINEX observation files have this information.


```r

info<- rRINEX::getApproxPositionFromRINEX.OBS.header(ef$obs.rover)
if(is.na(info)){
  message("problem here")
} else {
  print(sprintf("Long. %f and Lat. %f ", info[[1]], info[[2]] ) ) 
## Window buffer in degrees
  buffer<-0.001
# area <- get_stamenmap(bbox = c(left = info[[1]] - buffer, bottom =info[[2]] - buffer, 
#                                   right = info[[1]]+buffer, top = info[[2]] + buffer), zoom=14)
#  
}
#> Warning in if (is.na(info)) {: the condition has length > 1 and only the first
#> element will be used
#> [1] "Long. 11.933910 and Lat. 45.672808 "
```


### Get position of closest base stations to survey  

Get and plot the approximate position of the survey using the 
RINEX observation file.   

NB: --- *not* all RINEX observation files have this information.


```r
# rinex.info<- rRINEX::getInfoFromRINEX.OBS.header(ef$obs.rover)
# getFile.Veneto(rinex.info$timestamp, "PADO")
# point<- sf::st_sfc(sf::st_point(rinex.info$latlong), crs = 9000)
# 
# sf.close<-rRINEX::getClosestStations(ef$obs.rover)
#  
#  tmap::tmap_mode("view")
#  tmap::tm_shape(sf.close) +
#      tmap::tm_symbols(col="blue", scale=.99) +
#  tmap::tm_shape(point) +
#      tmap::tm_symbols(col="red", scale=1.5)
```
