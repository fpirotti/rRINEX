#' Stazioni della Rete GNSS Italiana
#' 
#' La Rete GNSS Italiana è una rete composta da circa 700 stazioni permanenti 
#' GNSS diffuse sul territorio nazionale e alcune nazioni limitrofe. 
#' Con cadenza giornaliera viene eseguito il ricalcolo delle coordinate con il 
#' software Bernese v. 5.2 e con cadenza settimanale vengono emanate le coordinate 
#' aggiornate dalla combinazione delle 7 soluzioni giornaliere.
#'
#' @docType data
#'
#' @usage data(stazioniGNSSita)
#'
#' @format An object of class \code{"sf"}.
#'
#' @keywords datasets
#'
#' @references Rete GNSS Veneto
#' (\href{http://retegnssveneto.cisas.unipd.it/gpsitn}{Rete GNSS Veneto})
#'
#' @source \href{http://retegnssveneto.cisas.unipd.it/gpsitn}{Rete GNSS Veneto}
#'
#' @examples
#' data(stazioniGNSSita)  
#' stazioniGNSSita[1,]
"stazioniGNSSita"