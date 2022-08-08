#' Stazioni di Network GNSS (ITA e EUREF) 
#' 
#' La Rete GNSS Italiana è una rete composta da circa 700 stazioni permanenti 
#' GNSS diffuse sul territorio nazionale e alcune nazioni limitrofe. 
#' Con cadenza giornaliera viene eseguito il ricalcolo delle coordinate con il 
#' software Bernese v. 5.2 e con cadenza settimanale vengono emanate le coordinate 
#' aggiornate dalla combinazione delle 7 soluzioni giornaliere.
#' 
#' La rete EUREF la trovi  (\href{https://files.igs.org/pub/station/general/IGSNetwork.csv}{https://files.igs.org/pub/station/general/IGSNetwork.csv}) 
#'
#' @docType data
#' 
#'
#' @format A list with multiple objects of class \code{"sf"} with: 
#' \itemize{
#'   \item ITA - stations from (\href{http://retegnssveneto.cisas.unipd.it/gpsitn/}{http://retegnssveneto.cisas.unipd.it/gpsitn/})
#'   \item IGSNetwork - stations from (\href{https://files.igs.org/pub/station/general/IGSNetwork.csv}{https://files.igs.org/pub/station/general/IGSNetwork.csv})
#'   }
#'
#' @keywords datasets
#'
#' @references Rete GNSS Veneto
#' (\href{http://retegnssveneto.cisas.unipd.it/gpsitn}{Rete GNSS Veneto})
#'
#' @source \href{http://retegnssveneto.cisas.unipd.it/gpsitn}{Rete GNSS Veneto}
#'
#' @export
#' 
#' @examples
#' ## NOT RUN: data(World, metro, rivers, land)
#' ## NOT RUN: tmap::tmap_mode("plot") 
#' ## NOT RUN:   tmap::tm_shape(tmap::World) +
#' ## NOT RUN:   tmap::tm_borders("black", lwd = .5)  + 
#' ## NOT RUN:    tmap::tm_shape(stazioniGNSS$IGSNetwork) +
#' ## NOT RUN:             tmap::tm_symbols(col = "red", scale=.3 ) +
#' ## NOT RUN:             tmap::tm_legend(show = FALSE)
"stazioniGNSS"

  
#' @title Example data
#' --
#' @description   Some example files
#'
#' @docType data
#'  
#' @format A list with information on some examples RINEX files.
#' \itemize{
#'   \item obs.rover exampleRover.20o a RINEX 3.01 format file 
#'   \item nav       exampleNav.20o a RINEX 3.01 format file 
#'   \item obs.base  exampleObsBaseStation.20o a RINEX 3.01 format file 
#'   
#' } 
#' 
#' @keywords path to examples
#' 
#' @export
#' 
#' @examples
#' print(rRINEX::example.files)
#'
#' 
"example.files"

