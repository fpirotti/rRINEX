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

  
#' example.files
#' @title Example data
#' --
#' @description   Some example files
#'
#' @docType data
#'  
#' @format A list with  some examples RINEX files.
#' \itemize{
#'   \item obs.rover exampleRover.20o a RINEX 3.01 format file 
#'   \item nav       exampleNav.20o a RINEX 3.01 format file 
#'   \item obs.base  exampleObsBaseStation.20o a RINEX 3.01 format file 
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


#' vocab
#' @title vocab
#' --
#' @description   vocab
#'
#' @docType data
#'  
#' @format A list with information on  RINEX terminology
#' \itemize{
#'   \item rinex.type exampleRover.20o a RINEX 3.01 format file 
#'   
#'   | **Type** | **Value**  | 
#'   | ----- | ------------ | 
#'   | **J** | QZSS         |                             
#'   | **C** | BDS          |                           
#'   | **I** | IRNSS        |                            
#'   | **S** | SBAS payload |                                    
#'   | **M** | Mixed        |                            
#'                                      
#'   \item rinex.band 
#'       
#'   | **Band** | **type**  | 
#'   | ----- | ------------------------------------------- | 
#'   | 1 |     L1 (GPS, QZSS, SBAS,BDS)                  |                                
#'   |   |     G1 (GLO)                                  |                
#'   |   |     E1 (GAL)                                  |                
#'   |   |     B1 (BDS)                                  |                
#'   | 2 |     L2 (GPS, QZSS)                            |                      
#'   |   |     G2 (GLO)                                  |                
#'   |   |     B1-2 (BDS)                                |                  
#'   | 4 |     G1a (GLO)                                 |                 
#'   | 5 |     L5 (GPS, QZSS, SBAS,   IRNSS)             |       
#'   |   |     E5a (GAL)                                 |                 
#'   |   |     B2/B2a (BDS)                              |                    
#'   | 6 |     E6 (GAL)                                  |                
#'   |   |     L6 (QZSS)                                 |                 
#'   |   |     B3 (BDS)                                  |                
#'   |   |     G2a (GLO)                                 |                 
#'   | 7 |     E5b (GAL)                                 |                 
#'   |   |     B2/B2b (BDS)                              |                    
#'   | 8 |     E5a+b (GAL)                               |                   
#'   |   |     B2a+b (BDS)                               |                   
#'   |   |                                               | 
#'        
#'   \item rinex.type    
#'   | **Code** | **type**  | 
#'   | ----- | ------------------------------------------- |                                          
#'   | **C** | Code / Pseudorange                          |
#'   | **L** | Phase                                       |          
#'   | **D** | Doppler                                     |     
#'   | **S** | Raw signal strength(carrier to noise ratio) |  
#'   | **I** | Ionosphere phase delay                      |                
#'   | **X** | Receiver channel numbers                    |                   
#'    
#'   \item rinex.satellite_system  
#' | **Code**  | **GNSS System**       |
#' | ----- | ----------------- |
#' | **G** | GPS               |
#' | **R** | GLONASS           |
#' | **E** | Galileo           |
#' | **C** | BeiDou (BDS)      |
#' | **J** | QZSS              |
#' | **I** | IRNSS             |
#' | **S** | SBAS              |
#' | **M** | Mixed or Multiple |
#' 
#' @export
#' 
#' @examples
#' print(rRINEX::vocab)
#'
#' 
"vocab"