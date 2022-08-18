#' rinexApp
#'
#' @description 
#' you can run rRINEX::rinexApp() if you have leaflet and shiny installed
#'  importFrom magrittr "%>%"
#' @return a shiny application for uploading RINEX obs and nav files
#' and plot info
#' @export
#'
#' @examples
#' #rRINEX::rinexApp()
rinexApp <- function() {

  ok <- setdiff(c("leaflet","shiny", "leafem"), rownames(utils::installed.packages()) )
  if(length(ok) > 0){
    message("Please install the following packages before running: 
            ", paste(ok, collapse=" - ")   )
    return(NULL)
  }
  
  leaflet.object <-
    leaflet::leaflet() %>%
    leaflet::addTiles(group = "OpenStreetMap") %>%
    leaflet.extras::addBingTiles(
      "Satellite BING maps",
      group = "BING",
      apikey = "AjvjPYuoA4IgNeooKvodDLcxbVL1F8RdIxXUeYsb6PgiVapURz_PbbWvOxVKmNps",
      imagerySet = c("Aerial")
    ) %>%
    leaflet::addTiles(urlTemplate  = '', group = "Blank") %>%
    leaflet::addLayersControl(
      position = ("topright"),
      baseGroups = c("Blank", "OpenStreetMap", "BING"),
      overlayGroups = c("RINEX OBS. Position", "RINEX OBS. Points"),
      leaflet::layersControlOptions(autoZIndex = FALSE, collapsed = FALSE)
    ) %>%

    leaflet::showGroup("OpenStreetMap")   %>%
    leaflet::showGroup("RINEX OBS. Position")   %>%
    
    leaflet::addScaleBar("bottomright") %>%
    leaflet::addMiniMap(tiles = "OpenStreetMap",
                        toggleDisplay = TRUE,
                        position = "topright") %>%
    htmlwidgets::onRender(
      "
    function(el, x) {
      myMap = this;
      Shiny.setInputValue('leafletRendered',true, {priority: \"event\"});

    }"
    ) %>% 
    leaflet::setView(lng = 11.970140, lat = 46, zoom = 7)  %>%
    leafem::addMouseCoordinates()

 
