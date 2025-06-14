
list.of.packages <- c("leaflet", "leafem", "shiny", "shinydashboard", "shinydashboardPlus",
                      "shinyWidgets", "shinyjs", "shinyjqui", "plotly", "leaflet.extras" )
ip <- installed.packages()[,"Package"]
if(!is.element("pacman", ip)) {
  message("Please wait while we install the necessary packages....")
  install.packages("pacman")
}


pacman::p_load(list.of.packages, character.only = TRUE)

 

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
