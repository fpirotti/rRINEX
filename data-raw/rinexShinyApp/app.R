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




## SERVER ----------
  server <- function(input, output, session) {

    globReact <- shiny::reactiveValues(rinex.hdr=NULL, rinex.data=NULL)
    
    output$rinexMap <- leaflet::renderLeaflet(leaflet.object)

    output$plotObs <- plotly::renderPlotly({
      # n <- 1e5
      # x <- rnorm(n)
      # y <- 2*x + rnorm(n, sd = 5)
      # 
      # fig <- plot_ly(x = x, y = y, alpha  = 0.01)
      # fig <- fig %>% toWebGL()
      # 
      # fig
    })
    shiny::observeEvent(globReact$rinex.hdr, {
      
      print(globReact$rinex.hdr)
      ll <- ifelse(is.null(globReact$rinex.hdr[["APPROX.POSITION.LATLONG"]]), 
                    "",
                    paste0("<br>Long:", globReact$rinex.hdr[["APPROX.POSITION.LATLONG"]][[1]],
                           "<br>Lat:",   globReact$rinex.hdr[["APPROX.POSITION.LATLONG"]][[2]]) )
      ttf <- ifelse(is.null(globReact$rinex.hdr[["TIME.OF.FIRST.OBS"]]), 
                    "Time of First Observation NOT found",
                    format(globReact$rinex.hdr[["TIME.OF.FIRST.OBS"]]) )
      text <- paste0("RINEX Version: ", globReact$rinex.hdr$version,
                     "<br>Hatanaka compressed:", globReact$rinex.hdr$is_crinex,
                     "<br>System: ", rRINEX::vocab$rinex.satellite_system[[globReact$rinex.hdr$systems]],
                     "<br>Type: ", globReact$rinex.hdr$rinextype,
                     "<br>File type: ", globReact$rinex.hdr$filetype,
                     "<br>Start Time: ", ttf, ll )
      
      output$rinexInfo<-  shiny::renderUI(shiny::HTML(text))
      
      if(!is.null(globReact$rinex.hdr[["APPROX.POSITION.LATLONG"]])){
        leaflet::leafletProxy('rinexMap') %>%
          leaflet::clearGroup(group = 'RINEX OBS. Position') %>% 
          leaflet::addMarkers(globReact$rinex.hdr[["APPROX.POSITION.LATLONG"]][[1]], 
                              globReact$rinex.hdr[["APPROX.POSITION.LATLONG"]][[2]], 
                              popup = text) %>%
          leaflet::flyTo(globReact$rinex.hdr[["APPROX.POSITION.LATLONG"]][[1]], 
                             globReact$rinex.hdr[["APPROX.POSITION.LATLONG"]][[2]],
                             12)
      }
      
    })

    shiny::observeEvent(input$rinexfile, {
      fln <- input$rinexfile$datapath
      

    })
  }

  shiny::shinyApp(ui, server)
}
#
# library(rRINEX)
# library(magrittr)
#  library(leaflet)
# library(plotly)
# # # library(RPostgreSQL)
  rinexApp()
