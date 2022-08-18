#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
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
})
