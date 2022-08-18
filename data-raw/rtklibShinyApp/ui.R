#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shiny::fluidPage(
    
    shinyjs::useShinyjs(),
    
    shiny::fluidRow(
        shiny::sidebarPanel(
            width = 6,
            
            shiny::fluidRow(
                shiny::column(width = 12, shiny::fileInput("rinexfile", "RINEX file") ),
                
            ),
            
            shiny::fluidRow(
                
                shiny::column(
                    width = 12, shiny::uiOutput("rinexInfo")
                )
            ),
            
            plotly::plotlyOutput("plotObs")
        ), 
        
        
        
        shiny::column(width = 6,
                      leaflet::leafletOutput("rinexMap", height = "98vh")
        )
    ))