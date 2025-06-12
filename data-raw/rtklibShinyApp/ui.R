source("uiWindows.R",local = TRUE)

customHeaderPanel <- function(title, windowTitle = title) {
    tagList(
        tags$head( 
            tags$script(src="myFunc.js?v=288d"),
            includeCSS("www/extra.css")
        ) 
    )
}



function(req) {
    dashboardPage(
        skin = "black",
        title = "rtklibR",
        
        
        dashboardHeader(
            title = customHeaderPanel("rtklibR"),
            disable = FALSE,
            
            tags$li(
                id = "rtkplot",
                title = "RtkPLOT",
                class = "dropdown",
                style = " margin-top:7px; cursor:pointer;",
                onclick = "$('#'+this.id.toUpperCase() ).show(100)",
                img(src = "https://raw.githubusercontent.com/tomojitakasu/RTKLIB/master/app/icon/rtk2.ico",
                    height = "36px")
            ),
            
            tags$li(
                id = "rtkpost",
                title = "RtkPOST",
                class = "dropdown",
                onclick = "$('#'+this.id.toUpperCase() ).show(100)",
                style = " margin-top:7px; cursor:pointer;",
                img(src = "https://raw.githubusercontent.com/tomojitakasu/RTKLIB/master/app/icon/rtk1.ico",
                    height = "36px")
            ),
            
            tags$li(
                id = "rtkget",
                title = "RtkGET",
                class = "dropdown",
                style = " margin-top:7px; cursor:pointer;",
                onclick = "$('#'+this.id.toUpperCase() ).show(100)",
                img(src = "https://raw.githubusercontent.com/tomojitakasu/RTKLIB/master/app/icon/rtk3.ico",
                    height = "36px")
            )
        ), 
        dashboardSidebar(  ),
        dashboardBody(
            useShinyjs(),
            
            
            
            PLOTWINDOW(),
            POSTPROCESSWINDOW(),
            CONVWINDOW(),
            OPTIONSWINDOW(),
            leafletOutput("rinexMap"),
            
            div(
                id = "loading-spinner",
                div(class = "sp sp-circle"),
                div(id = "loading-spinner-content",  "Caricamento in corso")
            ) ,
            
        )
    )
    
    
    
}
