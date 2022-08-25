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
        
        dashboardHeader(title=customHeaderPanel("rtklibR"), disable=FALSE,
                        
                        tags$li( id="internetConnection_holder",
                                 title="Clicca per misurare la velocità della tua rete!",
                                class = "dropdown", style=" margin-top:7px; cursor:pointer;",
                        actionBttn("internetConnectionButton", NULL, size = "sm", 
                                   style = "minimal", color="success",
                                   img(src = "https://raw.githubusercontent.com/tomojitakasu/RTKLIB/master/app/icon/rtk2.ico",
                                       height = "50px") )
                        )
                        ),
        dashboardSidebar(  ),
        dashboardBody(
            useShinyjs(),
            
            
            
            PLOTWINDOW(),
            
            
            leafletOutput("rinexMap"),
            
            div(
                id = "loading-spinner",
                div(class = "sp sp-circle"),
                div(id = "loading-spinner-content",  "Caricamento in corso")
            ) ,
            
        )
    )
    
    
    
}
