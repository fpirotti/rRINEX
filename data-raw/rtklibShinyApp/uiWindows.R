PLOTWINDOW <- function(){
  
 shinyjqui::jqui_draggable(  
    column(12, style="displan:none; position:absolute;z-index:99999; width:calc( 100vw - 60px ); top:5px; left:10px;", 
           shinydashboardPlus::box( id= "RTKPLOT",
                title = HTML(paste0(img(src="rtk2.ico",height = "24px"), " RTKPLOT                ")), 
                 width = NULL,
                collapsed = F,
                # enable_label = F, 
                # label_text = "",
                # label_#status = "danger",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,  
                closable = TRUE,  
                plotly::plotlyOutput("rtkplot.plotly")
           ) 
    ),
    options = list(
      cursor = "move",
      zIndex = 99999999999,
      stack = ".container",
      opacity = 0.5,
      handle = ".box-header"
    )
  )
    
}



POSTPROCESSWINDOW <- function(){
  
  shinyjqui::jqui_draggable(  
    column(12, style="displan:none; position:absolute;z-index:99999; width:calc( 100vw - 60px ); top:5px; left:10px;", 
           shinydashboardPlus::box( id= "RTKPOST",
                title = HTML(paste0(img(src="rtk1.ico",height = "24px"), " RTKPOST        ")), 
                closable = TRUE,  width = NULL,
                collapsed = F,
                # enable_label = F, 
                # label_text = "",
                # label_#status = "danger",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,   
                shinyWidgets::prettyCheckbox("tstart", "Time Start", inline = T),
                shinyWidgets::prettyCheckbox("tend", "Time End", inline = T),
                shinyWidgets::prettyCheckbox("interval", "Interval", inline = T),
                shiny::fluidRow(
                  column(4, shinyWidgets::airDatepickerInput("tstartval",timepicker = TRUE)),
                  column(4, shinyWidgets::airDatepickerInput("tendval",timepicker = TRUE)),
                  column(4, shinyWidgets::airDatepickerInput("interval",timepicker = TRUE))
                ),
                fileInput("file.obs", "RTCM, RCV RAW or RINEX OBS"),
                fileInput("file.nav1", "RINEX OBS/NAV/GNAV/HNAV/..."),
                fileInput("file.nav2", "RINEX OBS/NAV/GNAV/HNAV/..."),
                shiny::fluidRow(
                  column(2, shinyWidgets::actionBttn("rtkpost.plot.window","Plot...", style="fill", size="sm") ),
                  column(2, shinyWidgets::actionBttn("rtkpost.view.window","View...", style="fill", size="sm") ),
                  column(2, shinyWidgets::actionBttn("rtkpost.tokml.window","ToKML...", style="fill", size="sm") ),
                  column(2, shinyWidgets::actionBttn("rtkpost.options.window", "Options...", style="fill", size="sm") ),
                  column(2, shinyWidgets::actionBttn("rtkpost.execute.window", "Execute...", style="fill", size="sm") ),
                  column(2, shinyWidgets::actionBttn("rtkpost.close.window", "Close...",  style="fill", size="sm") )
                )
                
           ) 
    ),
    options = list(
      cursor = "move",
      zIndex = 99999999999,
      stack = ".container",
      opacity = 0.5,
      handle = ".box-header"
    )
  )
  
}


CONVWINDOW <- function(){
  
  shinyjqui::jqui_draggable(  
    column(12, style="displan:none; position:absolute;z-index:99999; width:calc( 100vw - 60px ); top:5px; left:10px;", 
           shinydashboardPlus::box( id= "RTKCONV",
                                    title = HTML(paste0(img(src="rtk3.ico",height = "24px"), " RTKCONV        ")), 
                                    closable = TRUE,  width = NULL,
                                    collapsed = F,
                                    status = "primary", 
                                    # enable_label = F, 
                                    # label_text = "",
                                    # label_#status = "danger",
                                    # status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,   
                                    div("hei")
                                    
           ) 
    ),
    options = list(
      cursor = "move",
      zIndex = 99999999999,
      stack = ".container",
      opacity = 0.5,
      handle = ".box-header"
    )
  )
  
}




## RTKOPTIONS ----
OPTIONSWINDOW <- function(){
  
  shinyjqui::jqui_draggable(  
    column(12, style="displan:none; position:absolute;z-index:99999; width:calc( 100vw - 60px ); top:5px; left:10px;", 
           shinydashboardPlus::box( id= "RTKOPTIONS", 
                                    title = HTML(paste0(icon("gears"), " OPTIONS        ")), 
                                    closable = TRUE,  width = NULL,
                                    collapsed = F,
                                    status = "primary", 
                                    # enable_label = F, 
                                    # label_text = "",
                                    # label_#status = "danger",
                                    # status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,   
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Setting1", 
                                                         shiny::fluidRow(
                                                           column(6, div("Positioning mode") ),
                                                           column(6, shinyWidgets::pickerInput("settings1.positioning.mode", choices = c("Single", "....")  ) )
                                                         ),
                                                         shiny::fluidRow(
                                                           column(6, div("Frequency / Filter Type") ),
                                                           column(3, shinyWidgets::pickerInput("settings1.frequency", choices = c("L1/L2", "....")  ) ),
                                                           column(3, shinyWidgets::pickerInput("settings1.filter.type", choices = c("Forward", "Backward", "Combined")  ) )
                                                         ),
                                                         shiny::fluidRow(
                                                           column(6, div("Positioning mode") ),
                                                           column(6, shinyWidgets::pickerInput("settings1.positioning.mode", choices = c("Single", "....")  ) )
                                                         ),
                                                         checkboxGroupButtons(
                                                           inputId = "somevalue", label = "Make a choice :", 
                                                           choices = c("GPS", "GLO", "GALILEO", "QZSS", "SBAS", "Beidu")#, 
                                                           #justified = TRUE, status = "primary"
                                                         )
                                                         ),
                                                tabPanel("Setting2", 
                                                         ),
                                                tabPanel("Output", 
                                                         ),
                                                tabPanel("Stats", 
                                                         ),
                                                tabPanel("Positions", 
                                                         ),
                                                tabPanel("Files", 
                                                         ),
                                                tabPanel("Misc", 
                                                         )
                                    )
                                    
           ) 
    ),
    options = list(
      cursor = "move",
      zIndex = 99999999999,
      stack = ".container",
      opacity = 0.5,
      handle = ".box-header"
    )
  )
  
}