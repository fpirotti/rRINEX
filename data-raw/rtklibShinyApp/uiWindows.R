PLOTWINDOW <- function(){
  
 shinyjqui::jqui_draggable(  
    column(12, style="displan:none; position:absolute;z-index:99999; width:calc( 100vw - 60px ); top:5px; left:10px;", 
           box( id= "RTKLib.PLOT",
                title = HTML(paste0(icon("chart-line", verify_fa = FALSE), " RTKLib PLOT                ")), 
                closable = TRUE,  width = NULL,
                collapsed = F,
                enable_label = T, 
                label_text = "",
                label_status = "danger",
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,   
                div("hei"),
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
                fileInput("file.nav2", "RINEX OBS/NAV/GNAV/HNAV/...")
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