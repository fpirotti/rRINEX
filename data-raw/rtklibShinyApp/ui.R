

customHeaderPanel <- function(title, windowTitle = title) {
    tagList(
        tags$head(
            tags$link(
                rel = "apple-touch-icon",
                sizes = "57x57",
                href = "apple-icon-57x57.png"
            ),
            tags$link(
                rel = "apple-touch-icon",
                sizes = "60x60",
                href = "apple-icon-60x60.png"
            ),
            tags$link(
                rel = "apple-touch-icon",
                sizes = "72x72",
                href = "apple-icon-72x72.png"
            ),
            tags$link(
                rel = "apple-touch-icon",
                sizes = "76x76",
                href = "apple-icon-76x76.png"
            ),
            tags$link(
                rel = "apple-touch-icon",
                sizes = "114x114",
                href = "apple-icon-114x114.png"
            ),
            tags$link(
                rel = "apple-touch-icon",
                sizes = "120x120",
                href = "apple-icon-120x120.png"
            ),
            tags$link(
                rel = "apple-touch-icon",
                sizes = "144x144",
                href = "apple-icon-144x144.png"
            ),
            tags$link(
                rel = "apple-touch-icon",
                sizes = "152x152",
                href = "apple-icon-152x152.png"
            ),
            tags$link(
                rel = "apple-touch-icon",
                sizes = "180x180",
                href = "apple-icon-180x180.png"
            ),
            tags$link(
                rel = "icon",
                type = "image/png",
                sizes = "192x192",
                href = "android-icon-192x192.png"
            ),
            tags$link(
                rel = "icon",
                type = "image/png",
                sizes = "32x32",
                href = "favicon-32x32.png"
            ),
            tags$link(
                rel = "icon",
                type = "image/png",
                sizes = "96x96",
                href = "favicon-96x96.png"
            ),
            tags$link(
                rel = "icon",
                type = "image/png",
                sizes = "16x16",
                href = "favicon-16x16.png"
            ),
            tags$meta(name = "msapplication-TileColor", content = "#ffffff"),
            tags$meta(name = "theme-color", content = "#ffffff"),
        )
        # , tags$h1(windowTitle, style = "margin: 9px 0px;  font-size: 24px; text-shadow: 1px 1px 10px; ")
    )
}



function(req) {
    dashboardPage(
        skin = "black",
        title = "rtklibR",
        dashboardHeader(disable=FALSE),
        dashboardSidebar(width=400,
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
                         ),
        dashboardBody(
            useShinyjs(),
            
            leafletOutput("rinexMap"),
            
            div(
                id = "loading-spinner",
                div(class = "sp sp-circle"),
                div(id = "loading-spinner-content",  "Caricamento in corso")
            ) ,
            
        )
    )
    
    
    
}
