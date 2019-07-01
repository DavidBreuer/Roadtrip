
library(shiny)

# UI element only consists of uiOutput due to password protection
ui <- fluidPage(
  dqshiny::dq_busy(),
  tags$head(
    tags$title("USA 2019 Roadtrip"),
    tags$link(href = "style.css", rel = "stylesheet"),
    tags$script(src = "scripts.js")
  ),
  uiOutput("content")
)

# server logic
server <- function(input, output, session) {
  
  # password modal
  showModal(modalDialog(
    passwordInput("pw", "Please enter password:"),
    title = "Authorization",
    footer = div(actionButton("dismiss", "Dismiss"), actionButton("ok", "OK", class = "btn-primary"))
  ))
  
  # wrong password delay
  delay <- 0L
  
  # json file containing all page information (position/title/images/layout)
  pages <- jsonlite::read_json("www/pages.json", simplifyVector = TRUE, simplifyDataFrame = FALSE)
  
  # password dismiss ... leads to google
  observeEvent(input$dismiss, dqshiny::run_js("dismiss"))
  
  # password input
  observeEvent(input$ok, {
    if (input$pw == "mypw") {
      removeModal()
      
      # btn for next page
      observeEvent(input$prevPage, {
        val <- input$currPage - 1L
        if (val < 1L) val <- length(pages)
        updateNumericInput(session, "currPage", value = val)
      })
      
      # btn for last page
      observeEvent(input$nextPage, {
        val <- input$currPage + 1L
        if (val > length(pages)) val <- 1L
        updateNumericInput(session, "currPage", value = val)
      })
      
      # ui containing the sidebar and the main panel showing the images
      output$content <- renderUI({
        fluidRow(
          # sidebar
          div(
            id = "sidecol",
            wellPanel(
              style = "padding: 5px;",
              dqshiny::icon_state_button(
                "hideshow", c("chevron-left", "chevron-right"),
                title = "Seitenleiste ein-/ausblenden (zum Navigieren die Pfeiltasten nutzen)"
              ),
              textOutput("page", inline = TRUE),
              dqshiny::icon_state_button("play", c("play", "pause"), title = "Starte/Stoppe Slideshow"),
              dqshiny::icon_state_button("fullscreen", c("expand", "compress"), title = "Vollbildmodus"),
              uiOutput("title"),
              div(
                id = "mapWrapper",
                div(
                  id = "mapBtns",
                  actionButton("restore", NULL, icon("compress"), title = "Karte auf Standardgröße"),
                  actionButton("maximize", NULL, icon("expand"), title = "Karte auf Vollbildgröße")
                ),
                leaflet::leafletOutput("map", height = "100%")
              ),
              actionButton("prevPage", NULL, icon("chevron-left"), title = "Letzte Seite"),
              numericInput("currPage", NULL, 1, 1, length(pages)),
              actionButton("nextPage", NULL, icon("chevron-right"), title = "Nächste Seite"),
              checkboxInput("autofocus", "automatischer Kartenfokus", TRUE),
              textOutput("description")
            )
          ),
          # main image panel
          div(
            id = "imageWrapper",
            div(id = "img1"), div(id = "img2"), div(id = "img3"),
            div(id = "img4"), div(id = "img5"), div(id = "img6")
          ),
          # lightbox
          div(
            id = "lightbox", class = "hidden",
            actionButton("prevLight", NULL, icon("chevron-left")),
            dqshiny::hidden(numericInput("currLight", NULL, 0L)),
            actionButton("closelb", NULL, icon("times")),
            actionButton("nextLight", NULL, icon("chevron-right"))
          ),
          tags$script("initLightbox();")
        )
      })
    } else {
      # wrong password
      if (delay > 9L) dqshiny::run_js("dismiss")
      Sys.sleep(delay)
      delay <<- delay + 1L
    }
  })
  
  # restore map size
  observeEvent(input$restore, {
    dqshiny::remove_class("mapWrapper", "maximize")
    dqshiny::run_js("mapReset")
  })
  
  # maximize map
  observeEvent(input$maximize, {
    dqshiny::add_class("mapWrapper", "maximize")
    dqshiny::run_js("mapReset")
  })
  
  # hide/show sidebar
  observeEvent(input$hideshow, {
    dqshiny::toggle_class("content", "fullwidth", input$hideshow == 2L)
  })
  
  # animation mode
  observe({
    if (!isTRUE(input$play == 2L)) return()
    val <- isolate(input$currPage)
    if (length(val) == 0) return()
    val <- if (val > length(pages)) 1L else val + 1L 
    updateNumericInput(session, "currPage", value = val)
    invalidateLater(5000L)
  })
  
  # close lightbox
  observeEvent(input$closelb, {
    dqshiny::hide("lightbox")
  })
  
  # previous image in lightbox
  observeEvent(input$prevLight, {
    if (is.null(input$currPage)) return()
    p <- pages[[input$currPage]]
    val <- input$currLight - 1L
    if (val < 1L) val <- length(p$images)
    updateNumericInput(session, "currLight", value = val)
  })
  
  # next image in lightbox
  observeEvent(input$nextLight, {
    if (is.null(input$currPage)) return()
    p <- pages[[input$currPage]]
    val <- input$currLight + 1L
    if (val > length(p$images)) val <- 1L
    updateNumericInput(session, "currLight", value = val)
  })
  
  # update current lightbox image
  observeEvent(input$currLight, {
    if (is.null(input$currPage)) return()
    p <- pages[[input$currPage]]
    i <- input$currLight
    dqshiny::run_js(
      "setCss", "lightbox", "background-image",
      paste0("url(img/", p$images[i], if (isTRUE(grepl("\\.[a-z]{3,4}$", p$images[i]))) ")" else ".jpg)")
    )
  })
  
  # enter/exit browser fullscreen
  observeEvent(input$fullscreen, {
    dqshiny::run_js("fullscreen", mode = c("exit", "enter")[input$fullscreen])
  })
  
  # handle page change
  observe({
    if (is.null(input$currPage)) return()
    p <- pages[[input$currPage]]
    
    # update map marker
    if (length(p$position)) {
      map <- leaflet::leafletProxy("map")
      leaflet::removeMarker(map, "pos")
      leaflet::addCircleMarkers(map, p$position[[1]], p$position[[2]], layerId = "pos", color = "#F30")
      if (input$autofocus && input$currPage > 1L) leaflet::flyTo(map, p$position[[1]], p$position[[2]], 10L)
    }
    
    # show/hide image divs
    invisible(lapply(2:6, function(i) {
      dqshiny::toggle_class("imageWrapper", paste0("show", i), i == length(p$images))
    }))
    
    # update panorama mode
    dqshiny::toggle_class("imageWrapper", "pano", p$type == "panorama")
    
    # update images
    invisible(lapply(seq(p$images), function(i) {
      dqshiny::run_js(
        "setCss", paste0("img", i), "background-image",
        paste0("url(img/", p$images[i], if (isTRUE(grepl("\\.[a-z]{3,4}$", p$images[i]))) ")" else ".jpg)")
      )
    }))
  })
  
  # description, title and page number
  output$description <- renderText(pages[[input$currPage]]$description)
  output$title <- renderUI(h2(pages[[input$currPage]]$title))
  output$page <- renderText(paste(input$currPage, "/", length(pages)))
  
  # route data
  route <- readRDS("route.RData")
  detour <- readRDS("detour.RData")
  markers <- readRDS("markers.RData")
  
  # map
  map <- leaflet::leaflet()
  map <- leaflet::setView(map, -116, 36, zoom = 6L)
  map <- leaflet::addProviderTiles(map, "Esri.WorldImagery", group = "Satellit")
  map <- leaflet::addTiles(map, options = leaflet::providerTileOptions(noWrap = TRUE), group = "Karte")
  map <- leaflet::addPolylines(map, route$lon, route$lat)
  map <- leaflet::addMarkers(map, markers$lon, markers$lat, label = markers$label)
  map <- leaflet::addPolylines(map, detour$lon, detour$lat, color = "#f00", dashArray = "12")
  map <- leaflet::addLayersControl(
    map, baseGroups = c("Karte", "Satellit"),
    options = leaflet::layersControlOptions(collapsed = FALSE),
    position = "bottomright"
  )
  output$map <- leaflet::renderLeaflet(map)
}

# Run the application 
shinyApp(ui = ui, server = server)
