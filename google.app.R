##  shiny_nCoV will display a leaflets map
##  plot points from a datafile that contains columns "latitude" and "longitude"
##  Input: Slider to control how many observations to plot.  These are in "date" order 
##   The effect should be to show the observations over time to show where the virus has spread.
##
##  Author: Daniel Vogel
##  Date:   2/7/2020

library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(RgoogleMaps)
library(RCurl)

## flag to change between internet and local mapTiles provider
localMap<-TRUE
## initial Zoom level for map and restrictions on zoom to limit mapTiles required
defaultZoom<-3
lowZoom<-2
highZoom<-5
print(paste0("defaultZoom:",defaultZoom))
print(paste0("lowZoom:",lowZoom))
print(paste0("highZoom:", highZoom))
##tile provider
## OSM provider organizes tile/z/x/y
## Esri organizes tile/z/y/x
tileServer<-"http://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}.png"
## Focus of map -Hebei, China
focusLon<-114
focusLat<-30.5

## get datafile
ncov_df<-read_csv("ncov_outside_hubei.csv")
attach(ncov_df)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
               
                sliderInput("range", "Cases to include", 0, nrow(ncov_df),
                           value = 1
                ),

                checkboxInput("localMap", "Use Local Map", localMap))
)

downloadMap<-function() {
  print("Using local map")
  ## load tiles for offline use
  lat_range=c(0, focusLat+40)
  lon_range=c(0, focusLon+40)
  print(paste0("lat range:", lat_range))
  print(paste0("lon range:", lon_range))
  
  ## load tiles for variety of Zoom levels
  for (zoom in lowZoom:highZoom) {
    
    message( paste0("Downloading mapTiles for zoom level=",zoom,"\n") )
    print(paste("tileServer URL:",tileServer))
    GetMapTiles(center=c( focusLat, lat=focusLon), 
                lonR=c(-140,150), latR=c(0,70),
   #             lonR=lon_range, latR=lat_range,
                zoom=zoom,
   #             type="Esri.WorldStreetMap",
   #             urlBase=tileServer,    NOT WORKING, defaults to google tileserver
                tileDir="mapTiles/google",
                CheckExistingFiles = TRUE,  ## force it to download tiles for testing
                returnTiles=TRUE,
                verbose=1
               ##nTiles = round(c(20,20)/(17-zoom))
    )
    message( "\nDownloading of mapTiles complete \n" )
    ## print some kind of loading message on map?
  
  }
  ## different providers in different directories.  Default is OSM
  #addResourcePath("EsriTiles", "H:/R/ncov/mapTiles/ESRIWorldStreetMap")
  addResourcePath("OSMTiles",   "H:/R/ncov/mapTiles/OSM")
  addResourcePath("EsriTiles",  "H:/R/ncov/mapTiles/ESRIWorldStreetMap")
  addResourcePath("googleTiles",  "H:/R/ncov/mapTiles/google")
  
}

server <- function(input, output, session) {

  #  download map tiles if needed
  if (localMap) {
    downloadMap()
  }
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    print(input$range)
    return(ncov_df[c(1:input$range),c(1:14)])
  })
  ## for debugging, show the zoom level to decide how many to downloads

  ##  show the map with focus on China
  ##  Use a provider "Esri" that shows country names in English
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).

    if (localMap){ 
      print("using local map")
      leaflet(options = leafletOptions(minZoom = lowZoom, maxZoom = highZoom)) %>% 
        setView(lng=focusLon, lat=focusLat, zoom=3) %>%  
        addTiles(urlTemplate="/googleTiles/{z}_{x}_{y}.png")
    } else {
      print("using internet map")
      leaflet(options = leafletOptions(minZoom = lowZoom, maxZoom = highZoom)) %>% 
        setView(lng=focusLon, lat=focusLat, zoom=3) %>%  
        addProviderTiles(providers$Esri.WorldStreetMap)

    }
      
#    urlBase="http:/a.tile.openstreetmap.org" %>%
#    addProviderTiles(providers$Esri.WorldStreetMap)  ##shows city names as well in English

  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = 2, weight = 1, color = "red",
                fillOpacity = 0.7
      ) %>%
      addMarkers(lng=focusLon, lat=focusLat)
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    ## filter the data to the number of observations chosen by the user
    proxy <- leafletProxy("map", data = filteredData())

    proxy %>% clearControls()
    
    ## Display the local or internet version of the map
    ## depending on which option was selected by the user
    localMap<-input$localMap
    if (localMap){
      leafletProxy("map") %>% 
        setView(lng=focusLon, lat=focusLat, zoom=3) %>%  
        addTiles(urlTemplate="/EsriTiles/{z}/{x}/{y}.png")
      
    } else {
      
      leafletProxy("map") %>% 
        setView(lng=focusLon, lat=focusLat, zoom=3) %>%  
        addProviderTiles(providers$Esri.WorldStreetMap)
    }
  })
}

shinyApp(ui, server)