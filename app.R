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

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Cases to include", 0, nrow(ncov_df),
                           value = 1
                ),

                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    print(input$range)
    ncov_df
  })
  
  
  ##  show the map with focus on China
  ##  Use a provider "Esri" that shows country names in English
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>% 
    setView(lng=114.27, lat=30.59, zoom=3) %>%  
    addProviderTiles(providers$Esri.WorldStreetMap)  ##shows city names as well in English

  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = 1, weight = 1, color = "red",
                fillOpacity = 0.7
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    ncov_df<-read_csv("ncov_outside_hubei.csv")
    attach(ncov_df)
    proxy <- leafletProxy("map", data = ncov_df)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      print("Show data on the table")
    ##  proxy %>% addLegend(position = "bottomleft")
    }
  })
}

shinyApp(ui, server)