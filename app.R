##  shiny_nCoV will display a leaflets map
##  plot points from a datafile that contains columns "latitude" and "longitude"
##  Input: Slider to control how many observations to plot.  These are in "date" order 
##   The effect should be to show the observations over time to show where the virus has spread.
##
##  Author: Daniel Vogel
##  Date:   2/7/2020
## 
## published on shinyapps.io
## 4/23/2020 updated to pull latestdata.csv to about 436,000 rows, and filter dirty rows
###
library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(lubridate)
library(digest)

datafilename<-"latestdata.csv"
dataurl<-"https://raw.githubusercontent.com/beoutbreakprepared/nCoV2019/master/latest_data/latestdata.csv"

## read in the date file ##
download_df<-function(dataurl,datafile){
  # optionally check if file aleady exists and save old copy.  Do a sha1 hash to compare with the file 
  # that is downloaded.  This will be used to compare the github version of the file before downloading.
  # if the file is the same, the download will be skipped.
  if (file.exists(datafile)){
    file.rename(datafile, paste0(datafile,".bak"))
  } 
  download.file(dataurl,datafile)
  return(read_csv(datafile))

}

cleandata<-function(df){
  ## check the number of rows before cleaning
  rows_raw<-nrow(df)
  print(paste("Data Frame has ",rows_raw,"rows before removing blank dates"))
  df<-df[ !(df$date_confirmation==""), ]
  ## add a date type field 
  df$sortdate<-as.Date(parse_date_time(df$date_confirmation,"dmy"))
  ## sort by date
  df<-df[order(df$sortdate),]
  ## remove extra columns 
  df<-select(df,ID,sortdate,date_confirmation,latitude,longitude,country_new,province)
  ## round the latitude & longitude so we can group close points
  df$latitude<-round(df$latitude,1)
  df$longitude<-round(df$longitude,1)
  ## add a field that ties to gps coordinates together  
  df$gps<-paste0(df$latitude,"x",df$longitude)
  ## remove rows with date=N/A
  df<-df[!(is.na(df$sortdate)), ]
  rows_clean<-nrow(df)
  print(paste("latestdata.csv has ",rows_clean,"rows"))
  print(paste(rows_raw - rows_clean,"rows were removed"))
  return(df)
}
#
# function to sort the countries by to sum of the case counts in reverse order
# and return a df with the top10 countries to be used as a legend
top10<-function(df){
  df<-df[!(is.na(df$country_new)), ]
  df<-df[order(df$country_new),]
  df<-df %>% group_by(country_new) %>%
    summarise(countrycount=sum(casecount))
  
  top10<-df[order(-df$countrycount),]
  top10<-top10[1:10,]
  return(top10)
}

## read a local file for the data.  This is done by default.
## There is a UI button to perform a new download.
## If the datafile does not exist, download it.
if (file.exists(datafilename)){
  ncov_df<-read_csv(datafilename)
  } else {
  ncov_df<-download_df(datafilename)
  }

# compute sha1 hash of current datafile comparing
checksum<-sha1(datafilename)
print(paste(datafilename,"sha1:",checksum))

# remove rows with blank dates and round gps coordinates.
# add a column called "sortdate" which is a true "date" type
ncov_df<-cleandata(ncov_df)

## create another column called "casecount"=qty points grouped by location
## This will eliminate duplicate plots but also keep track of cases in single location
ncov_df<-ncov_df %>%
  group_by(sortdate,gps,latitude,longitude,country_new) %>%
  summarise(casecount=n())

# get date range for slider
firstdate<-head(ncov_df,1)$sortdate
lastdate <-tail(ncov_df,1)$sortdate
print(paste(firstdate,"->",lastdate, "total days:",lastdate - firstdate))


## Create the ui ##
ui <- bootstrapPage(
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
    
                sliderInput("days", 
                            "Dates to include", 
                            firstdate, 
                            lastdate,
                            value = lastdate,
                            timeFormat="%Y-%m-%d"
                ),
                ## this button will pull new data from the source if needed
                actionButton("download", "Download new data")
  )
  
)

## Server functions ##
server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    print(input$days)
    ## this will work with the date slider.  New method of selecting which data to plot
    df<-ncov_df %>% filter( sortdate < input$days)
    return(df)
  })
  
  
  ##  show the map with focus on Europe so China and US can be seen 
  ##  Use a provider "Esri" that shows country names in English
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>% 
    setView(lng=9, lat=30.59, zoom=3) %>%  
    addProviderTiles(providers$Esri.WorldStreetMap)  ##shows city names as well in English

  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    ## we have added a column called "casecount" which can be used to label
    ## hotspots.
    df<-filteredData()
    leafletProxy("map", data = df ) %>%
      clearShapes() %>%
      ## each radius point =1m so this will multiply by 1km size.
      ## used sqrt to keep the diameter smaller for hotspots
      addCircles(radius=1000*sqrt(df$casecount), 
                 weight=1, 
                 color="red", 
                 opacity=.1,
                 fillOpacity=.1)
  })
  
  
  
  ## when user presses [download] it will retrieve a new datafile 
  ## the button will then disappearde
  observeEvent(input$download,{
      print("Download new datafile from source and clean it up")
      ## there may be a way to check the sha1 hash of the file on github prior to download
      ## github stores files in "blobs" which include the sha1 hash. 
      ## For now, I will download the new file and make the button disappear.
      ncov_df<-cleandata(download_df(dataurl,datafilename))
      newchecksum<-sha1(datafilename)
      print("Downloaded sha1 hash")
      print(paste("new:",newchecksum))
      print(paste("old:",checksum))
      if (newchecksum == checksum) {
        print(paste(datafilename," has not changed"))
      } else {
        print("Data has been updated")
        checksum<-newchecksum
      }
      removeUI(selector='#download', immediate=TRUE)
    
  }, autoDestroy=TRUE)
}

shinyApp(ui, server)