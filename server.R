library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(geojsonio)
library(shiny)
library(sp)
library(htmltools)
library(purrr)


# setwd("C:\\Users\\marlop1\\Documents\\GitHub\\SuperZip")
load_json <- function (region){
  fname <- paste("www/",region,".json",sep="")
  print(fname)
  geojsonio::geojson_read(fname, what = "sp")
}

TG1  <- geojsonio::geojson_read("www/TGobstg.json", what = "sp")

palette <- function(inputST){
  pal <- colorNumeric("Spectral", domain = TG1$tg_mean)
  return (pal)
}

addmapr <- function(dataTG, pal, labels, colort){
  return(leafletProxy("map", data = dataTG) %>%
 # clearShapes() %>%
   clearControls() %>%
   #leaflet("map")%>%
  addPolygons (
    fillColor = pal(dataTG$tg_mean),
    data = dataTG, #OneA,
    weight = 1,
    opacity = 1,
    color = colort,
    dashArray = "3",
    fillOpacity = 0.8,
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"))%>%
      #leaflet("map")%>%
      addLegend(pal = pal, values = TG1$tg_mean ,  title = "Température (°C)", opacity = 0.7,
                position = "topleft") )
}

mapTG <- function(TG, colort){ 
  dataTG <- load_json(TG)
  pal <- palette(dataTG)
  labels <- sprintf("Région: %s - Temp: %s", dataTG$TER_GUIDE, dataTG$tg_mean)
  addmapr(dataTG, pal, labels, colort)
}

function(input, output, session) {
  
  ## Interactive Map ###########################################
  # Create the map
  output$map <- renderLeaflet({
    leaflet(map) %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lat = 45.6, lng= -70.5, zoom = 7)
  })
  
  observe({
    TG <- input$Territoires
    colort <- "black"
    mapTG(TG, colort)
  })
 
  observe({
    TG <- input$Territoires2
    colort <- "white"
    mapTG(TG, colort)
  })
  
  observe({
    TG <- input$Territoires3
    colort <- "red"
    mapTG(TG, colort)
  })
  
  
  observe({ if (input$Echele == "Territoires guides" && input$Sousregions == "Toutes") {
    colort <- "black"
    pal <- palette(TG1)
    labels <- sprintf("Région: %s - Temp: %s", TG1$TER_GUIDE, TG1$tg_mean)
    addmapr(TG1, pal, labels, colort)} })

}
