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

load_json <- function (region){
  fname <- paste("www/",region,".json",sep="")
  print(fname)
  geojsonio::geojson_read(fname, what = "sp")
}

TG1  <- geojsonio::geojson_read("www/TG.json", what = "sp")

bins <- c('-5', '-4', '-2', '0', '2', '4', '6', '8', '10', '12', '14' )

palette <- function(inputST){
  pal <- colorBin("Spectral", domain = inputST$tg_mean, bins = bins)
  return (pal)
}

addmapr <- function(dataTG, pal, labels, colort){

return (leafletProxy("map", data = dataTG) %>%
 # clearShapes() %>%
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
      direction = "auto")
  ))
 #  %>%
 # addLegend(pal = palette, values = ~dataTG$tg_mean, title = "Température (°C)", opacity = 0.7,
 #            position = "bottomright")
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
  
  # This observer is responsible for plotting temperature shapes,
  # according to the variables the user has chosen to map to color and size.
  observe({
    TG <- input$Territoires
    dataTG <- load_json(TG)
    pal <- palette(dataTG)
    labels <- sprintf("Région: %s - Temp: %s", dataTG$TER_GUIDE, dataTG$tg_mean)
    colort <- "black"
    addmapr(dataTG, pal, labels, colort)
    
  })
 
  observe({
    TG <- input$Territoires2
    dataTG <- load_json(TG)
    pal <- palette(dataTG)
    labels <- sprintf("Région: %s - Temp: %s", dataTG$TER_GUIDE, dataTG$tg_mean)
    colort <- "white"
    addmapr(dataTG, pal, labels, colort)
  })
  
  observe({
    TG <- input$Territoires3
    dataTG <- load_json(TG)
    pal <- palette(dataTG)
    labels <- sprintf("Région: %s - Temp: %s", dataTG$TER_GUIDE, dataTG$tg_mean)
    colort <- "red"
    addmapr(dataTG, pal, labels, colort)
  })

}
