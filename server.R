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
load_json <- function (region, vari){
  fname <- paste("www/",region,"_hist_",vari,"_annual.json",sep="")
  print(fname)
  geojsonio::geojson_read(fname, what = "sp")
}


addmapr <- function(dataTG, colort, TG1, vari){ #pal
  if(vari == "tg_mean"){
    pal <- colorNumeric("Spectral", domain = TG1$tg_mean)
    labels <- sprintf("Région: %s - %s", dataTG$TER_GUIDE, dataTG$tg_mean)#dataTG$vari
    fillColor <- pal(dataTG$tg_mean)
    values <- TG1$tg_mean
    title <- "Température (°C)"
  }
  
  return(leafletProxy("map", data = dataTG) %>%
 # clearShapes() %>%
   clearControls() %>%
   #leaflet("map")%>% #for debugging
  addPolygons (
    data = dataTG, #OneA,
    fillColor = fillColor,
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
        )%>%
      # #leaflet("map")%>% #debugging
      addLegend(pal = pal, values = values ,  title = title, opacity = 0.7,
                position = "topleft")
 )
}

mapTG <- function(TG, colort, vari){ 
  dataTG <- load_json(TG, vari)
  TGall <- paste("www/TG_hist_",vari,"_annual.json",sep="")
  TG1  <- geojsonio::geojson_read(TGall, what = "sp")
  vari <- vari
  addmapr(dataTG, colort, TG1, vari) 
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
  
  observeEvent(  input$PrecTotale, {
    vari <- "prcptot"
   })
  
  observe({
    TG <- input$Territoires
    colort <- "black"
    vari <- "tg_mean"
    mapTG(TG, colort, vari)
  })
 
  observe({
    TG <- input$Territoires2
    colort <- "white"
    vari <- "tg_mean"
    mapTG(TG, colort, vari)
  })
  
  observe({
    TG <- input$Territoires3
    colort <- "red"
    vari <- "tg_mean"
    mapTG(TG, colort, vari)
  })
  
  observeEvent(  input$Nettoyer, {
    leafletProxy("map") %>%
    clearShapes()
  })
  
  
  observe({ if (input$Echele == "Territoires guides" && input$Sousregions == "Toutes") {
    TG <- "TG"
    colort <- "black"
    vari <- "tg_mean"
    dataTG <- load_json(TG, vari)
    addmapr(dataTG, colort, dataTG, vari)} })

}
