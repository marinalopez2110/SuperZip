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
load_json <- function (region, vari, period, saisson, scenario, percentile){
  fname <- paste("www/",region,"_", period, "_", vari,"_", saisson,"_", scenario, "_", percentile, ".json",sep="")
  print(fname)
  geojsonio::geojson_read(fname, what = "sp")
}


addmapr <- function(dataTG, TG1, vari){ #pal
  if(vari == "tg_mean"){
    pal <- colorNumeric("Spectral", domain = TG1$tg_mean)
    labels <- sprintf("Région: %s - %s", dataTG$TER_GUIDE, dataTG$tg_mean)#dataTG$vari
    fillColor <- pal(dataTG$tg_mean)
    values <- TG1$tg_mean
    title <- "Température (°C)"
  } else if(vari == "prcptot"){
    pal <- colorNumeric("Spectral", domain = TG1$prcptot)
    labels <- sprintf("Région: %s - %s", dataTG$TER_GUIDE, dataTG$prcptot)#dataTG$vari
    fillColor <- pal(dataTG$prcptot)
    values <- TG1$prcptot
    title <- "Précipitation totale (mm)"
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
    color = "black",
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

mapTG <- function(region, vari, period, saisson, scenario, percentile){ 
  dataTG <- load_json(region, vari, period, saisson, scenario, percentile)
  TGall <- paste("www/TG","_", period, "_", vari,"_", saisson,"_", scenario, "_", percentile, ".json",sep="")
  TG1  <- geojsonio::geojson_read(TGall, what = "sp")
  vari <- vari
  addmapr(dataTG, TG1, vari) 
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
    #Default values
    region <- input$Territoires
    vari <- "tg_mean"
    period <- "hist"
    saisson <- "annual"
    scenario <- "rcp45"
    percentile <- "50"
    if (input$PrecTotale) {
      vari <- "prcptot"
      print (vari)}
    if (input$Horizon == '2041-2070') {
      period <- "2050"
      print (period)}
    if (input$Horizon == '2071-2100') {
      period <- "2080"
      print (period)}
    if (input$Scenario == 'rcp85') {
      scenario <- "rcp85"
      print (scenario)}
    mapTG(region, vari, period, saisson, scenario, percentile)
  })
 
  observe({
    #Default values
    region <- input$Territoires2
    vari <- "tg_mean"
    period <- "hist"
    saisson <- "annual"
    scenario <- "rcp45"
    percentile <- "50"
    if (input$PrecTotale) {
      vari <- "prcptot"
      print (vari)}
    if (input$Horizon == '2041-2070') {
      period <- "2050"
      print (period)}
    if (input$Horizon == '2071-2100') {
      period <- "2080"
      print (period)}
    if (input$Scenario == 'rcp85') {
      scenario <- "rcp85"
      print (scenario)}
    mapTG(region, vari, period, saisson, scenario, percentile)
  })
  
  observe({
    #Default values
    region <- input$Territoires3
    vari <- "tg_mean"
    period <- "hist"
    saisson <- "annual"
    scenario <- "rcp45"
    percentile <- "50"
    if (input$PrecTotale) {
      vari <- "prcptot"
      print (vari)}
    if (input$Horizon == '2041-2070') {
      period <- "2050"
      print (period)}
    if (input$Horizon == '2071-2100') {
      period <- "2080"
      print (period)}
    if (input$Scenario == 'rcp85') {
      scenario <- "rcp85"
      print (scenario)}
    mapTG(region, vari, period, saisson, scenario, percentile)
  })
  
  observeEvent(  input$Nettoyer, {
    leafletProxy("map") %>%
    clearShapes()
  })
  
  
  observe({ if (input$Echele == "Territoires guides" && input$Sousregions == "Toutes") {
    region <- "TG"
    vari <- "tg_mean"
    period <- "hist"
    saisson <- "annual"
    scenario <- "rcp45"
    percentile <- "50"
    if (input$PrecTotale) {
      vari <- "prcptot"
      print (vari)}
    if (input$Horizon == '2041-2070') {
      period <- "2050"
      print (period)}
    if (input$Horizon == '2071-2100') {
      period <- "2080"
      print (period)}
    dataTG <- load_json(region, vari, period, saisson, scenario, percentile)
    addmapr(dataTG, dataTG, vari)} })

}
