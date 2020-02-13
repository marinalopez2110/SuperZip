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
library(xtable)

df <- read.table("www/testtabel.csv", header = TRUE, sep = ";")
# setwd("C:\\Users\\marlop1\\Documents\\GitHub\\SuperZip")
load_json <- function (region, vari, period, saisson, scenario, percentile){
  fname <- paste("www/",region,"_", period, "_", vari,"_", saisson,"_", scenario, "_", percentile, ".json",sep="")
  print(fname)
  geojsonio::geojson_read(fname, what = "sp")
}


addmapr <- function(dataTG, vari){ #pal
  if(vari == "tg_mean"){
    pal <- colorNumeric("Spectral", domain = c(-4.5, 14))
    print ("pal")
    labels <- sprintf("Région: %s - %s", dataTG$TER_GUIDE, dataTG$tg_mean)
    print("labels")
    fillColor <- pal(dataTG$tg_mean)
    print("fillColor")
    values <- dataTG$tg_mean
    print("values")
    title <- "Température (°C)"
  } else if(vari == "prcptot"){
    pal <- colorNumeric("Spectral", domain = c(350, 1700))
    labels <- sprintf("Région: %s - %s", dataTG$TER_GUIDE, dataTG$prcptot)#dataTG$vari
    fillColor <- pal(dataTG$prcptot)
    values <- dataTG$prcptot
    title <- "Précipitation totale (mm)"
  } 
  
  return(leafletProxy("map", data = dataTG) %>%
 # clearShapes() %>%
   clearControls() %>%
   #leaflet("map")%>% #for debugging
  addPolygons (
    data = dataTG, 
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
  print("leafproxy")
}

mapTG <- function(region, vari, period, saisson, scenario, percentile){ 
  dataTG <- load_json(region, vari, period, saisson, scenario, percentile)
  print ("dataTG")
  vari <- vari
  addmapr(dataTG, vari) 
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
    if (input$Percentile == '10') {
      percentile <- "10"
      print (percentile)}
    if (input$Percentile == '90') {
      percentile <- "90"
      print (percentile)}
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
    if (input$Percentile == '10') {
      percentile <- "10"
      print (percentile)}
    if (input$Percentile == '90') {
      percentile <- "90"
      print (percentile)}
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
    if (input$Percentile == '10') {
      percentile <- "10"
      print (percentile)}
    if (input$Percentile == '90') {
      percentile <- "90"
      print (percentile)}
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
    if (input$Percentile == '10') {
      percentile <- "10"
      print (percentile)}
    if (input$Percentile == '90') {
      percentile <- "90"
      print (percentile)}
    dataTG <- load_json(region, vari, period, saisson, scenario, percentile)
    addmapr(dataTG, vari)} })
  
  output$tabletest <- renderTable({
    xtable(df,digits=2, type = "html", html.table.attributes="class='table-bordered'")
  },
  size="footnotesize", #Change size; useful for bigger tables
  include.rownames=FALSE, #Don't print rownames
  caption.placement="top",
  include.colnames=FALSE,
  add.to.row = list(pos = list(0),
                    command = "<tr><th align='center'><br> Saison </th><th rowspan='2'><br> 1981-2010 </th><th colspan='2', align='center'> 2041-2070 </th><th  colspan='2'> 2071-2100 </th></tr>
  <tr><th> </th><th> </th><th> Émissions moderées </th><th> Émissions élevées </th><th> Émissions moderées </th><th> Émissions élevées </th></tr>"
  ))

}
