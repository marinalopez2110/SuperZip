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
library(dygraphs)




df <- read.table("www/table2.csv", header = TRUE, sep = ";")
# setwd("C:\\Users\\marlop1\\Documents\\GitHub\\SuperZip")
load_json <- function (region, vari, saisson){
  fname <- paste("www/",region,"_", vari, "_",saisson, ".json",sep="")
  print(fname)
  geojsonio::geojson_read(fname, what = "sp")
}

conditions <- function(horizon, scenario, percentile){
  if (input$Horizon == '2041-2070') {
    if (input$Scenario == "rcp45"){
      if(input$Percentile == "10"){
        all_selec <- "t2050_rcp45_p10"
      } else if(input$Percentile == "50"){
        all_selec <- "t2050_rcp45_p50"
      } else if(input$Percentile == "90"){
        all_selec <- "t2050_rcp45_p90"}
    } else if (input$Scenario == "rcp85"){
      if(input$Percentile == "10"){
        all_selec <- "t2050_rcp85_p10"
      } else if(input$Percentile == "50"){
        all_selec <- "t2050_rcp85_p50"
      } else if(input$Percentile == "90"){
        all_selec <- "t2050_rcp85_p90"}
    } } else if (input$Horizon == '2071-2100') {
      if (input$Scenario == "rcp45"){
        if(input$Percentile == "10"){
          all_selec <- "t2080_rcp45_p10"
        } else if(input$Percentile == "50"){
          all_selec <- "t2080_rcp45_p50"
        } else if(input$Percentile == "90"){
          all_selec <- "t2080_rcp45_p90"}
      } else if (input$Scenario == "rcp85"){
        if(input$Percentile == "10"){
          all_selec <- "t2080_rcp85_p10"
        } else if(input$Percentile == "50"){
          all_selec <- "t2080_rcp85_p50"
        } else if(input$Percentile == "90"){
          all_selec <- "t2080_rcp85_p90"}}}
  return(all_selec)}

addmapr <- function(dataTG, vari, period, scenario, percentile, all_selec){ 
 # if(vari == "tg_mean"){
    pal <- colorNumeric("Spectral", domain = c(-4.5, 14))
    print ("pal")
    print(all_selec)
    #print(dataTG[[all_selec]])
    labels <- sprintf("Région: %s : %s", dataTG$TER_GUIDE, dataTG[[all_selec]]) 
    print ("labels")
    fillColor <- pal(dataTG[[all_selec]])
    print ("fillcolor")
    values <- dataTG[[all_selec]]
    print ("values")
    title <- sprintf("Température (°C) -%s", all_selec)
    print("title")#}

    # else if(vari == "prcptot"){
    # pal <- colorNumeric("Spectral", domain = c(350, 1700))
    # labels <- sprintf("Région: %s - %s", dataTG$TER_GUIDE, dataTG$prcptot)#dataTG$vari
    # fillColor <- pal(dataTG$prcptot)
    # values <- dataTG$prcptot
    # title <- "Précipitation totale (mm)"} 
  
  return(leafletProxy("map", data = dataTG) %>%
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
  #print("leafproxy")
}

mapTG <- function(region, vari, period, saisson, scenario, percentile, all_selec){ 
  dataTG <- load_json(region, vari, saisson)
  print ("dataTG" )
  vari <- vari
  addmapr(dataTG, vari, period, scenario, percentile, all_selec) 
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
    all_selec <- "hist_p50"
    if (input$PrecTotale) {
      vari <- "prcptot"
      print (vari)}
    if (input$Moyenne) {
      vari <- "tg_mean"
      print (vari)}
    if (input$Horizon == '2041-2070') {
      if (input$Scenario == "rcp45"){
        if(input$Percentile == "10"){
          all_selec <- "t2050_rcp45_p10"
        } else if(input$Percentile == "50"){
          all_selec <- "t2050_rcp45_p50"
        } else if(input$Percentile == "90"){
          all_selec <- "t2050_rcp45_p90"}
      } else if (input$Scenario == "rcp85"){
        if(input$Percentile == "10"){
          all_selec <- "t2050_rcp85_p10"
        } else if(input$Percentile == "50"){
          all_selec <- "t2050_rcp85_p50"
        } else if(input$Percentile == "90"){
          all_selec <- "t2050_rcp85_p90"}
      } } else if (input$Horizon == '2071-2100') {
        if (input$Scenario == "rcp45"){
          if(input$Percentile == "10"){
            all_selec <- "t2080_rcp45_p10"
          } else if(input$Percentile == "50"){
            all_selec <- "t2080_rcp45_p50"
          } else if(input$Percentile == "90"){
            all_selec <- "t2080_rcp45_p90"}
        } else if (input$Scenario == "rcp85"){
          if(input$Percentile == "10"){
            all_selec <- "t2080_rcp85_p10"
          } else if(input$Percentile == "50"){
            all_selec <- "t2080_rcp85_p50"
          } else if(input$Percentile == "90"){
            all_selec <- "t2080_rcp85_p90"}}} 
    mapTG(region, vari, period, saisson, scenario, percentile, all_selec)
  })
 
  # observe({
  #   #Default values
  #   region <- input$Territoires2
  #   vari <- "tg_mean"
  #   period <- "hist"
  #   saisson <- "annual"
  #   scenario <- "rcp45"
  #   percentile <- "50"
  #   if (input$PrecTotale) {
  #     vari <- "prcptot"
  #     print (vari)}
  #   if (input$Horizon == '2041-2070') {
  #     period <- "2050"
  #     print (period)}
  #   if (input$Horizon == '2071-2100') {
  #     period <- "2080"
  #     print (period)}
  #   if (input$Scenario == 'rcp85') {
  #     scenario <- "rcp85"
  #     print (scenario)}
  #   if (input$Percentile == '10') {
  #     percentile <- "10"
  #     print (percentile)}
  #   if (input$Percentile == '90') {
  #     percentile <- "90"
  #     print (percentile)}
  #   mapTG(region, vari, period, saisson, scenario, percentile)
  # })
  # 
  # observe({
  #   #Default values
  #   region <- input$Territoires3
  #   vari <- "tg_mean"
  #   period <- "hist"
  #   saisson <- "annual"
  #   scenario <- "rcp45"
  #   percentile <- "50"
  #   if (input$PrecTotale) {
  #     vari <- "prcptot"
  #     print (vari)}
  #   if (input$Horizon == '2041-2070') {
  #     period <- "2050"
  #     print (period)}
  #   if (input$Horizon == '2071-2100') {
  #     period <- "2080"
  #     print (period)}
  #   if (input$Scenario == 'rcp85') {
  #     scenario <- "rcp85"
  #     print (scenario)}
  #   if (input$Percentile == '10') {
  #     percentile <- "10"
  #     print (percentile)}
  #   if (input$Percentile == '90') {
  #     percentile <- "90"
  #     print (percentile)}
  #   mapTG(region, vari, period, saisson, scenario, percentile)
  # })
  
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
    all_selec <- "hist_p50"
    if (input$PrecTotale) {
      vari <- "prcptot"
      print (vari)}
    if (input$Horizon == '2041-2070') {
      if (input$Scenario == "rcp45"){
        if(input$Percentile == "10"){
          all_selec <- "t2050_rcp45_p10"
        } else if(input$Percentile == "50"){
          all_selec <- "t2050_rcp45_p50"
        } else if(input$Percentile == "90"){
          all_selec <- "t2050_rcp45_p90"}
      } else if (input$Scenario == "rcp85"){
        if(input$Percentile == "10"){
          all_selec <- "t2050_rcp85_p10"
        } else if(input$Percentile == "50"){
          all_selec <- "t2050_rcp85_p50"
        } else if(input$Percentile == "90"){
          all_selec <- "t2050_rcp85_p90"}
      } } else if (input$Horizon == '2071-2100') {
        if (input$Scenario == "rcp45"){
          if(input$Percentile == "10"){
            all_selec <- "t2080_rcp45_p10"
          } else if(input$Percentile == "50"){
            all_selec <- "t2080_rcp45_p50"
          } else if(input$Percentile == "90"){
            all_selec <- "t2080_rcp45_p90"}
        } else if (input$Scenario == "rcp85"){
          if(input$Percentile == "10"){
            all_selec <- "t2080_rcp85_p10"
          } else if(input$Percentile == "50"){
            all_selec <- "t2080_rcp85_p50"
          } else if(input$Percentile == "90"){
            all_selec <- "t2080_rcp85_p90"}}}
    dataTG <- load_json(region, vari, saisson)
    addmapr(dataTG, vari, period, scenario, percentile, all_selec)} })
  
  
##### FOR THE TABLE  
  output$tabletest <- renderTable({
    xtable(df,digits=2, type = "html", html.table.attributes="class='table-bordered'")
  },
  size="footnotesize", #Change size; useful for bigger tables
  include.rownames=FALSE, #Don't print rownames
  caption.placement="top",
  include.colnames=FALSE,
  add.to.row = list(pos = list(0),
                    command = "<tr><th><br> Saison </th><th><br> 1981-2010 </th><th colspan='2'> 2041-2070 </th><th  colspan='2'> 2071-2100 </th></tr>
  <tr><th> </th><th> </th><th> Émissions moderées </th><th> Émissions élevées </th><th> Émissions moderées </th><th> Émissions élevées </th></tr>"
  ))
  
  
  ##### FOR THE TIMESERIES FIGURE
  
  dfts <- read.csv("www/p4tgmean.csv")
  rownames(dfts) <- dfts$time
  #series <- ts(d2f$time,  df2$tg_mean_p50) 
  keep <- c( "time", "tg_mean_p10", "tg_mean_p50", "tg_mean_p90" )
  dfts2  <- dfts[ , keep]
  
  output$dygraph <- renderDygraph({
    dfts <- read.csv("www/p4tgmean.csv")
    rownames(dfts) <- dfts$time
        keep <- c( "time", "tg_mean_p10", "tg_mean_p50", "tg_mean_p90" )
    dfts2  <- dfts[ , keep]
    dygraph(dfts2)
  })

}
