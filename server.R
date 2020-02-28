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
library(stringr)

###### LOAD TABLE
df <- read.table("www/table2.csv", header = TRUE, sep = ";")
# setwd("C:\\Users\\marlop1\\Documents\\GitHub\\SuperZip")


####### CONDITIONS FOR HORIZON, SCENARIO, SEASON AND PERCENTILE IN MAPS
conditions <- function(season2, horizon, scenario, percentile){
  if (horizon == 'Historique'){
    all_selec <-  paste(season2,"hist_p50", sep="")} 
  if (horizon == '2041-2070') {
    horizon2 <- "t2050"
    all_selec <- paste(season2, horizon2,"_",scenario,"_p", percentile, sep="")}
  if (horizon == '2071-2100'){
    horizon2 <- "t2080"
    all_selec <- paste(season2, horizon2,"_",scenario,"_p", percentile, sep="")}  
  print (all_selec)
  return(all_selec)}



### CHANGING VARIABLE
varif  <- function(Variable2){
  if (Variable2 == "Précipitations sous forme de neige") {
    vari <- "solidprcptot"
    print ("button neige")}
  if (Variable2 == "Précipitations totales") {
    vari <- "prcptot"
    print ("button precip")}
  if (Variable2 == "Températures moyennes") {
    vari <- "tg_mean"
    print ("button tem avg")}
  if (Variable2 == "Températures maximales") {
    vari <- "tx_mean"
    print ("Button tmax")}
  if (Variable2 == "Températures minimales") {
    vari <- "tn_mean"
    print ("button t min")}
  if (Variable2 == "Degrés-jours de croissance"){
    vari <- "DJC"
    print ("button djc")}
  if (Variable2 == "Évènements gel-dégel"){
    vari <- "dlyfrzthw"
    print ("button GelDegel")}
  if (Variable2 == "Saison de croissance"){
    vari <- "growing_season_length"
    print ("button Saison de croissance")}
  return(vari)}

##### INTERMIDIATE STEP *** CHECK IF STILL NEED IT
mapTG <- function(region, namer, vari, period, saison, scenario, percentile, all_selec, fname2){ 
  print (region)
  dataTG <- load_json(fname2) ### THIS STEP TAKES VERY LONG
  print ("dataTG" )
  vari <- vari
  addmapr(dataTG, vari, region, namer, period, scenario, percentile, all_selec) 
}

####### Get the name of the file
fnamef <- function (region, vari, saison){
  #SUBSTITUTE ACCENTS
  nameA <- str_replace_all(region, c( "é"= "e", "à"="a", "è"= "e", "ô" = "o", "ç"="c", "É"="E", "È"="E", "Î"="i", "Ç"="C"))
  print (nameA)
  fname3 <- paste("www/",nameA,"_", vari, "_",saison, ".json",sep="")
  print(fname3)
  return(fname3)}

###### LOAD GEOJSON FILE
load_json <- function (fname2){
  print ("load json")
  geojsonio::geojson_read(fname2, what = "sp")
 # filename(nameA, vari, saison)
}

#### MODIFY MAP BASED ON SELECTION - LEAFLETPROXY
addmapr <- function(dataTG, vari, region, namer, period, scenario, percentile, all_selec){ 
    print (region)
    print (namer)
    print("all_selec")
    #print(dataTG[[all_selec]])
    labels <- sprintf("Région: %s : %s", dataTG[[namer]], dataTG[[all_selec]]) 
    print ("labels")
    print ("values")
    if(vari == "tg_mean"){
    pal <- colorNumeric("Spectral", domain = c(-26, 30))
    title <- sprintf("Température Moy (°C) -%s", all_selec)
    values <- c(-25, 30)
    print("title")}
    else if(vari == "tn_mean"){
      pal <- colorNumeric("Spectral", domain = c(-30, 25))
      title <- sprintf("Température Min (°C) -%s", all_selec)
      values <- c(-30, 25)
      print("title")}
    else if(vari == "tx_mean"){
      pal <- colorNumeric("Spectral", domain = c(-28, 34))
      title <- sprintf("Température Max (°C) -%s", all_selec)
      values <- c(-28, 34)
      print("title")}
    else if(vari == "prcptot"){
      pal <- colorNumeric("Spectral", domain = c(50, 1500))
      title <- sprintf("Précipitation totale (mm) -%s", all_selec)
      values <- c(800, 1500)
      print("title")} 
    else if(vari == "solidprcptot"){
      pal <- colorNumeric("Spectral", domain = c(0, 50))
      title <- sprintf("Précipitation neige (cm) -%s", all_selec)
      values <- c(0, 50)
      print("title")} 
    else if(vari == "DJC"){
      pal <- colorNumeric("Spectral", domain = c(200, 4200))
      title <- sprintf("Degrés-jours de croissance (DJC) -%s", all_selec)
      values <- c(200, 4200)
      print("title")} 
    else if(vari == "dlyfrzthw"){
      pal <- colorNumeric("Spectral", domain = c(0, 105))
      title <- sprintf("Évènements gel-dégel -%s", all_selec)
      values <- c(0, 105)
      print("title")} 
    else if(vari == "growing_season_length"){
      pal <- colorNumeric("Spectral", domain = c(100, 280))
      title <- sprintf("Saison de croissance -%s", all_selec)
      values <- c(100, 280)
      print("title")} 
    fillColor <- pal(dataTG[[all_selec]])
    print ("fillcolor--------------------")
  
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
 )}


##### SHINY FUNCTION
function(input, output, session) {
  
  #### CHANGING SEASON, TIME PERIOD AND PERCENTILE
  saisonf <- function(horizon2){
    if (horizon2 == 'Historique' || horizon2 == '2041-2070' || horizon2 =='2071-2100'){
    horizon <- horizon2
    scenario <- input$Scenario
    percentile <- input$Percentile
    if (input$Saisonnalite == "Annuel" ){
      saisoni <- "annual"
      seasoni2 <- ""}
    if (input$Saisonnalite == "Saisonier" ){
      saisoni <- "seasonal"
      seasoni2 <- paste(input$season, "_", sep="")}
    if (input$Saisonnalite == "Mensuel" ){
      saisoni <- "monthly"
      seasoni2 <- paste(input$Mois, "_", sep="")}
    all_seleci <- conditions(seasoni2, horizon, scenario, percentile)}
    listsaison <- list(saisoni, seasoni2, all_seleci)
    return  (listsaison)}
  
  ### Function to change region for one or all regions
  regionf <- function(echele2, sousregion2){
    if (echele2 == 'Domaines bioclimatiques'){
      namer <- "NOM"
      if (sousregion2 == "Toutes") {region <- "DB"} 
      else {region <- input$Domaines}
    } else  if (echele2 == "Sous-domaines bioclimatiques"){
      namer <- "NOM"
      if (sousregion2 == "Toutes") {region <- "SDB"} 
      else {region <- input$Sousdomaines}
    } else  if (echele2 == "Régions écologiques"){
      namer <- "NOM"
      if (sousregion2 == "Toutes") {region <- "RE"} 
      else {region <- input$RegEcol}
    } else  if (echele2 ==  "Sous-région écologiques"){
      namer <- "NOM"
      if (sousregion2 == "Toutes") {region <- "SRE"} 
      else {region <- input$SousRegEcol}
    } else  if (echele2 == "Territoires guides"){
      namer <- "TER_GUIDE"
      if (sousregion2 == "Toutes") {region <- "TG"} 
      else {region <- input$Territoires}
    } else  if (echele2 == "Secteurs des opérations régionales"){
      namer <- "D_GENERAL"
      if (sousregion2 == "Toutes") {region <- "SOR"} 
      else {region <- input$Secteurs}
    } else  if (echele2 == "Régions forestières"){
      namer <- "NM_REG_FOR"
      if (sousregion2 == "Toutes") {region <- "RF"} 
      else {region <- input$RegForest}
    } else  if (echele2 == "Unités d’aménagement (UA)"){
      namer <- "PER_NO_UA"
      if (sousregion2 == "Toutes") {region <- "UA"} 
      else {region <- input$UA}
    }
    listrnr <- list(region, namer)
    return(listrnr)
  } 
  
  
  ### Function to change region for two or three regions
  regionf23 <- function(echele2, sousregion2){
    if (echele2 == 'Domaines bioclimatiques' ){
      namer <- "NOM"
      if (sousregion2 == "Toutes") {region <- "DB"} 
      else {region <- input$Domaines}
    } else  if (echele2 == "Sous-domaines bioclimatiques"){
      namer <- "NOM"
      if (sousregion2 == "Toutes") {region <- "SDB"} 
      else {region <- input$Sousdomaines}
    } else  if (echele2 == "Régions écologiques"){
      namer <- "NOM"
      if (sousregion2 == "Toutes") {region <- "RE"} 
      else {region <- input$RegEcol}
    } else  if (echele2 ==  "Sous-région écologiques"){
      namer <- "NOM"
      if (sousregion2 == "Toutes") {region <- "SRE"} 
      else {region <- input$SousRegEcol}
    } else  if (echele2 == "Territoires guides"){
      namer <- "TER_GUIDE"
      if (sousregion2 == "Toutes") {region <- "TG"} 
      else {region <- input$Territoires}
    } else  if (echele2 == "Secteurs des opérations régionales"){
      namer <- "D_GENERAL"
      if (sousregion2 == "Toutes") {region <- "SOR"} 
      else {region <- input$Secteurs}
    } else  if (echele2 == "Régions forestières"){
      namer <- "NM_REG_FOR"
      if (sousregion2 == "Toutes") {region <- "RF"} 
      else {region <- input$RegForest}
    } else  if (echele2 == "Unités d’aménagement (UA)"){
      namer <- "PER_NO_UA"
      if (sousregion2 == "Toutes") {region <- "UA"} 
      else {region <- input$UA}
    }
    listrnr <- list(region, namer)
    return(listrnr)
  } 
  
  
  
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
  
  
  ###### OBSERVE FOR FIRST REGION AND ALL REGIONS
  observe({
    ##Select region
    listr <- regionf(input$Echele, input$Sousregions) 
    print("region")
    region <- unlist(listr[1])
    print (region)
    namer <- unlist(listr[2])
  
    ##Default values
    period <- "hist"
    scenario <- "rcp45"
    percentile <- "50"
    
    ##Select Variable and season
    vari <- varif(input$Variable)
    saisonlist <- saisonf(input$Horizon)
    saison <- unlist(saisonlist[1])
    season2 <- unlist(saisonlist[2])
    all_selec <- unlist(saisonlist[3])

    ## Modify map
    fname2 <- fnamef(region, vari, saison)
    print("fname2")
    print (fname2)
    mapTG(region, namer, vari, period, saison, scenario, percentile, all_selec, fname2)
  })
  
 
  ####### DOWNLOAD BUTTON GEOJSON
  output$downloadData <- downloadHandler(
    filename <- function() { #
      listr <- regionf(input$Echele, input$Sousregions) 
      print("region")
      region <- unlist(listr[1])
      print (region)
      vari <- varif(input$Variable)
      saisonlist <- saisonf(input$Horizon)
      saison <- unlist(saisonlist[1])
      fname2 <- fnamef(region, vari, saison)
      print ("fname2")
      print (fname2)},
    content <- function(file) {
      listr <- regionf(input$Echele, input$Sousregions) 
      print("region")
      region <- unlist(listr[1])
      print (region)
      vari <- varif(input$Variable)
      saisonlist <- saisonf(input$Horizon)
      saison <- unlist(saisonlist[1])
      fname2 <- fnamef(region, vari, saison)
      print ("fname2")
      print (fname2)
      file.copy(fname2, file)}  ) 
 
  #  observe({
  #    if (input$Sousregions == "deux"){
  #      if (input$Echele == 'Domaines bioclimatiques'){
  #        namer <- "NOM"
  #        region <- input$Domaines2} 
  #   else if (input$Echele == "Sous-domaines bioclimatiques"){
  #   namer <- "NOM"
  #   region <- input$Sous-domaines2}
  #   else  if (input$Echele == "Régions écologiques"){
  #   namer <- "NOM"
  #   region <- input$RegEcol2}
  #   else  if (input$Echele ==  "Sous-région écologiques"){
  #   namer <- "NOM"
  #   region <- input$SousRegEcol2}
  #   else  if (input$Echele == "Territoires guides"){
  #   namer <- "TER_GUIDE"
  #   region <- input$Territoires2}
  #   else  if (input$Echele == "Secteurs des opérations régionales"){
  #   namer <- "D_GENERAL"
  #   region <- input$Secteurs2}
  #   else  if (input$Echele == "Régions forestières"){
  #   namer <- "NOM"
  #   region <- input$RegForest2}
  #   else  if (input$Echele == "Unités d'aménagement (UA)"){
  #   namer <- "NOM"
  #   region <- input$UA2}
  # }
  # #Default values
  # vari <- "tg_mean"
  # period <- "hist"
  # saisson <- "annual"
  # scenario <- "rcp45"
  # percentile <- "50"
  # #all_selec <- "hist_p50"
  # # if (input$PrecTotale) {
  # #   vari <- "prcptot"
  # #   print (vari)}
  # # if (input$Moyenne) {
  # #   vari <- "tg_mean"
  # #   print (vari)}
  # if (input$Horizon == 'Historique' || input$Horizon == '2041-2070' || input$Horizon =='2071-2100'){
  #   horizon <- input$Horizon
  #   scenario <- input$Scenario
  #   percentile <- input$Percentile
  #   all_selec <- conditions(horizon, scenario, percentile)}
  # mapTG(region, namer, vari, period, saisson, scenario, percentile, all_selec)
  #  })

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
  #   mapTG(region, vari, period, saison, scenario, percentile)
  # })
 
  
#######CLEAN UP BUTTON 
  observeEvent(  input$Nettoyer, {
    leafletProxy("map") %>%
    clearShapes()
  })
  

##### FOR THE TABLE  
  output$tabletest <- renderTable(
  {
    xtable(df,digits=2, type = "html", html.table.attributes="class='table-bordered'")
  },
  size="footnotesize", #Change size; useful for bigger tables
  include.rownames=FALSE, #Don't print rownames
  caption.placement="top",
  include.colnames=FALSE,
  add.to.row = list(pos = list(0),
                    command = "<tr><th><br> Saison </th><th><br> 1981-2010 </th><th colspan='2'> 2041-2070 </th><th  colspan='2'> 2071-2100 </th></tr>
                    <tr><th> </th><th> </th><th> Émissions moderées </th><th> Émissions élevées </th><th> Émissions moderées </th><th> Émissions élevées </th></tr>"))
  
  
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
    dygraph(dfts2, main = "Temperature Moyenne ")%>%
      dySeries("tg_mean_p10", drawPoints = TRUE, pointShape = "square", color = "pink") %>%
      dySeries("tg_mean_p50", stepPlot = TRUE, fillGraph = FALSE, color = "red") %>%
      dySeries("tg_mean_p90", drawPoints = TRUE, pointShape = "square", color = "pink")
  })

}
