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
  return(all_selec)}


##### INTERMIDIATE STEP *** CHECK IF STILL NEED IT
mapTG <- function(region, namer, vari, period, saison, scenario, percentile, all_selec, fname2){ 
  dataTG <- load_json(fname2) ### THIS STEP TAKES VERY LONG
  vari <- vari
  addmapr(dataTG, vari, region, namer, period, scenario, percentile, all_selec) 
}

####### Get the name of the file
fnamef <- function (region, vari, saison){
  #SUBSTITUTE ACCENTS
  nameA <- str_replace_all(region, c( "é"= "e", "à"="a", "è"= "e", "ô" = "o", "ç"="c", "É"="E", "È"="E", "Î"="i", "Ç"="C"))
  fname3 <- paste("www/",nameA,"_", vari, "_",saison, ".json",sep="")
  return(fname3)}

###### LOAD GEOJSON FILE
load_json <- function (fname2){
  #print ("load json")
  geojsonio::geojson_read(fname2, what = "sp")
 # filename(nameA, vari, saison)
}

#### MODIFY MAP BASED ON SELECTION - LEAFLETPROXY
addmapr <- function(dataTG, vari, region, namer, period, scenario, percentile, all_selec){ 
    labels <- sprintf("Région: %s : %s", dataTG[[namer]], dataTG[[all_selec]]) 
    if(vari == "tg_mean"){
    pal <- colorNumeric("Spectral", domain = dataTG[[all_selec]])
    title <- sprintf("Température Moy (°C) -%s", all_selec)
    values <-dataTG[[all_selec]]}
    else if(vari == "tn_mean"){
      pal <- colorNumeric("Spectral", domain = dataTG[[all_selec]])
      title <- sprintf("Température Min (°C) -%s", all_selec)
      values <- dataTG[[all_selec]]}
    else if(vari == "tx_mean"){
      pal <- colorNumeric("Spectral", domain = dataTG[[all_selec]])
      title <- sprintf("Température Max (°C) -%s", all_selec)
      values <- dataTG[[all_selec]]
      print("title")}
    else if(vari == "prcptot"){
      pal <- colorNumeric("Spectral", domain = dataTG[[all_selec]])
      title <- sprintf("Précipitation totale (mm) -%s", all_selec)
      values <- dataTG[[all_selec]]
      print("title")} 
    else if(vari == "solidprcptot"){
      pal <- colorNumeric("Spectral", domain = dataTG[[all_selec]])
      title <- sprintf("Précipitation neige (cm) -%s", all_selec)
      values <- dataTG[[all_selec]]
      print("title")} 
    else if(vari == "DJC"){
      pal <- colorNumeric("Spectral", domain = dataTG[[all_selec]])
      title <- sprintf("Degrés-jours de croissance (DJC) -%s", all_selec)
      values <- dataTG[[all_selec]]
      print("title")} 
    else if(vari == "dlyfrzthw"){
      pal <- colorNumeric("Spectral", domain = dataTG[[all_selec]])
      title <- sprintf("Évènements gel-dégel -%s", all_selec)
      values <- dataTG[[all_selec]]
      print("title")} 
    else if(vari == "growing_season_length"){
      pal <- colorNumeric("Spectral", domain = dataTG[[all_selec]])
      title <- sprintf("Saison de croissance -%s", all_selec)
      values <- dataTG[[all_selec]]
      print("title")} 
    fillColor <- pal(dataTG[[all_selec]])
  return(leafletProxy("map", data = dataTG) %>%
   clearControls() %>%
    # clearShapes() %>%
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
      else {region <- input$UA}    }
    listrnr <- list(region, namer)
    return(listrnr)  } 
  
  ### Function to change region for region2
  regionf2 <- function(echele2, sousregion2){
    print (sousregion2)
    if (echele2 == 'Domaines bioclimatiques' ){
      namer <- "NOM"
      region <- input$Domaines2}
    else  if (echele2 == "Sous-domaines bioclimatiques"){
      namer <- "NOM"
      region <- input$Sousdomaines2}
    else  if (echele2 == "Régions écologiques"){
      namer <- "NOM"
      region <- input$RegEcol2}
     else  if (echele2 ==  "Sous-région écologiques"){
      namer <- "NOM"
      region <- input$SousRegEcol2}
     else  if (echele2 == "Territoires guides"){
      namer <- "TER_GUIDE"
      region <- input$Territoires2}
     else  if (echele2 == "Secteurs des opérations régionales"){
      namer <- "D_GENERAL"
      region <- input$Secteurs2}
     else  if (echele2 == "Régions forestières"){
      namer <- "NM_REG_FOR"
      region <- input$RegForest2}
     else  if (echele2 == "Unités d’aménagement (UA)"){
      namer <- "PER_NO_UA"
      region <- input$UA2}
    listrnr <- list(region, namer)
    return(listrnr)
    }

  ### Function to change region for region3
    regionf3 <- function(echele2, sousregion2){
      print (sousregion2)
      if (echele2 == 'Domaines bioclimatiques' ){
        namer <- "NOM"
        region <- input$Domaines3}
      else  if (echele2 == "Sous-domaines bioclimatiques"){
        namer <- "NOM"
        region <- input$Sousdomaines3}
      else  if (echele2 == "Régions écologiques"){
        namer <- "NOM"
        region <- input$RegEcol3}
      else  if (echele2 ==  "Sous-région écologiques"){
        namer <- "NOM"
        region <- input$SousRegEcol3}
      else  if (echele2 == "Territoires guides"){
        namer <- "TER_GUIDE"
        region <- input$Territoires3}
      else  if (echele2 == "Secteurs des opérations régionales"){
        namer <- "D_GENERAL"
        region <- input$Secteurs3}
      else  if (echele2 == "Régions forestières"){
        namer <- "NM_REG_FOR"
        region <- input$RegForest3}
      else  if (echele2 == "Unités d’aménagement (UA)"){
        namer <- "PER_NO_UA"
        region <- input$UA3}
    listrnr <- list(region, namer)
    return(listrnr)
    }
  
  ### CHANGING VARIABLE
  varif  <- function(Variable2){
    if (Variable2 == "Précipitations sous forme de neige") {
      vari <- "solidprcptot"
      print ("button neige")}
    if (Variable2 == "Précipitations totales") {
      vari <- "prcptot"
      print ("button precip")}
    if (Variable2 == "Températures moyennes") {
      vari <- "tg_mean"}
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
  
   ## Interactive Map ###########################################
  # Create the map
  output$map <- renderLeaflet({
    leaflet(map) %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lat = 47.5, lng= -72.5, zoom = 7)
  })
  
  
  ###### OBSERVE FOR FIRST REGION AND ALL REGIONS
  observe({
    ##Select region
    listr <- regionf(input$Echele, input$Sousregions) 
    region <- unlist(listr[1])
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
    #print("fname2")
    #print (fname2)
    mapTG(region, namer, vari, period, saison, scenario, percentile, all_selec, fname2)
  })
  
  ###### OBSERVE FOR SECOND REGION
  observe({
    if (input$Sousregions =='deux' || input$Sousregions =='trois' ){
    ##Select region
    print("region 2")
    print(input$Sousregions)
    listr <- regionf2(input$Echele, input$Sousregions)
    
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
    #print("fname2")
    #print (fname2)
    mapTG(region, namer, vari, period, saison, scenario, percentile, all_selec, fname2)}
  })
  
  ###### OBSERVE FOR THIRD REGION
  observe({
    if (input$Sousregions =='trois'){
      ##Select region
      print("region 3")
      print(input$Sousregions)
      listr <- regionf3(input$Echele, input$Sousregions)
      
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
      #print("fname2")
      #print (fname2)
      mapTG(region, namer, vari, period, saison, scenario, percentile, all_selec, fname2)}
  })
  
  
 
  ####### DOWNLOAD BUTTON GEOJSON
  output$downloadData <- downloadHandler(
    filename <- function() { #
      listr <- regionf(input$Echele, input$Sousregions) 
      region <- unlist(listr[1])
      # print (region)
      vari <- varif(input$Variable)
      saisonlist <- saisonf(input$Horizon)
      saison <- unlist(saisonlist[1])
      fname2 <- fnamef(region, vari, saison)
      #print ("fname2")
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
 

  
#######CLEAN UP BUTTON 
  observeEvent(  input$Nettoyer, {
    leafletProxy("map") %>%
    clearShapes()
  })
  

##### FOR THE TABLE  
  
  ### Reactive funcions to select the space scale
  
  EcheleT <- reactive(input$EcheleT)
  sousechelleT <- reactive({ if (EcheleT() == "Régions forestières") {input$RegForestT} 
    else if (EcheleT() == "Territoires guides") {input$TerritoiresT}
    else if (EcheleT() == "Domaines bioclimatiques") {input$DomainesT}
    else if (EcheleT() == "Sous-domaines bioclimatiques") {input$SousdomainesT}
    else if (EcheleT() == "Régions écologiques") {input$RegEcoLT}
    else if (EcheleT() == "Sous-région écologiques") {input$SousRegEcoLT}
    else if (EcheleT() == "Secteurs des opérations régionales") {input$SecteursT}
    else if (EcheleT() == "Unités d’aménagement (UA)") {input$UAT}})
  
  variT <- reactive({
  if (input$VariableT == "Précipitations sous forme de neige") {varit1 <- "solidprcptot"}
  else if (input$VariableT == "Précipitations totales") {varit1 <- "prcptot"}
  else if (input$VariableT == "Températures moyennes") {varit1 <- "tg_mean"}
  else if (input$VariableT == "Températures maximales") {varit1 <- "tx_mean"}
  else if (input$VariableT == "Températures minimales") {varit1 <- "tn_mean"}
  else if (input$VariableT == "Degrés-jours de croissance"){varit1 <- "DJC"}
  else if (input$VariableT == "Évènements gel-dégel"){varit1 <- "dlyfrzthw"}
  else if (input$VariableT == "Saison de croissance"){varit1 <- "growing_season_length"}
  varit1})
  

  # observe(
  #   #print ("table"),
  #   print(sousechelleT())
  #   )
  observeEvent( input$EcheleT, { 
    print ("table")
    print(sousechelleT())    })
  observeEvent( input$VariableT, { 
    print(variT())    })
  

  
  ### Output function
  output$tabletest <- renderTable(
    
  {
    ###### LOAD TABLE
    nameATO <- str_replace_all(sousechelleT(), c( "é"= "e", "à"="a", "è"= "e", "ô" = "o", "ç"="c", "É"="E", "È"="E", "Î"="i", "Ç"="C"))
    df <- read.table(paste("www/",nameATO,"_",variT(),"_","table.csv", sep=''), header = TRUE, sep = ",")
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
  variTS <- reactive({
  if (input$VariableTS == "Précipitations sous forme de neige") {varits1 <- "solidprcptot"}
  else if (input$VariableTS == "Précipitations totales") {varits1 <- "prcptot"}
  else if (input$VariableTS == "Températures moyennes") {varits1 <- "tg_mean"}
  else if (input$VariableTS == "Températures maximales") {varits1 <- "tx_mean"}
  else if (input$VariableTS == "Températures minimales") {varits1 <- "tn_mean"}
  else if (input$VariableTS == "Degrés-jours de croissance"){varits1 <- "DJC"}
  else if (input$VariableTS == "Évènements gel-dégel"){varits1 <- "dlyfrzthw"}
  else if (input$VariableTS == "Saison de croissance"){varits1 <- "growing_season_length"}
  varits1})
  
  
  seasonTS <- reactive({
    if (input$SaisonnaliteTS == "Annuel") {season1 <- "annual"}
    else if (input$seasonTS == "winterTS") {season1 <- "winter"}
    else if (input$seasonTS == "springTS") {season1 <- "spring"}
    else if (input$seasonTS == "summerTS") {season1 <- "summer"}
    else if (input$seasonTS == "fallTS") {season1 <- "fall"}
    else if (input$MoisTS == "januaryrTS"){season1 <- "january"}
    else if (input$MoisTS == "februaryTS"){season1 <- "february"}
    else if (input$MoisTS == "marchTS"){season1 <- "march"}
    else if (input$MoisTS == "aprilTS"){season1 <- "april"}
    else if (input$MoisTS == "mayTS"){season1 <- "may"}
    else if (input$MoisTS == "juneTS"){season1 <- "june"}
    else if (input$MoisTS == "julyTS"){season1 <- "july"}
    else if (input$MoisTS == "augustTS"){season1 <- "august"}
    else if (input$MoisTS == "septemberTS"){season1 <- "september"}
    else if (input$MoisTS == "octoberTS"){season1 <- "october"}
    else if (input$MoisTS == "novemberTS"){season1 <- "november"}
    else if (input$MoisTS == "decemberTS"){season1 <- "december"}
    season1})
  
  
    ### Reactive funcions to select the space scale
  
  echelleTS <- reactive(input$echelleTS)
  sousechelleTS <- reactive({ if (echelleTS() == "Régions forestières") {input$RegForestTS} 
    else if (echelleTS() == "Territoires guides") {input$TerritoiresTS}
    else if (echelleTS() == "Domaines bioclimatiques") {input$DomainesTS}
    else if (echelleTS() == "Sous-domaines bioclimatiques") {input$SousdomainesTS}
    else if (echelleTS() == "Régions écologiques") {input$RegEcoLTS}
    else if (echelleTS() == "Sous-région écologiques") {input$SousRegEcoLTS}
    else if (echelleTS() == "Secteurs des opérations régionales") {input$SecteursTS}
    else if (echelleTS() == "Unités d’aménagement (UA)") {input$UATS}})
  observe(
    print(sousechelleTS()))
  observeEvent( input$VariableTS, { 
      print(variTS())    })
  observeEvent( input$SaisonnaliteTS, { 
      print(seasonTS()) })
  
  columnsts <- function(variTS){
    p10_45 <- paste(variTS,"_p10_45", sep="")
    p50_45 <- paste(variTS,"_p50_45", sep="")
    p90_45 <- paste(variTS,"_p90_45", sep="")
    p10_85 <- paste(variTS,"_p10_85", sep="")
    p50_85 <- paste(variTS,"_p50_85", sep="")
    p90_85 <- paste(variTS,"_p90_85", sep="")
    Obs <- paste(variTS,"_Obs",  sep="")
    listcol <- list(p10_45,p50_45, p90_45, p10_85, p50_85,p90_85,Obs)
    return(listcol)
  }
  
  
  output$dygraph <- renderDygraph({
    nameATS <- str_replace_all(sousechelleTS(), c( "é"= "e", "à"="a", "è"= "e", "ô" = "o", "ç"="c", "É"="E", "È"="E", "Î"="i", "Ç"="C"))
    dfts <- read.csv(paste("www/",nameATS,"_",variTS(),"_", seasonTS(),".csv", sep=''))
    listcol <- columnsts (variTS())
    p1045 <- unlist(listcol[1])
    p5045 <- unlist(listcol[2])
    p9045 <- unlist(listcol[3])
    p1085 <- unlist(listcol[4])
    p5085 <- unlist(listcol[5])
    p9085 <- unlist(listcol[6])
    Obsts <- unlist(listcol[7])
    rownames(dfts) <- dfts$time
        keep <- c( "time",p1045, p5045, p9045, p1085, p5085,p9085,Obsts)

    dfts2  <- dfts[ , keep]
    dygraph(dfts2, main = input$VariableTS)%>%
      dySeries(p1085, drawPoints = TRUE, pointShape = "square", color = "pink") %>%
      dySeries(p5085, stepPlot = TRUE, fillGraph = FALSE, color = "red") %>%
      dySeries(p9085, drawPoints = TRUE, pointShape = "square", color = "pink")%>%
      dySeries(p1045, drawPoints = TRUE, pointShape = "square", color = "blue") %>%
      dySeries(p5045, stepPlot = TRUE, fillGraph = FALSE, color = "green") %>%
      dySeries(p9045, drawPoints = TRUE, pointShape = "square", color = "blue")%>%
      dySeries(Obsts, drawPoints = TRUE, pointShape = "square", color = "orange")
  })
  
  
  output$downloadDataTS <- downloadHandler(
    filename <- function() { #
      nameATS <- str_replace_all(sousechelleTS(), c( "é"= "e", "à"="a", "è"= "e", "ô" = "o", "ç"="c", "É"="E", "È"="E", "Î"="i", "Ç"="C"))
      fnameTS <- paste("www/",nameATS,"_",variTS(),"_", seasonTS(),".csv", sep='')
      # print ("fname2")
       print (fnameTS)
      },
    content <- function(file) {
      nameATS <- str_replace_all(sousechelleTS(), c( "é"= "e", "à"="a", "è"= "e", "ô" = "o", "ç"="c", "É"="E", "È"="E", "Î"="i", "Ç"="C"))
      fnameTS <- paste("www/",nameATS,"_",variTS(),"_", seasonTS(),".csv", sep='')
      print ("fnameTS")
      print (fnameTS)
      file.copy(fnameTS, file)}  ) 

}
