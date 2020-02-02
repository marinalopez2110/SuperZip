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
#TG  <- geojsonio::geojson_read("www/TG.json", what = "sp") # Deployment
# t1a <- geojsonio::geojson_read("www\\1a.json", what = "sp") #Deployment
# t2c <- geojsonio::geojson_read("www/2c.json", what = "sp") #Deployment
# t3d <- geojsonio::geojson_read("www/3d.json", what = "sp") #Deployment

#inputST <- reactive(paste(t,input$Territoires))


bins <- c('-5', '-4', '-2', '0', '2', '4', '6', '8', '10', '12', '14' )

palette <- function(inputST){
  pal <- colorBin("Spectral", domain = inputST$tg_mean, bins = bins)
  return (pal)
}

# pal <- colorBin("Spectral", domain = TG$tg_mean, bins = bins)
# pal1a <- colorBin("Spectral", domain = OneA$tg_mean, bins = bins)
# pal2c <- colorBin("Spectral", domain = TwoC$tg_mean, bins = bins)
# pal3d <- colorBin("Spectral", domain = ThreeD$tg_mean, bins = bins)

# labels <- sprintf("<strong>%s</strong><br/>%g °C", TG$TER_GUIDE, TG$tg_mean) %>% lapply(htmltools::HTML)
#labels <- as.character(tagList(tags$strong(HTML(sprintf("Region: %s", TG$TER_GUIDE))), tags$br(), sprintf("Temp: %s", TG$tg_mean)))
#labels  <- sprintf("Région: %s - Temp: %s", TG$TER_GUIDE, TG$tg_mean)
#labels <- sprintf("Région: %s - Temp: %s", inputST$TER_GUIDE, inputST$tg_mean) 
#labels2 <- sprintf("Région: %s - Temp: %s", TwoC$TER_GUIDE, TwoC$tg_mean) 
#labels3 <- sprintf("Région: %s - Temp: %s", ThreeD$TER_GUIDE, ThreeD$tg_mean)


function(input, output, session) {
  
 
  ## Interactive Map ###########################################
  
  # This observer is responsible for plotting temperature shapes,
  # according to the variables the user has chosen to map to color and size.
  observe({
    TG <- input$Territoires
    dataTG =  load_json(TG)
    #if (TG == '1a') {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
    pal <- palette(dataTG)
    
    leafletProxy("map", data = dataTG) %>%
      
        #clearShapes() %>%
      addPolygons (
            fillColor = pal(dataTG$tg_mean),
            data = dataTG, #OneA,
            weight = 1,
            opacity = 1,
            color = "black",
            dashArray = "3",
            fillOpacity = 0.8,
            # label = labels,
            # labelOptions = labelOptions(
            #   style = list("font-weight" = "normal", padding = "3px 8px"),
            #   textsize = "15px",
            #   direction = "auto")
            )
    # } else if (TG == '2c') {
    # leafletProxy("map", data = TwoC) %>%
    #  # clearShapes() %>%
    # addPolygons (
    #       fillColor = pal2c(TwoC$tg_mean),
    #       data =  TwoC,
    #       weight = 1,
    #       opacity = 1,
    #       color = "white",
    #       dashArray = "3",
    #       fillOpacity = 0.7,
    #       highlight = highlightOptions(
    #         weight = 2,
    #         color = "#666",
    #         dashArray = "",
    #         fillOpacity = 0.8,
    #         bringToFront = TRUE),
    #       label = labels2,
    #       labelOptions = labelOptions(
    #         style = list("font-weight" = "normal", padding = "3px 8px"),
    #         textsize = "15px",
    #         direction = "auto"))
 # }
  })
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet(map) %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lat = 45.6, lng= -70.5, zoom = 7)
  })
  
  # observe({
  #   TG2 <- input$Territoires2
  #   
  #   if (TG2 == '1a') {
  #     # Color and palette are treated specially in the "superzip" case, because
  #     # the values are categorical instead of continuous.
  #     leafletProxy("map", data = OneA) %>%
  #       #clearShapes() %>%
  #       addPolygons (
  #         fillColor = pal1a(OneA$tg_mean),
  #         data = OneA,
  #         weight = 1,
  #         opacity = 1,
  #         color = "black",
  #         dashArray = "3",
  #         fillOpacity = 0.8,
  #         label = labels1,
  #         labelOptions = labelOptions(
  #           style = list("font-weight" = "normal", padding = "3px 8px"),
  #           textsize = "15px",
  #           direction = "auto"))
  #   } else if (TG2 == '2c') {
  #     leafletProxy("map", data = TwoC) %>%
  #       #clearShapes() %>%
  #       addPolygons (
  #         fillColor = pal2c(TwoC$tg_mean),
  #         data =  TwoC,
  #         weight = 1,
  #         opacity = 1,
  #         color = "white",
  #         dashArray = "3",
  #         fillOpacity = 0.7,
  #         highlight = highlightOptions(
  #           weight = 2,
  #           color = "#666",
  #           dashArray = "",
  #           fillOpacity = 0.8,
  #           bringToFront = TRUE),
  #         label = labels2,
  #         labelOptions = labelOptions(
  #           style = list("font-weight" = "normal", padding = "3px 8px"),
  #           textsize = "15px",
  #           direction = "auto"))
  #   }
  # })
  
  
  # observe({
  #   TG3 <- input$Territoires3
  #   
  #   if (TG3 == '1a') {
  #     # Color and palette are treated specially in the "superzip" case, because
  #     # the values are categorical instead of continuous.
  #     leafletProxy("map", data = OneA) %>%
  #       #clearShapes() %>%
  #       addPolygons (
  #         fillColor = pal1a(OneA$tg_mean),
  #         data = OneA,
  #         weight = 1,
  #         opacity = 1,
  #         color = "black",
  #         dashArray = "3",
  #         fillOpacity = 0.8,
  #         label = labels1,
  #         labelOptions = labelOptions(
  #           style = list("font-weight" = "normal", padding = "3px 8px"),
  #           textsize = "15px",
  #           direction = "auto"))
  #   } else if (TG3 == '2c') {
  #     leafletProxy("map", data = TwoC) %>%
  #       #clearShapes() %>%
  #       addPolygons (
  #         fillColor = pal2c(TwoC$tg_mean),
  #         data =  TwoC,
  #         weight = 1,
  #         opacity = 1,
  #         color = "white",
  #         dashArray = "3",
  #         fillOpacity = 0.7,
  #         highlight = highlightOptions(
  #           weight = 2,
  #           color = "#666",
  #           dashArray = "",
  #           fillOpacity = 0.8,
  #           bringToFront = TRUE),
  #         label = labels2,
  #         labelOptions = labelOptions(
  #           style = list("font-weight" = "normal", padding = "3px 8px"),
  #           textsize = "15px",
  #           direction = "auto"))
  #   }
  # })
  

}
