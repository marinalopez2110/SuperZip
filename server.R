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


TG  <- geojsonio::geojson_read("www/TG.json", what = "sp") # Deployment
OneA <- geojsonio::geojson_read("www/OneA.json", what = "sp") #Deployment
TwoC <- geojsonio::geojson_read("www/TwoC.json", what = "sp") #Deployment
ThreeD <- geojsonio::geojson_read("www/ThreeD.json", what = "sp") #Deployment

bins <- c(14, 12, 10, 8, 6, 4, 2, 0, -2, -4, -5)

pal <- colorBin("Spectral", domain = TG$tg_mean, bins = bins)
pal1a <- colorBin("Spectral", domain = OneA$tg_mean, bins = bins)
pal2c <- colorBin("Spectral", domain = TwoC$tg_mean, bins = bins)
pal3d <- colorBin("Spectral", domain = ThreeD$tg_mean, bins = bins)

# labels <- sprintf("<strong>%s</strong><br/>%g °C", TG$TER_GUIDE, TG$tg_mean) %>% lapply(htmltools::HTML)
#labels <- as.character(tagList(tags$strong(HTML(sprintf("Region: %s", TG$TER_GUIDE))), tags$br(), sprintf("Temp: %s", TG$tg_mean)))
labels  <- sprintf("Région: %s - Temp: %s", TG$TER_GUIDE, TG$tg_mean)
labels1 <- sprintf("Région: %s - Temp: %s", OneA$TER_GUIDE, OneA$tg_mean) 
labels2 <- sprintf("Région: %s - Temp: %s", TwoC$TER_GUIDE, TwoC$tg_mean) 
labels3 <- sprintf("Région: %s - Temp: %s", ThreeD$TER_GUIDE, ThreeD$tg_mean)


function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet(OneA) %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lat = 45.6, lng= -70.5, zoom = 7)
    # %>%
    #   addPolygons (
    #     fillColor = pal1a(OneA$tg_mean),
    #     data = OneA,
    #     weight = 1,
    #     opacity = 1,
    #     color = "black",
    #     dashArray = "3",
    #     fillOpacity = 0.8,
    #     label = labels1,
    #     labelOptions = labelOptions(
    #       style = list("font-weight" = "normal", padding = "3px 8px"),
    #       textsize = "15px",
    #       direction = "auto")
    #     )%>%
    #   addPolygons (
    #     fillColor = pal2c(TwoC$tg_mean),
    #     data =  TwoC,
    #     weight = 1,
    #     opacity = 1,
    #     color = "white",
    #     dashArray = "3",
    #     fillOpacity = 0.7,
    #     highlight = highlightOptions(
    #       weight = 2,
    #       color = "#666",
    #       dashArray = "",
    #       fillOpacity = 0.8,
    #       bringToFront = TRUE),
    #     label = labels2,
    #     labelOptions = labelOptions(
    #       style = list("font-weight" = "normal", padding = "3px 8px"),
    #       textsize = "15px",
    #       direction = "auto"))%>%
    #   addPolygons (
    #     fillColor = pal3d(ThreeD$tg_mean),
    #     data =  ThreeD,
    #     weight = 1,
    #     opacity = 1,
    #     color = "red",
    #     dashArray = "3",
    #     fillOpacity = 0.7,
    #     highlight = highlightOptions(
    #       weight = 2,
    #       color = "#666",
    #       dashArray = "",
    #       fillOpacity = 0.8,
    #       bringToFront = TRUE),
    #     label = labels3,
    #     labelOptions = labelOptions(
    #       style = list("font-weight" = "normal", padding = "3px 8px"),
    #       textsize = "15px",
    #       direction = "auto"))%>%
    #   addLegend(pal = pal, values = ~TG$tg_mean, title = "Température (°C)", opacity = 0.7, 
    #             position = "bottomright")
  })
  
  
  # This observer is responsible for plotting temperature shapes,
  # according to the variables the user has chosen to map to color and size.
  observe({
    TG <- input$Territoires
    
    if (TG == '1a') {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      leafletProxy("map", data = OneA) %>%
        #clearShapes() %>%
      addPolygons (
            fillColor = pal1a(OneA$tg_mean),
            data = OneA,
            weight = 1,
            opacity = 1,
            color = "black",
            dashArray = "3",
            fillOpacity = 0.8,
            label = labels1,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"))
    } else if (TG == '2c') {
    leafletProxy("map", data = TwoC) %>%
     # clearShapes() %>%
    addPolygons (
          fillColor = pal2c(TwoC$tg_mean),
          data =  TwoC,
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 2,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.8,
            bringToFront = TRUE),
          label = labels2,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))
  }
  })
  
  observe({
    TG2 <- input$Territoires2
    
    if (TG2 == '1a') {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      leafletProxy("map", data = OneA) %>%
        #clearShapes() %>%
        addPolygons (
          fillColor = pal1a(OneA$tg_mean),
          data = OneA,
          weight = 1,
          opacity = 1,
          color = "black",
          dashArray = "3",
          fillOpacity = 0.8,
          label = labels1,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))
    } else if (TG2 == '2c') {
      leafletProxy("map", data = TwoC) %>%
        #clearShapes() %>%
        addPolygons (
          fillColor = pal2c(TwoC$tg_mean),
          data =  TwoC,
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 2,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.8,
            bringToFront = TRUE),
          label = labels2,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))
    }
  })
  
  
  observe({
    TG3 <- input$Territoires3
    
    if (TG3 == '1a') {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      leafletProxy("map", data = OneA) %>%
        #clearShapes() %>%
        addPolygons (
          fillColor = pal1a(OneA$tg_mean),
          data = OneA,
          weight = 1,
          opacity = 1,
          color = "black",
          dashArray = "3",
          fillOpacity = 0.8,
          label = labels1,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))
    } else if (TG3 == '2c') {
      leafletProxy("map", data = TwoC) %>%
        #clearShapes() %>%
        addPolygons (
          fillColor = pal2c(TwoC$tg_mean),
          data =  TwoC,
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 2,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.8,
            bringToFront = TRUE),
          label = labels2,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))
    }
  })
  

}