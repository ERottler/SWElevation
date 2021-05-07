###

#Shiny dashboard to investigate elevation-dependent compensation effects in snowmelt
#Server
#Erwin Rottler, University of Potsdam

###

base_dir <- "/home/erwin/Nextcloud/pdoc_up/SWElevation/"

shinyServer(function(input, output) {
   
  #Leaflet map with all stations
  output$map <- renderLeaflet({
    
    m = leaflet() %>%
      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%
      addCircleMarkers(catch_usgs@data$lon_cen,
                       catch_usgs@data$lat_cen,
                       label = catch_usgs@data$lat_cen,
                       labelOptions = labelOptions(noHide = F, textOnly = F, direction = "top"),
                       stroke = F, group = "Centroids", fillOpacity = 0.8, fillColor = "#993300",
                       popup = catch_usgs@data$lat_cen,
                       clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {
                                                                                   var childCount = cluster.getChildCount();
                                                                                   if (childCount < 100) {
                                                                                   c = '#993300;'
                                                                                   } else if (childCount < 200) {
                                                                                   c = '#993300;'
                                                                                   } else {
                                                                                   c = '#993300;'
                                                                                   }
                                                                                   return new L.DivIcon({ html: '<div style=\"background-color:'+c+' color: #FFFFFF\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(50, 50) }) ;}"
                       )
                       )
                       )%>%
      addLayersControl(
        baseGroups = c("Terrain Background", "Open Street Map"),
        overlayGroups = c("Centroids"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = F)
      ) %>%
      fitBounds(lng1 = -50, lng2 = 50, lat1 = -30, lat2 = 60)
    #retrun map
    m
    
    })


  
})
