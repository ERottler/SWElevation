###

#Shiny dashboard to investigate elevation-dependent compensation effects in snowmelt
#Server
#Erwin Rottler, University of Potsdam

###

app_dir <- "/home/erwin/Nextcloud/pdoc_up/SWElevation/R/SWElevation/"

#read data for app
load(paste0(app_dir, "data/load_data.RData"))

#Initial dummy catchment
catch_sel <- sp::Polygon(matrix(rnorm(10, 0, 0.01), ncol = 2))

#Initial dummy gauge for plotly selection
df <-data.frame(x = 0, y = 0)
gauge_plotly <-SpatialPoints(coords = df)

#folder names is IDs of stations analyzed
stat_id_sel <- list.dirs(paste0(app_dir, "data/"), full.names = F)[-1]
stat_id_ind <- which(grdc_meta$id %in% stat_id_sel)

shinyServer(function(input, output) {
  
  # #colors for hydrological response units (HUs)
  # hu_cols <- c("navyblue", "royalblue3", "steelblue2", "slategray2", "cadetblue3", 
  #              "turquoise4","yellow2", "gold3", "orange2", "sienna2", "tomato3", "plum2", 
  #              "orchid4", "violetred2", "red2", "darkred", "limegreen", "seagreen2")
  # hu_cols_num <- colorNumeric(hu_cols, 1:18) 
  # 
  #Leaflet map with all stations
  output$map <- renderLeaflet({
    
    crswgs84 <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    ind_outlet <- which(grdc_catch@data$grdc_no == "4115201") #Columbia River
    ind_select <- which(grdc_catch@data$grdc_no %in% stat_id_sel) #Watersheds analysed
    catch_outlet <- grdc_catch[ind_outlet, ]
    catch_select <- grdc_catch[ind_select, ]
    catch_outlet <- spTransform(catch_outlet, crswgs84)
    catch_select <- spTransform(catch_select, crswgs84)
    
    m = leaflet() %>%
      addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen.TerrainBackground") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,        group = "OpenStreetMap") %>%
      addPolygons(data = catch_sel, layerId = "watershed", group = "Watershed") %>%
      addPolygons(data = catch_outlet, layerId = "Columbia", fill = F, weight = 2.0,
                  color = "#000000", opacity = 0.8, group = "Columbia") %>%
      addPolygons(data = catch_select[,], fill = F, weight = 1.2,
                  color = "#000000", opacity = 1.0, group = "Watersheds") %>%
      # addMeasure(
      #   primaryLengthUnit = "kilometers",
      #   secondaryAreaUnit = FALSE
      # )%>%
      # addCircleMarkers(data = gauge_plotly, layerId = "plotly_gauge", group = "Plotly Click") %>%
      addCircleMarkers(grdc_meta$longitude[stat_id_ind],
                       grdc_meta$latitude[stat_id_ind],
                       label = grdc_meta$name[stat_id_ind],
                       labelOptions = labelOptions(noHide = F, textOnly = F, direction = "top"),
                       stroke = F, group = "Outlets", fillOpacity = 0.8, fillColor = "#000000",
                       popup = grdc_meta$name[stat_id_ind]#,
                       # clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {
                       #                                                             var childCount = cluster.getChildCount();
                       #                                                             if (childCount < 100) {
                       #                                                             c = '#333333;'
                       #                                                             } else if (childCount < 200) {
                       #                                                             c = '#333333;'
                       #                                                             } else {
                       #                                                             c = '#333333;'
                       #                                                             }
                       #                                                             return new L.DivIcon({ html: '<div style=\"background-color:'+c+' color: #FFFFFF\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(50, 50) }) ;}"
                       # )
                       # )
                       ) %>%
      addLayersControl(
        baseGroups = c("Stamen.TerrainBackground", "OpenStreetMap"),
        overlayGroups = c("Outlets", "Watershed"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = F)
      ) %>%
      fitBounds(lng1 = -112, lng2 = -95, lat1 = 35, lat2 = 55) 
      # hideGroup("Plotly Click")
    
    #retrun map
    m
    
    # for(i in 1:18){
    #   
    #   ind_sel <- which(usgs_meta$hu == i)
    #   
    #   m <- m %>%
    #     addCircleMarkers(usgs_meta$longitude[ind_sel],
    #                      usgs_meta$latitude[ind_sel],
    #                      label = paste0(usgs_meta$name[ind_sel]),
    #                      labelOptions = labelOptions(noHide = F, textOnly = F, direction = "right"),
    #                      stroke = F, group = "Outlets", fillOpacity = 0.9, fillColor = hu_cols_num(i),
    #                      popup = paste(
    #                        usgs_meta$name[ind_sel], '<br>',
    #                        "HU:", usgs_meta$hu[ind_sel], '<br>',
    #                        "ID:", usgs_meta$id[ind_sel], '<br>',
    #                        "<extra></extra>"
    #                      )
    #     )
    #   
    # }
    
    
    })

  #Initial conditions plot: 'Select watershed outlet on map.'
  f_plot <- function(){
    
    par(bg="transparent")
    plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    mtext("Select watershed outlet on map.", line = -1, cex = 1.5, col = "white")
    
  }
  
  output$elev_plot <- renderPlot({f_plot()})
  
  #Dummy which gets selected gauge
  gauge_sel <-  shiny::reactiveValues(clicked_gauge = "XXX")
  
  #Reaction to selection of station on map
  observeEvent(input$map_marker_click,{
    
    gauge_sel$clicked_gauge <- input$map_marker_click
    
    stat_sel <- which(grdc_meta$latitude == gauge_sel$clicked_gauge$lat)
    
    stat_name <- trimws(grdc_meta$name[stat_sel], which = "both") #station name
    if(nchar(stat_name) > 35){
      stat_name_max35 <- paste0(substr(stat_name, 1, 30), " [...]")
    }else{
      stat_name_max35 <- stat_name
    }
    
    sta_id <- grdc_meta$id[stat_sel] #station id
    
    #load data for selected gauge
    load(paste0(app_dir, "data/", sta_id, "/res_dashb.RData"))
    load(paste0(app_dir, "data/", sta_id, "/elevs_sel.RData"))
    
    #select watershed boundaries for selected gauge
    sel_ind <- which(grdc_catch@data$grdc_no == sta_id)
    
    crswgs84 <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    catch_sel_raw <- grdc_catch[sel_ind, ]
    catch_sel <- spTransform(catch_sel_raw, crswgs84)
    
    #calculate area of selected catchment
    catch_sel_area <- raster::area(catch_sel)/1000000 #[km²]
    
    #Update leaflat map and show watershed selected
    leafletProxy("map") %>%
      # removeMarker(layerId = "plotly_gauge") %>%
      removeShape(layerId = "watershed") %>%
      addPolygons(data = catch_sel, layerId = "watershed", fill = F,
                  color = "#990000", opacity = 0.9, group = "Watershed") %>%
      addLayersControl(
        baseGroups = c("Stamen.TerrainBackground", "OpenStreetMap"),
        overlayGroups = c("Outlets", "Watershed"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = F)
      )
    
    f_plot <- function(){
      
      par(bg="transparent")
      
      if(input$plot_type == "elevdist"){
        
        elev_bands <- my_elev_bands
        
        bands_frac <- NULL
        for(i in 1:(length(my_elev_bands)-1)){
          
          numb_inside <- length(which(elevs_sel >= my_elev_bands[i] & elevs_sel < my_elev_bands[i+1]))
          frac_inside <- numb_inside/length(elevs_sel)*100 #[%]
          bands_frac <- c(bands_frac, frac_inside)
          
        }
        
        col_background <- rgb(45, 45, 45, maxColorValue = 255)
        
        par(mar = c(3.5, 3.5, 2, 0.5))
        par(bg = col_background)
        
        plot(bands_frac, my_elev_bands[-length(my_elev_bands)], type = "n", 
             xlim = c(0, max(bands_frac, na.rm = T)+2), ylim = c(0, 4500),
             axes = F, ylab = "", xlab = "", xaxs = "i", col.axis = "white")
        abline(h = seq(0, 4000, 500), lty = "dashed", col = "grey55")
        abline(v = seq(0, 100, 10), lty = "dashed", col = "grey55")

        for(i in 1:length(bands_frac)){

          segments(x0 = 0, y0 = my_elev_bands[i],
                   x1 =  bands_frac[i], y1 = my_elev_bands[i], col = "steelblue3", lwd = 5, lend = 2)

        }
        axis(1, mgp=c(3, 0.25, 0), tck = -0.009, cex.axis = 1.2, col = "white", col.axis = "white")
        axis(2, mgp=c(3, 0.25, 0), tck = -0.009, cex.axis = 1.2, col = "white", col.axis = "white")
        mtext(expression(paste("Elevation [m]")), side = 2, line = 2.0, cex = 1.4, col = "white")
        mtext(expression(paste("Areal fraction [%]")), side = 1, line = 2.0, cex = 1.4, col = "white")
        mtext(stat_name_max35, line = 0.2, side = 3, cex = 1.5, adj = 0, col = "white")
        mtext(paste0("Total area: ", round(catch_sel_area, 0), " km²"), line = 0.2, side = 3,
              cex = 1.1, adj = 1.0, col = "white")
        graphics::box(col = "white")

        
      }

      if(input$plot_type == "smea_ann"){
        
        x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
        x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
        
        col_background <- rgb(45, 45, 45, maxColorValue = 255)
        
        my_col <- viridis::viridis(40, direction = 1, option = "D")[8:40]
        my_col <- colorRampPalette(c(col_background, my_col))(200)
        
        my_bre <- seq(min(smea_ann_band, na.rm = T), max(smea_ann_band, na.rm = T), length.out = length(my_col)+1)
        
        par(mar = c(3.5, 4.5, 2, 0.0))
        par(bg = col_background)
        
        layout(matrix(c(1,1,1,1,1,1,1,2),
                      1, 8), widths=c(), heights=c())
        
        image(x = 1:365,
              y = my_elev_bands[-length(my_elev_bands)],
              z = smea_ann_band, col =my_col, breaks = my_bre,
              ylab = "", xlab = "", axes = F)
        axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
             col = "white", col.axis = "white", tck = -0.04, cex.axis = 1.5)#plot ticks
        axis(1, at = x_axis_lab, c("O","N","D","J","F","M","A","M","J","J","A","S"), tick = FALSE,
             col="white", col.axis="white", mgp=c(3, 0.65, 0), cex.axis = 1.7)#plot labels
        axis(2, mgp=c(3, 0.45, 0), tck = -0.009, cex.axis = 1.8, col = "white", col.axis = "white")
        mtext(expression(paste("Elevation [m]")), side = 2, line = 2.2, cex = 1.4, col = "white")
        mtext(stat_name_max35, line = 0.2, side = 3, cex = 1.5, adj = 0, col = "white")
        mtext("[mm]", line = 0.2, side = 3, cex = 1.2, adj = 1, col = "white")
        graphics::box(col = "white")
        
        
        par(mar = c(3.5, 0.5, 2, 4.0))
        
        image_scale(as.matrix(smea_ann_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
        axis(4, mgp=c(3, 0.85, 0), tck = -0.12, col.axis = "white", cex.axis = 1.8)
        graphics::box(col = "white")
        
        
      }
      
      if(input$plot_type == "sslo_ann"){
        
        x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
        x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
        
        col_background <- rgb(45, 45, 45, maxColorValue = 255)
        
        cols_min <- colorRampPalette(c("yellow2" ,"gold3", "darkgoldenrod3","darkorange3", "darkorange4", "darkred", col_background))(100)
        cols_max <- colorRampPalette(c(col_background, viridis::viridis(9, direction = 1)[3:4], "lightskyblue2", "lightskyblue1", "grey95"))(100)
        my_col <- c(cols_min, cols_max)
        my_bre <- seq(-max_na(abs(sslo_ann_band)), max_na(abs(sslo_ann_band)), length.out = length(my_col)+1)
        
        par(mar = c(3.5, 4.5, 2, 0.0))
        par(bg = col_background)
        
        layout(matrix(c(1,1,1,1,1,1,1,2),
                      1, 8), widths=c(), heights=c())
        
        image(x = 1:365,
              y = my_elev_bands[-length(my_elev_bands)],
              z = sslo_ann_band, col =my_col, breaks = my_bre,
              ylab = "", xlab = "", axes = F)
        axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
             col = "white", col.axis = "white", tck = -0.04, cex.axis = 1.5)#plot ticks
        axis(1, at = x_axis_lab, c("O","N","D","J","F","M","A","M","J","J","A","S"), tick = FALSE,
             col="white", col.axis="white", mgp=c(3, 0.65, 0), cex.axis = 1.7)#plot labels
        axis(2, mgp=c(3, 0.45, 0), tck = -0.009, cex.axis = 1.8, col = "white", col.axis = "white")
        mtext(expression(paste("Elevation [m]")), side = 2, line = 2.2, cex = 1.4, col = "white")
        mtext(stat_name_max35, line = 0.2, side = 3, cex = 1.5, adj = 0, col = "white")
        mtext("[mm]", line = 0.2, side = 3, cex = 1.2, adj = 1, col = "white")
        graphics::box(col = "white")
        
        
        par(mar = c(3.5, 0.5, 2, 4.0))
        
        image_scale(as.matrix(sslo_ann_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
        axis(4, mgp=c(3, 0.85, 0), tck = -0.12, col.axis = "white", cex.axis = 1.8)
        graphics::box(col = "white")
        
        
      }
      
      if(input$plot_type == "vmea_ann"){
        
        x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
        x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
        
        col_background <- rgb(45, 45, 45, maxColorValue = 255)
        
        my_col <- viridis::viridis(40, direction = 1, option = "D")[8:40]
        my_col <- colorRampPalette(c(col_background, my_col))(200)
        
        my_bre <- seq(min(vmea_ann_band, na.rm = T), max(vmea_ann_band, na.rm = T), length.out = length(my_col)+1)
        
        par(mar = c(3.5, 4.5, 2, 0.0))
        
        layout(matrix(c(1,1,1,1,1,1,1,2),
                      1, 8), widths=c(), heights=c())
        
        image(x = 1:365,
              y = my_elev_bands[-length(my_elev_bands)],
              z = vmea_ann_band, col =my_col, breaks = my_bre,
              ylab = "", xlab = "", axes = F)
        axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
             col = "white", col.axis = "white", tck = -0.04, cex.axis = 1.5)#plot ticks
        axis(1, at = x_axis_lab, c("O","N","D","J","F","M","A","M","J","J","A","S"), tick = FALSE,
             col="white", col.axis="white", mgp=c(3, 0.65, 0), cex.axis = 1.7)#plot labels
        axis(2, mgp=c(3, 0.45, 0), tck = -0.009, cex.axis = 1.8, col = "white", col.axis = "white")
        mtext(expression(paste("Elevation [m]")), side = 2, line = 2.2, cex = 1.4, col = "white")
        mtext(stat_name_max35, line = 0.2, side = 3, cex = 1.5, adj = 0, col = "white")
        mtext("[mm]", line = 0.2, side = 3, cex = 1.2, adj = 1, col = "white")
        graphics::box(col = "white")
        
        
        par(mar = c(3.5, 0.5, 2, 4.0))
        
        image_scale(as.matrix(vmea_ann_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
        axis(4, mgp=c(3, 0.85, 0), tck = -0.12, col.axis = "white", cex.axis = 1.8)
        graphics::box(col = "white")
        
        
      }
      
      if(input$plot_type == "vslo_ann"){
        
        x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
        x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
        
        col_background <- rgb(45, 45, 45, maxColorValue = 255)
        
        cols_min <- colorRampPalette(c("yellow2" ,"gold3", "darkgoldenrod3","darkorange3", "darkorange4", "darkred", col_background))(100)
        cols_max <- colorRampPalette(c(col_background, viridis::viridis(9, direction = 1)[3:4], "lightskyblue2", "lightskyblue1", "grey95"))(100)
        my_col <- c(cols_min, cols_max)
        my_bre <- seq(-max_na(abs(vslo_ann_band)), max_na(abs(vslo_ann_band)), length.out = length(my_col)+1)
        
        par(mar = c(3.5, 4.5, 2, 0.0))
        par(bg = col_background)
        
        layout(matrix(c(1,1,1,1,1,1,1,2),
                      1, 8), widths=c(), heights=c())
        
        image(x = 1:365,
              y = my_elev_bands[-length(my_elev_bands)],
              z = vslo_ann_band, col =my_col, breaks = my_bre,
              ylab = "", xlab = "", axes = F)
        axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
             col = "white", col.axis = "white", tck = -0.04, cex.axis = 1.5)#plot ticks
        axis(1, at = x_axis_lab, c("O","N","D","J","F","M","A","M","J","J","A","S"), tick = FALSE,
             col="white", col.axis="white", mgp=c(3, 0.65, 0), cex.axis = 1.7)#plot labels
        axis(2, mgp=c(3, 0.45, 0), tck = -0.009, cex.axis = 1.8, col = "white", col.axis = "white")
        mtext(expression(paste("Elevation [m]")), side = 2, line = 2.2, cex = 1.4, col = "white")
        mtext(stat_name_max35, line = 0.2, side = 3, cex = 1.5, adj = 0, col = "white")
        mtext("[mm]", line = 0.2, side = 3, cex = 1.2, adj = 1, col = "white")
        graphics::box(col = "white")
        
        
        par(mar = c(3.5, 0.5, 2, 4.0))
        
        image_scale(as.matrix(vslo_ann_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
        axis(4, mgp=c(3, 0.85, 0), tck = -0.12, col.axis = "white", cex.axis = 1.8)
        graphics::box(col = "white")
        
      }
      
      if(input$plot_type == "vdif_ann"){
        
        x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
        x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
        
        col_background <- rgb(45, 45, 45, maxColorValue = 255)
        
        cols_min <- colorRampPalette(c("yellow2" ,"gold3", "darkgoldenrod3","darkorange3", "darkorange4", "darkred", col_background))(100)
        cols_max <- colorRampPalette(c(col_background, viridis::viridis(9, direction = 1)[3:4], "lightskyblue2", "lightskyblue1", "grey95"))(100)
        my_col <- c(cols_min, cols_max)
        my_bre <- seq(-max_na(abs(vdif_ann_band)), max_na(abs(vdif_ann_band)), length.out = length(my_col)+1)
        
        par(mar = c(3.5, 4.5, 2, 0.0))
        par(bg = col_background)
        
        layout(matrix(c(1,1,1,1,1,1,1,2),
                      1, 8), widths=c(), heights=c())
        
        image(x = 1:365,
              y = my_elev_bands[-length(my_elev_bands)],
              z = vdif_ann_band, col =my_col, breaks = my_bre,
              ylab = "", xlab = "", axes = F)
        axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
             col = "white", col.axis = "white", tck = -0.04, cex.axis = 1.5)#plot ticks
        axis(1, at = x_axis_lab, c("O","N","D","J","F","M","A","M","J","J","A","S"), tick = FALSE,
             col="white", col.axis="white", mgp=c(3, 0.65, 0), cex.axis = 1.7)#plot labels
        axis(2, mgp=c(3, 0.45, 0), tck = -0.009, cex.axis = 1.8, col = "white", col.axis = "white")
        mtext(expression(paste("Elevation [m]")), side = 2, line = 2.2, cex = 1.4, col = "white")
        mtext(stat_name_max35, line = 0.2, side = 3, cex = 1.5, adj = 0, col = "white")
        mtext("[mm]", line = 0.2, side = 3, cex = 1.2, adj = 1, col = "white")
        graphics::box(col = "white")
        
        
        par(mar = c(3.5, 0.5, 2, 4.0))
        
        image_scale(as.matrix(vdif_ann_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
        axis(4, mgp=c(3, 0.85, 0), tck = -0.12, col.axis = "white", cex.axis = 1.8)
        graphics::box(col = "white")
        
        
      }
      
      if(input$plot_type == "vdis_ann"){
        
        x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
        x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
        
        col_background <- rgb(45, 45, 45, maxColorValue = 255)
        
        cols_min <- colorRampPalette(c("yellow2" ,"gold3", "darkgoldenrod3","darkorange3", "darkorange4", "darkred", col_background))(100)
        cols_max <- colorRampPalette(c(col_background, viridis::viridis(9, direction = 1)[3:4], "lightskyblue2", "lightskyblue1", "grey95"))(100)
        my_col <- c(cols_min, cols_max)
        my_bre <- seq(-max_na(abs(vdis_ann_band)), max_na(abs(vdis_ann_band)), length.out = length(my_col)+1)
        
        par(mar = c(3.5, 4.5, 2, 0.0))
        par(bg = col_background)
        
        layout(matrix(c(1,1,1,1,1,1,1,2),
                      1, 8), widths=c(), heights=c())
        
        image(x = 1:365,
              y = my_elev_bands[-length(my_elev_bands)],
              z = vdis_ann_band, col =my_col, breaks = my_bre,
              ylab = "", xlab = "", axes = F)
        axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
             col = "white", col.axis = "white", tck = -0.04, cex.axis = 1.5)#plot ticks
        axis(1, at = x_axis_lab, c("O","N","D","J","F","M","A","M","J","J","A","S"), tick = FALSE,
             col="white", col.axis="white", mgp=c(3, 0.65, 0), cex.axis = 1.7)#plot labels
        axis(2, mgp=c(3, 0.45, 0), tck = -0.009, cex.axis = 1.8, col = "white", col.axis = "white")
        mtext(expression(paste("Elevation [m]")), side = 2, line = 2.2, cex = 1.4, col = "white")
        mtext(stat_name_max35, line = 0.2, side = 3, cex = 1.5, adj = 0, col = "white")
        mtext("[mm]", line = 0.2, side = 3, cex = 1.2, adj = 1, col = "white")
        graphics::box(col = "white")
        
        
        par(mar = c(3.5, 0.5, 2, 4.0))
        
        image_scale(as.matrix(vdis_ann_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
        axis(4, mgp=c(3, 0.85, 0), tck = -0.12, col.axis = "white", cex.axis = 1.8)
        graphics::box(col = "white")
        
        
      }
      
      if(input$plot_type == "tmea_ann"){
        
        x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
        x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
        
        col_background <- rgb(45, 45, 45, maxColorValue = 255)
        
        cols_max <- colorRampPalette(c(col_background, "darkred", "darkorange4","darkorange3", "darkgoldenrod3","gold3", "yellow2"))(100)
        cols_min <- colorRampPalette(c("grey95", "lightskyblue1", "lightskyblue2", viridis::viridis(9, direction = 1)[4:3], col_background))(100)
        my_col <- c(cols_min, cols_max)
        my_bre <- c(seq(min_na(tmea_ann_band), 0, length.out = 100), seq(0, max_na(tmea_ann_band), length.out = 100+1))
        
        par(mar = c(3.5, 4.5, 2, 0.0))
        par(bg = col_background)
        
        layout(matrix(c(1,1,1,1,1,1,1,2),
                      1, 8), widths=c(), heights=c())
        
        image(x = 1:365,
              y = my_elev_bands[-length(my_elev_bands)],
              z = tmea_ann_band, col =my_col, breaks = my_bre,
              ylab = "", xlab = "", axes = F)
        axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
             col = "white", col.axis = "white", tck = -0.04, cex.axis = 1.5)#plot ticks
        axis(1, at = x_axis_lab, c("O","N","D","J","F","M","A","M","J","J","A","S"), tick = FALSE,
             col="white", col.axis="white", mgp=c(3, 0.65, 0), cex.axis = 1.7)#plot labels
        axis(2, mgp=c(3, 0.45, 0), tck = -0.009, cex.axis = 1.8, col = "white", col.axis = "white")
        mtext(expression(paste("Elevation [m]")), side = 2, line = 2.2, cex = 1.4, col = "white")
        mtext(stat_name_max35, line = 0.2, side = 3, cex = 1.5, adj = 0, col = "white")
        mtext("[°C]", line = 0.2, side = 3, cex = 1.2, adj = 1, col = "white")
        graphics::box(col = "white")
        
        par(mar = c(3.5, 0.5, 2, 4.0))
        
        image_scale(as.matrix(tmea_ann_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
        axis(4, mgp=c(3, 0.85, 0), tck = -0.12, col.axis = "white", cex.axis = 1.8)
        graphics::box(col = "white")
        
        
      }
      
      if(input$plot_type == "tslo_ann"){
        
        x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
        x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
        
        col_background <- rgb(45, 45, 45, maxColorValue = 255)
        
        cols_max <- colorRampPalette(c(col_background, "darkred", "darkorange4","darkorange3", "darkgoldenrod3","gold3", "yellow2"))(100)
        cols_min <- colorRampPalette(c("grey95", "lightskyblue1", "lightskyblue2", viridis::viridis(9, direction = 1)[4:3], col_background))(100)
        my_col <- c(cols_min, cols_max)
        my_bre <- seq(-max_na(abs(tslo_ann_band)), max_na(abs(tslo_ann_band)), length.out = length(my_col)+1)
        
        par(mar = c(3.5, 4.5, 2, 0.0))
        par(bg = col_background)
        
        layout(matrix(c(1,1,1,1,1,1,1,2),
                      1, 8), widths=c(), heights=c())
        
        image(x = 1:365,
              y = my_elev_bands[-length(my_elev_bands)],
              z = tslo_ann_band, col =my_col, breaks = my_bre,
              ylab = "", xlab = "", axes = F)
        axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
             col = "white", col.axis = "white", tck = -0.04, cex.axis = 1.5)#plot ticks
        axis(1, at = x_axis_lab, c("O","N","D","J","F","M","A","M","J","J","A","S"), tick = FALSE,
             col="white", col.axis="white", mgp=c(3, 0.65, 0), cex.axis = 1.7)#plot labels
        axis(2, mgp=c(3, 0.45, 0), tck = -0.009, cex.axis = 1.8, col = "white", col.axis = "white")
        mtext(expression(paste("Elevation [m]")), side = 2, line = 2.2, cex = 1.4, col = "white")
        mtext(stat_name_max35, line = 0.2, side = 3, cex = 1.5, adj = 0, col = "white")
        mtext("[°C/dec]", line = 0.2, side = 3, cex = 1.2, adj = 1, col = "white")
        graphics::box(col = "white")
        
        par(mar = c(3.5, 0.5, 2, 4.0))
        
        image_scale(as.matrix(tslo_ann_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
        axis(4, mgp=c(3, 0.85, 0), tck = -0.12, col.axis = "white", cex.axis = 1.8)
        graphics::box(col = "white")
        
        
      }
      
      if(input$plot_type == "pmea_ann"){
        
        x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
        x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
        
        col_background <- rgb(45, 45, 45, maxColorValue = 255)
        
        my_col <- viridis::viridis(40, direction = 1, option = "D")[8:40]
        my_col <- colorRampPalette(c(col_background, my_col))(200)
        
        my_bre <- seq(min(pmea_ann_band, na.rm = T), max(pmea_ann_band, na.rm = T), length.out = length(my_col)+1)
        
        par(mar = c(3.5, 4.5, 2, 0.0))
        
        layout(matrix(c(1,1,1,1,1,1,1,2),
                      1, 8), widths=c(), heights=c())
        
        image(x = 1:365,
              y = my_elev_bands[-length(my_elev_bands)],
              z = pmea_ann_band, col =my_col, breaks = my_bre,
              ylab = "", xlab = "", axes = F)
        axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
             col = "white", col.axis = "white", tck = -0.04, cex.axis = 1.5)#plot ticks
        axis(1, at = x_axis_lab, c("O","N","D","J","F","M","A","M","J","J","A","S"), tick = FALSE,
             col="white", col.axis="white", mgp=c(3, 0.65, 0), cex.axis = 1.7)#plot labels
        axis(2, mgp=c(3, 0.45, 0), tck = -0.009, cex.axis = 1.8, col = "white", col.axis = "white")
        mtext(expression(paste("Elevation [m]")), side = 2, line = 2.2, cex = 1.4, col = "white")
        mtext(stat_name_max35, line = 0.2, side = 3, cex = 1.5, adj = 0, col = "white")
        mtext("[mm]", line = 0.2, side = 3, cex = 1.2, adj = 1, col = "white")
        graphics::box(col = "white")
        
        
        par(mar = c(3.5, 0.5, 2, 4.0))
        
        image_scale(as.matrix(pmea_ann_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
        axis(4, mgp=c(3, 0.85, 0), tck = -0.12, col.axis = "white", cex.axis = 1.8)
        graphics::box(col = "white")
        
        
      }
      if(input$plot_type == "pslo_ann"){
        
        x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
        x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
        
        col_background <- rgb(45, 45, 45, maxColorValue = 255)
        
        cols_min <- colorRampPalette(c("yellow2", "gold3", "darkgoldenrod3", "darkorange3", "darkorange4", "darkred", col_background))(100)
        cols_max <- colorRampPalette(c(col_background, viridis::viridis(9, direction = 1)[3:4], "lightskyblue2", "lightskyblue1", "grey95"))(100)
        my_col <- c(cols_min, cols_max)
        my_bre <- seq(-max_na(abs(pslo_ann_band)), max_na(abs(pslo_ann_band)), length.out = length(my_col)+1)
        
        par(mar = c(3.5, 4.5, 2, 0.0))
        par(bg = col_background)
        
        layout(matrix(c(1,1,1,1,1,1,1,2),
                      1, 8), widths=c(), heights=c())
        
        image(x = 1:365,
              y = my_elev_bands[-length(my_elev_bands)],
              z = pslo_ann_band, col =my_col, breaks = my_bre,
              ylab = "", xlab = "", axes = F)
        axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
             col = "white", col.axis = "white", tck = -0.04, cex.axis = 1.5)#plot ticks
        axis(1, at = x_axis_lab, c("O","N","D","J","F","M","A","M","J","J","A","S"), tick = FALSE,
             col="white", col.axis="white", mgp=c(3, 0.65, 0), cex.axis = 1.7)#plot labels
        axis(2, mgp=c(3, 0.45, 0), tck = -0.009, cex.axis = 1.8, col = "white", col.axis = "white")
        mtext(expression(paste("Elevation [m]")), side = 2, line = 2.2, cex = 1.4, col = "white")
        mtext(stat_name_max35, line = 0.2, side = 3, cex = 1.5, adj = 0, col = "white")
        mtext("[mm/dec]", line = 0.2, side = 3, cex = 1.2, adj = 1, col = "white")
        graphics::box(col = "white")
        
        par(mar = c(3.5, 0.5, 2, 4.0))
        
        image_scale(as.matrix(pslo_ann_band), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
        axis(4, mgp=c(3, 0.85, 0), tck = -0.12, col.axis = "white", cex.axis = 1.8)
        graphics::box(col = "white")
        
      }
      
    }
    
    output$elev_plot <- renderPlot({f_plot()})
    
    output$down <- downloadHandler(
      
      filename = function(){
        
        paste0(sta_id, "_", input$plot_type, ".png")
        
      },
      content = function(file){
        
        png(file, width = 800, height = 450) #open device
        
        f_plot()#creat plot
        
        dev.off() #close device
        
      }
      
    )
    
    })
  
  # output$plotly_ele_are <- renderPlotly({
  #   
  #   y_sel <- usgs_basin_physi$Size.km2.
  #   x_sel <- usgs_basin_physi$Elevation.m.
  #   
  #   phy_df <- data.frame(x_sel = as.numeric(x_sel),
  #                        y_sel = as.numeric(y_sel),
  #                        hu = as.character(usgs_basin_physi$BASIN_HUC),
  #                        id = usgs_basin_physi$BASIN_ID,
  #                        stringsAsFactors = F)
  #   
  #   plotly_ele_are <- plot_ly(phy_df,
  #                             y = ~y_sel,
  #                             x = ~x_sel,
  #                             type = 'scatter',
  #                             mode = "markers",
  #                             hovertemplate = paste(
  #                               "HU:", phy_df$hu, '<br>',
  #                               "ID", phy_df$id, '<br>',
  #                               "y-axis: %{y:.2f}",'<br>',
  #                               "x-axis: %{x:.0f}",
  #                               "<extra></extra>"
  #                             ),
  #                             color = ~hu,
  #                             colors = hu_cols,
  #                             opacity = 0.7,
  #                             marker = list(size = 15),
  #                             # symbol = ~forcing,
  #                             # symbols = c("circle", rep(c('circle', 'square', 'x-dot', 'triangle-up'), 5)),
  #                             height = '580'
  #   )
  #   
  #   title_font <- list(
  #     size = 18,
  #     color = "white"
  #   )
  #   
  #   tick_font <- list(
  #     size = 14,
  #     color = "white",
  #     ticks = "inside"
  #   )
  #   
  #   y_axis <- list(
  #     title = "Basin area [km²]",
  #     showticklabels = TRUE,
  #     exponentformat = "none",
  #     tickangle = 270,
  #     titlefont = title_font,
  #     tickfont = tick_font,
  #     showline = F,
  #     zeroline = F,
  #     showgrid = T
  #   )
  #   
  #   x_axis <- list(
  #     title = "Basin elevation [m]",
  #     showticklabels = TRUE,
  #     exponentformat = "none",
  #     titlefont = title_font,
  #     tickfont = tick_font,
  #     showline = F,
  #     zeroline = F,
  #     showgrid = T
  #   )
  #   
  #   legend_font <- list(
  #     font = list(
  #       size = 14,
  #       color = "white"))
  #   
  #   vline <- function(x = 0, color = "white") {
  #     list(
  #       type = "line",
  #       y0 = 0,
  #       y1 = 1,
  #       yref = "paper",
  #       x0 = x,
  #       x1 = x,
  #       line = list(color = color)
  #     )
  #   }
  #   
  #   plotly_ele_are <- plotly_ele_are %>%
  #     layout(title = '',
  #            yaxis = y_axis,
  #            xaxis = x_axis,
  #            legend = legend_font,
  #            plot_bgcolor='transparent',
  #            paper_bgcolor='transparent',
  #            margin = list(
  #              r = 350,
  #              t = 10,
  #              b = 10,
  #              l = 200)
  #     )
  #   
  #   plotly_ele_are
  #   
  # })
  # 
  # output$plotly_ele_met <- renderPlotly({
  #   
  #   #Define data depending on gauge selection
  #   if(input$var_plotly_ele_met == "Tmax - mean"){
  #     y_sel <- res_out$tma_mea
  #     ylab <- "Temperature max [°C]"
  #   }
  #   if(input$var_plotly_ele_met == "Tmin - mean"){
  #     y_sel <- res_out$tmi_mea
  #     ylab <- "Temperature min [°C]"
  #   }
  #   if(input$var_plotly_ele_met == "Tmax - trend"){
  #     y_sel <- res_out$tma_slo
  #     ylab <- "Temperature max [°C/decade]"
  #   }
  #   if(input$var_plotly_ele_met == "Tmin - trend"){
  #     y_sel <- res_out$tmi_slo
  #     ylab <- "Temperature min [°C/decade]"
  #   }
  #   
  #   x_sel <- res_out$base_elev
  #   
  #   swe_df <- data.frame(x_sel = as.numeric(x_sel),
  #                        y_sel = as.numeric(y_sel),
  #                        hu = as.character(res_out$hu),
  #                        id = res_out$id,
  #                        el = res_out$base_elev,
  #                        stringsAsFactors = F)
  #   
  #   plotly_ele_met <- plot_ly(swe_df,
  #                             y = ~y_sel,
  #                             x = ~x_sel,
  #                             type = 'scatter',
  #                             mode = "markers",
  #                             hovertemplate = paste(
  #                              "HU:", res_out$hu, '<br>',
  #                              "ID", res_out$id, '<br>',
  #                              "Elevaion:", res_out$base_elev, '<br>',
  #                              "y-axis: %{y:.2f}",'<br>',
  #                              "x-axis: %{x:.0f}",
  #                              "<extra></extra>"
  #                              ),
  #                             color = ~hu,
  #                             colors = hu_cols,
  #                             opacity = 0.7,
  #                             marker = list(size = 15),
  #                             # symbol = ~forcing,
  #                             # symbols = c("circle", rep(c('circle', 'square', 'x-dot', 'triangle-up'), 5)),
  #                             height = '580'
  #   )
  #   
  #   title_font <- list(
  #     size = 18,
  #     color = "white"
  #   )
  #   
  #   tick_font <- list(
  #     size = 14,
  #     color = "white",
  #     ticks = "inside"
  #   )
  #   
  #   y_axis <- list(
  #     title = ylab,
  #     showticklabels = TRUE,
  #     exponentformat = "none",
  #     tickangle = 270,
  #     titlefont = title_font,
  #     tickfont = tick_font,
  #     showline = F,
  #     zeroline = F,
  #     showgrid = T
  #   )
  #   
  #   x_axis <- list(
  #     title = "Base elevation [m]",
  #     showticklabels = TRUE,
  #     exponentformat = "none",
  #     titlefont = title_font,
  #     tickfont = tick_font,
  #     showline = F,
  #     zeroline = F,
  #     showgrid = T
  #   )
  #   
  #   legend_font <- list(
  #     font = list(
  #       size = 14,
  #       color = "white"))
  #   
  #   vline <- function(x = 0, color = "white") {
  #     list(
  #       type = "line",
  #       y0 = 0,
  #       y1 = 1,
  #       yref = "paper",
  #       x0 = x,
  #       x1 = x,
  #       line = list(color = color)
  #     )
  #   }
  #   
  #   plotly_ele_met <- plotly_ele_met %>%
  #     layout(title = '',
  #            yaxis = y_axis,
  #            xaxis = x_axis,
  #            legend = legend_font,
  #            plot_bgcolor='transparent',
  #            paper_bgcolor='transparent',
  #            margin = list(
  #              r = 350,
  #              t = 10,
  #              b = 10,
  #              l = 200)
  #     )
  #   
  #   plotly_ele_met
  #   
  # })
  # 
  # # Reaction to selection on plotly graph
  # observeEvent(event_data(event = "plotly_click"),{
  #   d <- event_data(event = "plotly_click")
  #   gauge_sel_plotly <- d
  #   
  #   xaxi_plotly_sel <- gauge_sel_plotly$x
  #   yaxi_plotly_sel <- gauge_sel_plotly$y
  #   
  #   print(yaxi_plotly_sel)
  #   
  #   #Selection on plotly_ele_met
  #   if(round(yaxi_plotly_sel, 5) %in% round(res_out$tma_mea, 5) | 
  #      round(yaxi_plotly_sel, 5) %in% round(res_out$tmi_mea, 5) |
  #      round(yaxi_plotly_sel, 5) %in% round(res_out$tma_slo, 5) |
  #      round(yaxi_plotly_sel, 5) %in% round(res_out$tmi_slo, 5)
  #      ){
  #     
  #     if(input$var_plotly_ele_met == "Tmax - mean"){
  #       res_sel_plotly <- res_out$tma_mea
  #     }
  #     
  #     if(input$var_plotly_ele_met == "Tmax - trend"){
  #       res_sel_plotly <- res_out$tma_slo
  #     }
  #     
  #     if(input$var_plotly_ele_met == "Tmin - mean"){
  #       res_sel_plotly <- res_out$tmi_mea
  #     }
  #     
  #     if(input$var_plotly_ele_met == "Tmin - trend"){
  #       res_sel_plotly <- res_out$tmi_slo
  #     }
  #     
  #     plotly_ind_sel <- which(round(res_sel_plotly, 5) == round(yaxi_plotly_sel, 5))
  #     
  #     plotly_hu_sel <- res_out$hu[plotly_ind_sel]
  #     plotly_id_sel <- res_out$id[plotly_ind_sel]
  #     
  #     #remove zero at beginning of id (if present)
  #     if(substr(plotly_id_sel, 1, 1) == "0"){
  #       plotly_id_sel <- substr(plotly_id_sel, 2, 8) 
  #     }
  #     meta_gauge_sel_ind <- which(usgs_meta$id == plotly_id_sel)
  #     
  #     print(plotly_id_sel)
  #     print(meta_gauge_sel_ind)
  #     
  #     df <-data.frame(x = usgs_meta$longitude[meta_gauge_sel_ind], y = usgs_meta$latitude[meta_gauge_sel_ind])
  #     
  #   }
  #   
  #   #Selection on plotly_ele_are
  #   if(round(yaxi_plotly_sel, 5) %in% round(usgs_basin_physi$Size.km2., 5)){
  #     
  #     plotly_ind_sel <- which(round(usgs_basin_physi$Size.km2., 5) == round(yaxi_plotly_sel, 5))
  #     
  #     plotly_hu_sel <- usgs_basin_physi$BASIN_HUC[plotly_ind_sel]
  #     plotly_id_sel <- usgs_basin_physi$BASIN_ID[plotly_ind_sel]
  #     
  #     #remove zero at beginning of id (if present)
  #     if(substr(plotly_id_sel, 1, 1) == "0"){
  #       plotly_id_sel <- substr(plotly_id_sel, 2, 8) 
  #     }
  #     meta_gauge_sel_ind <- which(usgs_meta$id == plotly_id_sel)
  #     
  #     print(plotly_id_sel)
  #     print(meta_gauge_sel_ind)
  #     
  #     df <-data.frame(x = usgs_meta$longitude[meta_gauge_sel_ind], y = usgs_meta$latitude[meta_gauge_sel_ind])
  #     
  #   }
  #   
  #   gauge_plotly <-SpatialPoints(coords = df)
  #   label_content <- "Plotly Click Selection"
  #   
  #   #Update leaflat map and show watershed selected
  #   leafletProxy("map") %>%
  #     removeMarker(layerId = "plotly_gauge") %>%
  #     addCircleMarkers(data = gauge_plotly, layerId = "plotly_gauge", group = "Plotly Click", 
  #                      label = label_content, labelOptions = labelOptions(noHide = TRUE),
  #                      fillOpacity = 0.0, fillColor = "white", color = "black", radius = 12) %>%
  #     # addPopups(data = gauge_plotly, layerId = "plotly_gauge", group = "Plotly Click", 
  #     #           popup = popup_content) %>%
  #     addLayersControl(
  #       baseGroups = c("Stamen.TerrainBackground", "Stamen.TonerBackground", "OpenStreetMap"),
  #       overlayGroups = c("Outlets", "Watershed", "Plotly Click"),
  #       position = "bottomleft",
  #       options = layersControlOptions(collapsed = F)
  #     ) %>%
  #     fitBounds(lng1 = -112, lng2 = -55, lat1 = 23, lat2 = 50)
  #   
  # })
  
  
})
