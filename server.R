###

#Shiny dashboard to investigate elevation-dependent compensation effects in snowmelt
#Server
#Erwin Rottler, University of Potsdam

###

app_dir <- "/home/erwin/Nextcloud/pdoc_up/SWElevation/R/SWElevation/"
dat_dir <- "/home/erwin/Documents/storage_research/camels_us/basin_timeseries_v1p2_metForcing_obsFlow/basin_dataset_public_v1p2/"

#read data for app
load(paste0(app_dir, "swe_data.RData"))
load(paste0(app_dir, "elev_res.RData"))

#Initial dummy catchment
catch_sel <- sp::Polygon(matrix(rnorm(10, 0, 0.01), ncol = 2))

#Initial dummy gauge for plotly selection
df <-data.frame(x = 0, y = 0)
gauge_plotly <-SpatialPoints(coords = df)

shinyServer(function(input, output) {
  
  #colors for hydrological response units (HUs)
  hu_cols <- c("navyblue", "royalblue3", "steelblue2", "slategray2", "cadetblue3", 
               "turquoise4","yellow2", "gold3", "orange2", "sienna2", "tomato3", "plum2", 
               "orchid4", "violetred2", "red2", "darkred", "limegreen", "seagreen2")
  hu_cols_num <- colorNumeric(hu_cols, 1:18) 
  
  #Leaflet map with all stations
  output$map <- renderLeaflet({
    
    m = leaflet() %>%
      addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen.TerrainBackground") %>%
      addProviderTiles(providers$Stamen.TonerBackground, group = "Stamen.TonerBackground") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,        group = "OpenStreetMap") %>%
      addPolygons(data = catch_sel, layerId = "watershed", group = "Watershed") %>%
      addCircleMarkers(data = gauge_plotly, layerId = "plotly_gauge", group = "Plotly Click") %>%
      addLayersControl(
        baseGroups = c("Stamen.TerrainBackground", "Stamen.TonerBackground", "OpenStreetMap"),
        overlayGroups = c("Outlets", "Watershed"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = F)
      ) %>%
      fitBounds(lng1 = -112, lng2 = -55, lat1 = 23, lat2 = 50) 
      # hideGroup("Plotly Click")
    
    for(i in 1:18){
      
      ind_sel <- which(usgs_meta$hu == i)
      
      m <- m %>%
        addCircleMarkers(usgs_meta$longitude[ind_sel],
                         usgs_meta$latitude[ind_sel],
                         label = paste0(usgs_meta$name[ind_sel]),
                         labelOptions = labelOptions(noHide = F, textOnly = F, direction = "right"),
                         stroke = F, group = "Outlets", fillOpacity = 0.9, fillColor = hu_cols_num(i),
                         popup = paste(
                           usgs_meta$name[ind_sel], '<br>',
                           "HU:", usgs_meta$hu[ind_sel], '<br>',
                           "ID:", usgs_meta$id[ind_sel], '<br>',
                           "<extra></extra>"
                         )
        )
      
    }
    
    #retrun map
    m
    
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
    
    stat_sel <- which(usgs_meta$latitude == gauge_sel$clicked_gauge$lat)
    
    stat_name <- trimws(usgs_meta$name[stat_sel], which = "both") #station name
    if(nchar(stat_name) > 35){
      stat_name_max35 <- paste0(substr(stat_name, 1, 30), " [...]")
    }else{
      stat_name_max35 <- stat_name
    }
    
    sta_id <- usgs_meta$id[stat_sel] #station id
    sta_hu <- usgs_meta$hu[stat_sel] # station hu
    
    #zero-buffer HU (if necessary)
    if(nchar(as.character(sta_hu)) < 2){
      sta_hu_zero <- paste0("0", sta_hu)
    }else{
      sta_hu_zero <- sta_hu
    }
    
    #zero-buffer ID (if necessary)
    if(nchar(as.character(sta_id)) < 8){
      sta_id_zero <- paste0("0", sta_id)
    }else{
      sta_id_zero <- sta_id
    }
    
    band_meta_path <- paste0(dat_dir, "elev_bands_forcing/daymet/", sta_hu_zero, "/", sta_id_zero, ".list")

    bands_meta <- read.table(band_meta_path)
    
    #select watershed boundaries for selected gauge
    sel_ind <- which(catch_usgs@data$hru_id == sta_id)
    
    crswgs84 <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    catch_sel_raw <- catch_usgs[sel_ind, ]
    catch_sel <- spTransform(catch_sel_raw, crswgs84)
    
    #Update leaflat map and show watershed selected
    leafletProxy("map") %>%
      removeMarker(layerId = "plotly_gauge") %>%
      removeShape(layerId = "watershed") %>%
      addPolygons(data = catch_sel, layerId = "watershed", fill = F,
                  color = "#366488", opacity = 0.9, group = "Watershed") %>%
      addLayersControl(
        baseGroups = c("Stamen.TerrainBackground", "Stamen.TonerBackground", "OpenStreetMap"),
        overlayGroups = c("Outlets", "Watershed"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = F)
      )
    
    f_plot <- function(){
      
      par(bg="transparent")
      
      if(input$plot_type == "elevdist"){
        
        #get elevations and area
        bands_elev <- as.numeric(substr(rownames(bands_meta), 20, 22))*100
        bands_area <- bands_meta[, 1]
        area_total <- sum(bands_area, na.rm = T)
        area_total_km2 <- area_total / 1000000
        bands_frac <- (bands_area / area_total) * 100
        
        par(mar = c(3.5, 3.5, 2, 0.5))
        
        plot(bands_frac, bands_elev, type = "n", xlim = c(0, max(bands_frac, na.rm = T)+5), ylim = c(0, 4400),
             axes = F, ylab = "", xlab = "", xaxs = "i", col.axis = "white")
        abline(h = seq(0, 4000, 500), lty = "dashed", col = "grey55")
        abline(v = seq(0, 100, 10), lty = "dashed", col = "grey55")
        
        for(i in 1:length(bands_frac)){
          
          segments(x0 = 0, y0 = bands_elev[i],
                   x1 =  bands_frac[i], y1 = bands_elev[i], col = "steelblue3", lwd = 8, lend = 2)
          
        }
        axis(1, mgp=c(3, 0.25, 0), tck = -0.009, cex.axis = 1.2, col = "white", col.axis = "white")
        axis(2, mgp=c(3, 0.25, 0), tck = -0.009, cex.axis = 1.2, col = "white", col.axis = "white")
        mtext(expression(paste("Elevation [m]")), side = 2, line = 2.0, cex = 1.4, col = "white")
        mtext(expression(paste("Areal fraction [%]")), side = 1, line = 2.0, cex = 1.4, col = "white")
        mtext(stat_name_max35, line = 0.2, side = 3, cex = 1.5, adj = 0, col = "white")
        mtext(paste0("Total area: ", round(area_total_km2, 0), " km²"), line = 0.2, side = 3, 
              cex = 1.1, adj = 1.0, col = "white")
        graphics::box(col = "white")
        
        
      }
      
    }
    
    output$elev_plot <- renderPlot({f_plot()})
    
    output$down <- downloadHandler(
      
      filename = function(){
        
        paste0(sta_hu, "_",  sta_id, "_", input$plot_type, ".png")
        
      },
      content = function(file){
        
        png(file, width = 800, height = 450) #open device
        
        f_plot()#creat plot
        
        dev.off() #close device
        
      }
      
    )
    
    })
  
  output$plotly_ele_are <- renderPlotly({
    
    y_sel <- usgs_basin_physi$Size.km2.
    x_sel <- usgs_basin_physi$Elevation.m.
    
    phy_df <- data.frame(x_sel = as.numeric(x_sel),
                         y_sel = as.numeric(y_sel),
                         hu = as.character(usgs_basin_physi$BASIN_HUC),
                         id = usgs_basin_physi$BASIN_ID,
                         stringsAsFactors = F)
    
    plotly_ele_are <- plot_ly(phy_df,
                              y = ~y_sel,
                              x = ~x_sel,
                              type = 'scatter',
                              mode = "markers",
                              hovertemplate = paste(
                                "HU:", phy_df$hu, '<br>',
                                "ID", phy_df$id, '<br>',
                                "y-axis: %{y:.2f}",'<br>',
                                "x-axis: %{x:.0f}",
                                "<extra></extra>"
                              ),
                              color = ~hu,
                              colors = hu_cols,
                              opacity = 0.7,
                              marker = list(size = 15),
                              # symbol = ~forcing,
                              # symbols = c("circle", rep(c('circle', 'square', 'x-dot', 'triangle-up'), 5)),
                              height = '580'
    )
    
    title_font <- list(
      size = 18,
      color = "white"
    )
    
    tick_font <- list(
      size = 14,
      color = "white",
      ticks = "inside"
    )
    
    y_axis <- list(
      title = "Basin area [km²]",
      showticklabels = TRUE,
      exponentformat = "none",
      tickangle = 270,
      titlefont = title_font,
      tickfont = tick_font,
      showline = F,
      zeroline = F,
      showgrid = T
    )
    
    x_axis <- list(
      title = "Basin elevation [m]",
      showticklabels = TRUE,
      exponentformat = "none",
      titlefont = title_font,
      tickfont = tick_font,
      showline = F,
      zeroline = F,
      showgrid = T
    )
    
    legend_font <- list(
      font = list(
        size = 14,
        color = "white"))
    
    vline <- function(x = 0, color = "white") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color)
      )
    }
    
    plotly_ele_are <- plotly_ele_are %>%
      layout(title = '',
             yaxis = y_axis,
             xaxis = x_axis,
             legend = legend_font,
             plot_bgcolor='transparent',
             paper_bgcolor='transparent',
             margin = list(
               r = 350,
               t = 10,
               b = 10,
               l = 200)
      )
    
    plotly_ele_are
    
  })
  
  output$plotly_ele_met <- renderPlotly({
    
    #Define data depending on gauge selection
    if(input$var_plotly_ele_met == "Tmax - mean"){
      y_sel <- res_out$tma_mea
      ylab <- "Temperature max [°C]"
    }
    if(input$var_plotly_ele_met == "Tmin - mean"){
      y_sel <- res_out$tmi_mea
      ylab <- "Temperature min [°C]"
    }
    if(input$var_plotly_ele_met == "Tmax - trend"){
      y_sel <- res_out$tma_slo
      ylab <- "Temperature max [°C/decade]"
    }
    if(input$var_plotly_ele_met == "Tmin - trend"){
      y_sel <- res_out$tmi_slo
      ylab <- "Temperature min [°C/decade]"
    }
    
    x_sel <- res_out$base_elev
    
    swe_df <- data.frame(x_sel = as.numeric(x_sel),
                         y_sel = as.numeric(y_sel),
                         hu = as.character(res_out$hu),
                         id = res_out$id,
                         el = res_out$base_elev,
                         stringsAsFactors = F)
    
    plotly_ele_met <- plot_ly(swe_df,
                              y = ~y_sel,
                              x = ~x_sel,
                              type = 'scatter',
                              mode = "markers",
                              hovertemplate = paste(
                               "HU:", res_out$hu, '<br>',
                               "ID", res_out$id, '<br>',
                               "Elevaion:", res_out$base_elev, '<br>',
                               "y-axis: %{y:.2f}",'<br>',
                               "x-axis: %{x:.0f}",
                               "<extra></extra>"
                               ),
                              color = ~hu,
                              colors = hu_cols,
                              opacity = 0.7,
                              marker = list(size = 15),
                              # symbol = ~forcing,
                              # symbols = c("circle", rep(c('circle', 'square', 'x-dot', 'triangle-up'), 5)),
                              height = '580'
    )
    
    title_font <- list(
      size = 18,
      color = "white"
    )
    
    tick_font <- list(
      size = 14,
      color = "white",
      ticks = "inside"
    )
    
    y_axis <- list(
      title = ylab,
      showticklabels = TRUE,
      exponentformat = "none",
      tickangle = 270,
      titlefont = title_font,
      tickfont = tick_font,
      showline = F,
      zeroline = F,
      showgrid = T
    )
    
    x_axis <- list(
      title = "Base elevation [m]",
      showticklabels = TRUE,
      exponentformat = "none",
      titlefont = title_font,
      tickfont = tick_font,
      showline = F,
      zeroline = F,
      showgrid = T
    )
    
    legend_font <- list(
      font = list(
        size = 14,
        color = "white"))
    
    vline <- function(x = 0, color = "white") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color)
      )
    }
    
    plotly_ele_met <- plotly_ele_met %>%
      layout(title = '',
             yaxis = y_axis,
             xaxis = x_axis,
             legend = legend_font,
             plot_bgcolor='transparent',
             paper_bgcolor='transparent',
             margin = list(
               r = 350,
               t = 10,
               b = 10,
               l = 200)
      )
    
    plotly_ele_met
    
  })

  # Reaction to selection on plotly graph
  observeEvent(event_data(event = "plotly_click"),{
    d <- event_data(event = "plotly_click")
    gauge_sel_plotly <- d
    
    xaxi_plotly_sel <- gauge_sel_plotly$x
    yaxi_plotly_sel <- gauge_sel_plotly$y
    
    print(yaxi_plotly_sel)
    
    #Selection on plotly_ele_met
    if(round(yaxi_plotly_sel, 5) %in% round(res_out$tma_mea, 5) | 
       round(yaxi_plotly_sel, 5) %in% round(res_out$tmi_mea, 5) |
       round(yaxi_plotly_sel, 5) %in% round(res_out$tma_slo, 5) |
       round(yaxi_plotly_sel, 5) %in% round(res_out$tmi_slo, 5)
       ){
      
      if(input$var_plotly_ele_met == "Tmax - mean"){
        res_sel_plotly <- res_out$tma_mea
      }
      
      if(input$var_plotly_ele_met == "Tmax - trend"){
        res_sel_plotly <- res_out$tma_slo
      }
      
      if(input$var_plotly_ele_met == "Tmin - mean"){
        res_sel_plotly <- res_out$tmi_mea
      }
      
      if(input$var_plotly_ele_met == "Tmin - trend"){
        res_sel_plotly <- res_out$tmi_slo
      }
      
      plotly_ind_sel <- which(round(res_sel_plotly, 5) == round(yaxi_plotly_sel, 5))
      
      plotly_hu_sel <- res_out$hu[plotly_ind_sel]
      plotly_id_sel <- res_out$id[plotly_ind_sel]
      
      #remove zero at beginning of id (if present)
      if(substr(plotly_id_sel, 1, 1) == "0"){
        plotly_id_sel <- substr(plotly_id_sel, 2, 8) 
      }
      meta_gauge_sel_ind <- which(usgs_meta$id == plotly_id_sel)
      
      print(plotly_id_sel)
      print(meta_gauge_sel_ind)
      
      df <-data.frame(x = usgs_meta$longitude[meta_gauge_sel_ind], y = usgs_meta$latitude[meta_gauge_sel_ind])
      
    }
    
    #Selection on plotly_ele_are
    if(round(yaxi_plotly_sel, 5) %in% round(usgs_basin_physi$Size.km2., 5)){
      
      plotly_ind_sel <- which(round(usgs_basin_physi$Size.km2., 5) == round(yaxi_plotly_sel, 5))
      
      plotly_hu_sel <- usgs_basin_physi$BASIN_HUC[plotly_ind_sel]
      plotly_id_sel <- usgs_basin_physi$BASIN_ID[plotly_ind_sel]
      
      #remove zero at beginning of id (if present)
      if(substr(plotly_id_sel, 1, 1) == "0"){
        plotly_id_sel <- substr(plotly_id_sel, 2, 8) 
      }
      meta_gauge_sel_ind <- which(usgs_meta$id == plotly_id_sel)
      
      print(plotly_id_sel)
      print(meta_gauge_sel_ind)
      
      df <-data.frame(x = usgs_meta$longitude[meta_gauge_sel_ind], y = usgs_meta$latitude[meta_gauge_sel_ind])
      
    }
    
    gauge_plotly <-SpatialPoints(coords = df)
    label_content <- "Plotly Click Selection"
    
    #Update leaflat map and show watershed selected
    leafletProxy("map") %>%
      removeMarker(layerId = "plotly_gauge") %>%
      addCircleMarkers(data = gauge_plotly, layerId = "plotly_gauge", group = "Plotly Click", 
                       label = label_content, labelOptions = labelOptions(noHide = TRUE),
                       fillOpacity = 0.0, fillColor = "white", color = "black", radius = 12) %>%
      # addPopups(data = gauge_plotly, layerId = "plotly_gauge", group = "Plotly Click", 
      #           popup = popup_content) %>%
      addLayersControl(
        baseGroups = c("Stamen.TerrainBackground", "Stamen.TonerBackground", "OpenStreetMap"),
        overlayGroups = c("Outlets", "Watershed", "Plotly Click"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = F)
      ) %>%
      fitBounds(lng1 = -112, lng2 = -55, lat1 = 23, lat2 = 50)
    
  })
  
  
})
