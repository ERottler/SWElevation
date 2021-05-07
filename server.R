###

#Shiny dashboard to investigate elevation-dependent compensation effects in snowmelt
#Server
#Erwin Rottler, University of Potsdam

###

app_dir <- "/home/erwin/Nextcloud/pdoc_up/SWElevation/R/SWElevation/"

#read data for app
load(paste0(app_dir, "swe_data.RData"))
load(paste0(app_dir, "elev_res.RData"))

#Initial dummy catchment
catch_sel <- sp::Polygon(matrix(rnorm(10, 0, 0.01), ncol = 2))

shinyServer(function(input, output) {
  
  #colors for hydrological response units (HUs)
  hu_cols <- c("navyblue", "royalblue3", "steelblue2", "slategray2", "cadetblue3", 
               "turquoise4","yellow2", "gold3", "orange2", "sienna2", "tomato3", "plum2", 
               "orchid4", "violetred2", "red2", "darkred", "limegreen", "seagreen2")
  hu_cols_num <- colorNumeric(hu_cols, 1:18) 
  
  #Leaflet map with all stations
  output$map <- renderLeaflet({
    
    m = leaflet() %>%
      addProviderTiles(providers$Stamen.TonerBackground, group = "Stamen.TonerBackground") %>%
      addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen.TerrainBackground") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,        group = "OpenStreetMap") %>%
      addPolygons(data = catch_sel, layerId = "watershed", group = "Watershed") %>%
      addLayersControl(
        baseGroups = c("Stamen.TonerBackground", "Stamen.TerrainBackground", "OpenStreetMap"),
        overlayGroups = c("Gauges", "Watershed"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = F)
      ) %>%
      fitBounds(lng1 = -112, lng2 = -55, lat1 = 23, lat2 = 50)
    
    for(i in 1:18){
      
      ind_sel <- which(usgs_meta$hu == i)
      
      m <- m %>%
        addCircleMarkers(usgs_meta$longitude[ind_sel],
                         usgs_meta$latitude[ind_sel],
                         label = paste0("HU ", usgs_meta$hu[ind_sel], ":",
                                       usgs_meta$name[ind_sel]),
                         labelOptions = labelOptions(noHide = F, textOnly = F, direction = "top"),
                         stroke = F, group = "Gauges", fillOpacity = 0.9, fillColor = hu_cols_num(i),
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

  #Dummy which gets selected gauge
  gauge_sel <-  shiny::reactiveValues(clicked_gauge = "XXX")
  
  #Reaction to selection of station on map
  observeEvent(input$map_marker_click,{
    
    gauge_sel$clicked_gauge <- input$map_marker_click
    
    stat_sel <- which(usgs_meta$latitude == gauge_sel$clicked_gauge$lat)
    
    stat_name <- usgs_meta$name[stat_sel] #station name
    sta_id <- usgs_meta$id[stat_sel] #station id
    
    #select watershed boundaries for selected gauge
    sel_ind <- which(catch_usgs@data$hru_id == sta_id)
    
    crswgs84 <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    catch_sel_raw <- catch_usgs[sel_ind, ]
    catch_sel <- spTransform(catch_sel_raw, crswgs84)
    
    #Update leaflat map and show watershed selected
    leafletProxy("map") %>%
      removeShape(layerId = "watershed") %>%
      addPolygons(data = catch_sel, layerId = "watershed", fill = F,
                  color = "#366488", opacity = 0.9, group = "Watershed") %>%
      addLayersControl(
        baseGroups = c("Stamen.TonerBackground", "Stamen.TerrainBackground", "OpenStreetMap"),
        overlayGroups = c("River gauges", "Watershed", "Test"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = F)
      )
    
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
    if(input$var_plotly_ele_met == "Tmax - trend"){
      y_sel <- res_out$tma_slo
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

  #Reaction to selection on plotly graph
  observe({
    d <- event_data(event = "plotly_click")
    gauge_sel_plotly <- d
    print(gauge_sel_plotly)
  })
  
})
