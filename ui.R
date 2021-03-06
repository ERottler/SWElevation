###

#Shiny dashboard to investigate CAMELS-US
#Server
#Erwin Rottler, University of Potsdam

###

library(shiny)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(leaflet)
library(rgdal)
library(grDevices)
library(shinyWidgets)
library(viridis)
library(meltimr)
library(scales)


navbarPage("Watershed Investigator", id="nav", theme = shinytheme("slate"), selected = "Interactive Map", position = "fixed-top",
           
           tags$style(type="text/css", "body {padding-top: 70px;}"),
           
           tabPanel("Summary & Overview",
                    
                    setBackgroundColor("grey22"),
                    
                    HTML("<br>"),
                    
                    p(style="text-align: justify; font-size: 16px; width: 99%",
                      "This",
                      tags$a(href="https://rstudio.github.io/shinydashboard/", "Shiny dashboard",
                             style="color:#6699CC;", target="_blank"),
                      "enables the"),
                    
                    HTML("<br>"), 
                    
                    p(style="text-align: justify; font-size: 40px; width: 99%;",
                      "Analysis of elevation-dependent compensation effects in snowmelt"),
                    
                    HTML("<br>"), 
                    
                    h2("Summary"),
                    
                    # plotlyOutput("plotly_ele_are", width="100%"),
                    
                    HTML("<br><br><br><br><br>"),
                    
                    selectInput("var_plotly_ele_met", "",
                                choices = c("Tmax - mean",
                                            "Tmin - mean",
                                            "Tmax - trend",
                                            "Tmin - trend")),
                    
                    # plotlyOutput("plotly_ele_met", width="100%"),
                    
                    HTML("<br><br>")
                    
                    
                    
           ),
           
           tabPanel("Interactive Map",
                    div(class="outer",
                        
                        tags$head(
                          includeCSS("styles.css")
                        ),
                        
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                        
                        tags$head(tags$style(
                          HTML('
                               #controls {background-color: rgba(45, 45, 45, 1)}'
                          ))),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = T, top = 80, left = "auto", right = 20, bottom = "auto",
                                      width = 650, height = "auto", style = "opacity: 0.99",
                                      
                                      h3("Watershed Investigator"),
                                      
                                      selectInput("plot_type", "Select plot type", c(
                                        "Elevation - Map" = "map_ele",
                                        "Elevation - Distribution" = "elevdist",
                                        "Temperature - Annual average - Map" = "map_tem",
                                        "Temperature - Average - Elevation bands" = "tmea_ann",
                                        "Temperature - Trend - Elevation bands" = "tslo_ann",
                                        "Precipitaiton - Annual average - Map" = "map_prc",
                                        "Precipitaiton - Average - Elevation bands" = "pmea_ann",
                                        "Precipitaiton - Trend - Elevation bands" = "pslo_ann",
                                        "Snow cover duration - Annual average - Map" = "map_scd",
                                        "Sde - mea - Map" = "map_swe",
                                        "Sde - mea - Band" = "smea_ann",
                                        "Sde - slo - Band" = "sslo_ann",
                                        "Ssu - mea - Band" = "vmea_ann",
                                        "Ssu - slo - Band" = "vslo_ann",
                                        "Sdc - mea - Band" = "vdif_ann",
                                        "Sdc - slo - Band" = "vdis_ann"
                                      )),
                                      plotOutput("elev_plot", width = "100%"),
                                      
                                      downloadButton(outputId = "down", label = "Save Plot", height = "1.0cm")
                                      
                        )
                    )
           )
)