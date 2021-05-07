###

#Shiny dashboard to investigate elevation-dependent compensation effects in snowmelt
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

navbarPage("Snowmelt", id="nav", theme = shinytheme("slate"), selected = "Summary & Overview", position = "fixed-top",
           
           tags$style(type="text/css", "body {padding-top: 70px;}"),
           
           tabPanel("Summary & Overview",
                    
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
                    
                    HTML("<br><br>"),
                    
                    selectInput("var_plotly_ele_met", "",
                                choices = c("Tmax - mean",
                                            "Tmin - mean",
                                            "Tmax - trend",
                                            "Tmin - trend")),
                    
                    # verbatimTextOutput("click"),
                    
                    plotlyOutput("plotly_ele_met", width="100%")
                    
           ),
           
           tabPanel("Interactive map",
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
                               #controls {background-color: rgba(255, 255, 255, 1)}'
                          ))),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = T, top = 80, left = "auto", right = 20, bottom = "auto",
                                      width = 650, height = "auto", style = "opacity: 0.99",
                                      
                                      h3("Hydro Explorer")
                                      
                        )
                    )
           )
)