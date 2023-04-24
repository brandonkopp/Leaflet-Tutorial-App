library(shiny)
library(leaflet)
library(geojsonio)
library(raster)
library(RColorBrewer)
library(dplyr)

shinyUI(navbarPage("Leaflet Tutorial",id="nav",
#### MAP PANEL  
  tabPanel("Interactive Map",
       absolutePanel(top = 60, left = 50, class = "panel panel-default", 
           bottom = "auto", height="auto", width=290,fixed = TRUE,
           style = "z-index:500;opacity: 0.90;padding: 10px; border-bottom: 1px solid #CCC; background: #e5f2ff;",
           h3("Options"),
           selectInput("basemap", "Basemap", c("Default","No Basemap","CartoDB.Positron","CartoDB.DarkMatter",
                                       "Stamen.Toner","Stamen.TonerLite","Stamen.Terrain",
                                       "Esri.WorldStreetMap","Esri.WorldImagery","Esri.NatGeoWorldMap","OpenTopoMap"),
                          selected = "Default"),
           uiOutput("locs"),
           selectInput("dataType", "Data Type", c("None","Points", "Lines","Polygons","Raster"),
                       selected = "None"),
           uiOutput("dataSelect"),
           uiOutput("cols"),
           uiOutput("revs"), 
           uiOutput("legend")
           
           ),
            
           div(class="outer",
               tags$head(
                 includeCSS("./www/styles.css")
               ),
               #Display Map
               leafletOutput("leafletMap", width = "100%", height = "100%")
           )
  ),
#### LEAFLET TUTORIAL
  tabPanel("Tutorial",
         HTML("<iframe src='Leaflet-in-R.html' width='100%' height='800px' frameborder='0'>")    
  )
))
