library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  mapLng <- reactive({
    switch(input$location,
           "All of U.S." = -98.555878,
           "Washington, D.C." =  -77.036544,
           "Maryland" = -77.555766
          ) 
  })

  mapLat <- reactive({
    switch(input$location,
           "All of U.S." = 39.796851,
           "Washington, D.C." =  38.902727,
           "Maryland" = 39.075733
          ) 
  })
  
  mapZoom <- reactive({
    switch(input$location,
           "All of U.S." = 4,
           "Washington, D.C." = 12,
           "Maryland" = 8
          ) 
  })
  
  leafDat <- reactive({
        if(input$dataSelector == "DC Bikeshare Locations"){
          bikeshare <- read.csv("./data/Capital_Bike_Share_Locations.csv", stringsAsFactors = F)
          bikeshare$total <- bikeshare$NUMBER_OF_BIKES + bikeshare$NUMBER_OF_EMPTY_DOCKS
          bikeshare$OWNER[bikeshare$OWNER == ""] <- "OTHER"
          return(bikeshare)
        }else if(input$dataType == "Points" & input$dataSelector == "State Unemployment Rate"){
          centers <- data.frame(lat = state.center$y, lon = state.center$x, state=state.name)
          centers$lat[centers$state == "Hawaii"] <- 21.337643
          centers$lon[centers$state == "Hawaii"] <- -157.758295
          centers$lat[centers$state == "Alaska"] <- 64.739935
          centers$lon[centers$state == "Alaska"] <- -151.048953
          states <- read.csv("./data/states.csv", stringsAsFactors = FALSE)
          states <- left_join(centers, states)
          return(states)
        }else if(input$dataSelector == "DC Metro Lines"){
          dat <- readOGR("./data/Metro_Lines.geojson", stringsAsFactors = F, verbose = F)
          dat@data$color <- gsub(" .*","", dat@data$NAME)
          return(dat)
        }else if(input$dataSelector == "State Unemployment Rate"){
          dat <- readOGR("./data/states_geojson.geojson", "OGRGeoJSON")
      
        }else if(input$dataSelector == "State Mean Annual Wages"){
          dat <- readOGR("./data/states_geojson.geojson", "OGRGeoJSON")
        }else if(input$dataSelector == "State Total Employment"){
          dat <- readOGR("./data/states_geojson.geojson", "OGRGeoJSON")
        }else if(input$dataSelector == "Maryland County Mean Weekly Wages"){
          dat <- readOGR("./data/md_county_geojson.geojson", "OGRGeoJSON")
        }else if(input$dataSelector == "DC Block Group Population"){
          dat <- readOGR("./data/Census_Block_Groups__2010.geojson", "OGRGeoJSON")
        }else if(input$dataSelector == "DC Block Group Housing Units"){
          DCblocks <- readOGR("./data/Census_Block_Groups__2010.geojson", "OGRGeoJSON")
        }
  })
  
  
  output$leafletMap <- renderLeaflet({
     if(input$location != "Not Specified"){
        leaf <-  leaflet() %>%
                    setView(mapLng(), mapLat(),mapZoom())
     }else{
        leaf <- leaflet()
      }
    
    if(input$basemap == "Default"){
        leaf <- leaf %>%
          addTiles()
    }else if(input$basemap == "No Basemap"){
      
    }else{
        leaf <- leaf %>%
          addProviderTiles(input$basemap) 
    }

########## POINTS
    if(input$dataSelector == "DC Bikeshare Locations"){
      bikeshare <- leafDat()
      
      pal <- colorFactor(input$colorPalette, domain = bikeshare$OWNER, reverse = input$reverse)
      
      popup <-  with(bikeshare,paste(sep = "",
                                     "<b><h4>",ADDRESS,"</h4></b>",
                                     "<table>",
                                     "<tr><td><b># of Docks:</b></td><td>",total,"</td><tr>",
                                     "<tr><td><b># of Available Bikes:</b></td><td>",NUMBER_OF_BIKES,"</td><tr>",
                                     "<tr><td><b># of Empty Docks:</b></td><td>",NUMBER_OF_EMPTY_DOCKS,"</td><tr>",
                                     "</table>"))
      
      leaf <- leaf %>%
        addCircleMarkers(data = bikeshare, lng = ~LONGITUDE, lat = ~LATITUDE, popup=popup,
                         fill=TRUE, fillColor = ~pal(OWNER), fillOpacity = 0.7,
                         weight = 1, radius=~total/2.5, color= "black", stroke = TRUE) %>%
        addLegend(data = bikeshare, input$legendPosition, pal = pal, values = ~OWNER,
                  title = "Owner", opacity = 0.8)
    }else if(input$dataType == "Points" & input$dataSelector == "State Unemployment Rate"){
      states <- leafDat()

      pal <- colorNumeric(input$colorPalette, domain = states$unemp_rate, na.color = "grey46",reverse = input$reverse)

      popup <- paste0("<h2>",states$state,"</h2>",
                      "<b>Unemployment Rate: </b>",states$unemp_rate, "%<br>",
                      "<b>Rank: </b>",states$unemp_rank)
      
      leaf <- leaf %>%
        addCircleMarkers(data = states, lat= ~lat, lng=~lon, radius = ~unemp_rate*4.5, 
                         stroke = TRUE, color="black", weight=1,
                         fillColor=~pal(unemp_rate), fillOpacity = 0.9, opacity=0.9, popup = popup) %>%
        addLegend(data = states, input$legendPosition, pal = pal, values = ~unemp_rate,title = "Unemployment Rate", opacity = 0.8, 
                  labFormat = labelFormat(suffix = "%"))  
        
########## LINES      
    }else if(input$dataSelector == "DC Metro Lines"){
      dat <- leafDat()
      
      leaf <- leaf %>%
        addPolylines(data = dat, color=~color, opacity=0.6) 
########## POLYGONS      
    }else if(input$dataSelector == "State Unemployment Rate"){
      dat <- leafDat()
      
      pal <- colorNumeric(input$colorPalette, domain = dat@data$unemp_rate, reverse = input$reverse, na.color = "gray35")
      
      popup <-  with(dat@data,paste(sep = "",
                                     "<img src='https://brandonkopp.com/wp-content/uploads/2017/10/BLS_emblem_2016.png' height='40px' style='vertical-align: text-bottom'>",
                                     "<b style='font-size:2em;line-height:40px;margin-right:40px'> ",NAME,"</b><hr style='margin:5px'>",
                                     "<table style='font-size:1.2em; width:220px'>",
                                     "<tr><td><b>Unemployment Rate:</b></td><td>  ",unemp_rate,"%</td><tr>",
                                     "<tr><td><b>Rank:</b></td><td>  ",unemp_rank,"</td><tr>",
                                     "<tr><td><b>Total Employment:</b></td><td> ",format(tot_employment, big.mark = ","),"</td><tr>",
                                    "<tr><td><b>Mean Annual Wage:</b></td><td>$",format(mean_annual_salary, big.mark = ","),"</td><tr>",
                                     "</table>"))
      
      leaf <-  leaf %>%
        addPolygons(data = dat, weight = .3 ,color = "black",
                    fillColor = ~pal(unemp_rate),fillOpacity = 0.75, popup=popup,
                    highlightOptions = highlightOptions(weight=3,color="white",opacity = 1,bringToFront = T)) %>%
        addLegend(data = dat, input$legendPosition, pal = pal, values = ~unemp_rate, na.label = "",
                  title = "Unemployment Rate", opacity = 0.8, labFormat = labelFormat(suffix= "%"))
    }else if(input$dataSelector == "State Mean Annual Wages"){
      dat <- leafDat()
      
      pal <- colorNumeric(input$colorPalette, domain = dat@data$mean_annual_salary, reverse = input$reverse, na.color = "gray35")
      
      popup <-  with(dat@data,paste(sep = "",
                                    "<img src='https://brandonkopp.com/wp-content/uploads/2017/10/BLS_emblem_2016.png' height='40px' style='vertical-align: text-bottom'>",
                                    "<b style='font-size:2em;line-height:40px;margin-right:40px'> ",NAME,"</b><hr style='margin:5px'>",
                                    "<table style='font-size:1.2em; width:220px'>",
                                    "<tr><td><b>Unemployment Rate:</b></td><td>  ",unemp_rate,"%</td><tr>",
                                    "<tr><td><b>Rank:</b></td><td>  ",unemp_rank,"</td><tr>",
                                    "<tr><td><b>Total Employment:</b></td><td> ",format(tot_employment, big.mark = ","),"</td><tr>",
                                    "<tr><td><b>Mean Annual Wage:</b></td><td>$",format(mean_annual_salary, big.mark = ","),"</td><tr>",
                                    "</table>"))
      leaf <-  leaf %>%
        addPolygons(data = dat, weight = .3 ,color = "black",
                    fillColor = ~pal(mean_annual_salary),fillOpacity = 0.75, popup = popup,
                    highlightOptions = highlightOptions(weight=3,color="white",opacity = 1,bringToFront = T)) %>%
        addLegend(data = dat, input$legendPosition, pal = pal, values = ~mean_annual_salary, na.label = "",
                  title = "Mean Annual Wages", opacity = 0.8, labFormat = labelFormat(prefix= "$"))
    }else if(input$dataSelector == "State Total Employment"){
      dat <- leafDat()
      
      pal <- colorNumeric(input$colorPalette, domain = dat@data$tot_employment, reverse = input$reverse, na.color = "gray35")
      
      popup <-  with(dat@data,paste(sep = "",
                                    "<img src='https://brandonkopp.com/wp-content/uploads/2017/10/BLS_emblem_2016.png' height='40px' style='vertical-align: text-bottom'>",
                                    "<b style='font-size:2em;line-height:40px;margin-right:40px'> ",NAME,"</b><hr style='margin:5px'>",
                                    "<table style='font-size:1.2em; width:220px'>",
                                    "<tr><td><b>Unemployment Rate:</b></td><td>  ",unemp_rate,"%</td><tr>",
                                    "<tr><td><b>Rank:</b></td><td>  ",unemp_rank,"</td><tr>",
                                    "<tr><td><b>Total Employment:</b></td><td> ",format(tot_employment, big.mark = ","),"</td><tr>",
                                    "<tr><td><b>Mean Annual Wage:</b></td><td>$",format(mean_annual_salary, big.mark = ","),"</td><tr>",
                                    "</table>"))
      
      leaf <-  leaf %>%
        addPolygons(data = dat, weight = .3 ,color = "black",
                    fillColor = ~pal(tot_employment),fillOpacity = 0.75, popup = popup,
                    highlightOptions = highlightOptions(weight=3,color="white",opacity = 1,bringToFront = T)) %>%
        addLegend(data = dat, input$legendPosition, pal = pal, values = ~tot_employment, na.label = "",
                  title = "Total Employment", opacity = 0.8)
    }else if(input$dataSelector == "Maryland County Mean Weekly Wages"){
      dat <- leafDat()
      
      pal <- colorNumeric(input$colorPalette, domain = dat@data$avg_wkly_wage, reverse = input$reverse, na.color = "gray35")
      
      leaf <-  leaf %>%
        addPolygons(data = dat, weight = .3 ,color = "black",
                    fillColor = ~pal(avg_wkly_wage),fillOpacity = 0.75) %>%
        addLegend(data = dat, input$legendPosition, pal = pal, values = ~avg_wkly_wage, na.label = "",
                  title = "Mean Weekly Wage", opacity = 0.8, labFormat = labelFormat(prefix = "$"))
    }else if(input$dataSelector == "DC Block Group Population"){
      DCblocks <- leafDat()
      
      pal <- colorNumeric(input$colorPalette, domain = DCblocks@data$P0010001, reverse = input$reverse, na.color = "gray35")
      
      leaf <-  leaf %>%
        addPolygons(data = DCblocks, weight = .3 ,color = "black",
                    fillColor = ~pal(P0010001),fillOpacity = 0.75) %>%
        addLegend(data = DCblocks, input$legendPosition, pal = pal, values = ~P0010001, na.label = "",
                  title = "Population", opacity = 0.8)
    }else if(input$dataSelector == "DC Block Group Housing Units"){
      DCblocks <- leafDat()
      
      pal <- colorNumeric(input$colorPalette, domain = DCblocks@data$H0010001, reverse = input$reverse, na.color = "gray35")
      
      leaf <-  leaf %>%
        addPolygons(data = DCblocks, weight = .3 ,color = "black",
                    fillColor = ~pal(H0010001),fillOpacity = 0.75) %>%
        addLegend(data = DCblocks, input$legendPosition, pal = pal, values = ~H0010001, na.label = "",
                  title = "Housing Units", opacity = 0.8)
########## RASTER
    }else if(input$dataSelector == "DC 311 Calls"){
      load("./data/DC311.RData")
      
      color_pal <- colorNumeric(
        palette = rev(brewer.pal(9, input$colorPalette)), domain = values(loc_density_raster), 
        na.color = "transparent",
        reverse = input$reverse
      )
      
      leaf <- leaf %>%
        addRasterImage(x = loc_density_raster, colors=color_pal, opacity = 0.6, project = FALSE)
      
    }else{
      leaf
    }
    
  })

  output$locs <- renderUI({
     if(grepl("dc", input$dataSelector, ignore.case = T)){
       selectInput("location","Map Location", c("Not Specified","All of U.S.","Washington, D.C.", "Maryland"),
                   selected = "Washington, D.C.")
     }else if(grepl("state", input$dataSelector, ignore.case = T)){
       selectInput("location","Map Location", c("Not Specified","All of U.S.","Washington, D.C.", "Maryland"),
                   selected = "All of U.S.")
     }else if(grepl("maryland", input$dataSelector, ignore.case = T)){
       selectInput("location","Map Location", c("Not Specified","All of U.S.","Washington, D.C.", "Maryland"),
                   selected = "Maryland")
     }else{
       selectInput("location","Map Location", c("Not Specified","All of U.S.","Washington, D.C.", "Maryland"),
                   selected = "All of U.S.")
     }
  })
  
  output$dataSelect <- renderUI({
      if(input$dataType == "None"){
          selectInput("dataSelector", "Select Display Data", c("Specify Data Type"))
      }else if(input$dataType == "Points"){
        selectInput("dataSelector", "Select Display Data", choices = c("State Unemployment Rate", "DC Bikeshare Locations"))
      }else if(input$dataType == "Lines"){
        selectInput("dataSelector", "Select Display Data", c("DC Metro Lines"))
      }else if(input$dataType == "Polygons"){
        selectInput("dataSelector", "Select Display Data", 
                    list(BLS_Data = c("State Unemployment Rate", "State Mean Annual Wages", "State Total Employment", "Maryland County Mean Weekly Wages"),
                    DC_Data = c("DC Block Group Population", "DC Block Group Housing Units")))
      }else if(input$dataType == "Raster"){
        selectInput("dataSelector", "Select Display Data", c("DC 311 Calls"))
      }
  })
  
  
  output$cols <- renderUI({
    if(input$dataType == "None"){
      
    }else if(input$dataType == "Points"){
      selectInput("colorPalette", "Color Palette", 
                  choices =  list(
                    Factor = c("Set3","Set2","Set1","Paired","Pastel1","Dark2"),
                    Diverging = c("RdBu","RdYlBu","RdYlGn", "RdGy")))
    }else if(input$dataType == "Lines"){

    }else if(input$dataType == "Polygons"){
      selectInput("colorPalette", "Color Palette", choices = list(
        Diverging = c("RdBu","RdYlBu","RdYlGn", "RdGy"),
        Color_Ramp = c("Reds","Blues","Greens","Purples","Greys")))
    }else if(input$dataType == "Raster"){
      selectInput("colorPalette", "Color Palette", choices = list(
        Diverging = c("RdBu","RdYlBu","RdYlGn", "RdGy"),
        Color_Ramp = c("Reds","Blues","Greens","Purples","Greys")))
    }
  })
  
  output$revs <- renderUI({
    if(input$dataType %in% c("None","Lines")){
  
    }else{
      checkboxInput("reverse","Reverse Color Palette",value=FALSE)
    }
    
  })
  
  output$legend <- renderUI({
    if(input$dataType %in% c("None","Raster","Lines")){
      
    }else{
      selectInput("legendPosition", "Legend Position",c("bottomright","bottomleft","topleft","topright"))
    }
  })
  
})
