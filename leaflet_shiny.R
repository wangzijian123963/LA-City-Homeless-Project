library(shiny)
library(dplyr)
library(rgdal)
library(ggplot2)
library(leaflet)


#load shape file
ct_311=readOGR("ct_311.shp")
cd_311=readOGR("CD_calls_map.shp")
la_map_ct=readOGR("la_map_ct.shp")
la_map_cd=readOGR("la_map_cd.shp")
ct_crime = readOGR("CT_map_crime.shp")
cd_crime = readOGR("CD_map_crime.shp")

# spTransform
ct_311<- spTransform(ct_311, CRS("+init=epsg:4326"))
cd_311<- spTransform(cd_311, CRS("+init=epsg:4326"))
la_map_ct<- spTransform(la_map_ct, CRS("+init=epsg:4326"))
la_map_cd<- spTransform(la_map_cd, CRS("+init=epsg:4326"))
ct_crime = spTransform(ct_crime, CRS("+init=epsg:4326"))
cd_crime = spTransform(cd_crime, CRS("+init=epsg:4326"))

#extract data frame
ct311<-ct_311@data
cd311<-cd_311@data
lamap_ct<-la_map_ct@data
lamap_cd<-la_map_cd@data
ct_data = ct_crime@data
cd_data = cd_crime@data

# load data frame for markers
migration_data<-read.csv("migration.csv")
vacant<-read.csv("Vacant Property.csv")
prob.encamp<-read.csv("Problematic Encampment.csv")

# GUI
ui <-navbarPage("City of LA Project", id="nav",
                
                # Sheltered/Unsheltered Population
                tabPanel("Homeless Demographics",
                         
                         # If not using custom CSS, set height of leafletOutput to a number instead of percent
                         leafletOutput("hcmap", width="1400", height="900"),
                         
                         # Shiny versions prior to 0.11 should use class = "modal" instead.
                         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                       draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                       width = 350, height = "auto",
                                       #title
                                       h2("Explorer"),
                                       # geo variables
                                       selectInput(inputId = "geolev",
                                                   label = "Select Geographical Variable",
                                                   choices = list ("Census Tract",
                                                                   "Council District"),
                                                   selected = "Census Tract"),
                                       # measurement
                                       selectInput(inputId = "meas",
                                                   label = "Select Measurement",
                                                   choices = list ("Count",
                                                                   "Density"),
                                                   selected = "Count"),
                                       conditionalPanel("input.meas == 'Density'",
                                                        # Only prompt for threshold when measurement = density
                                                        selectInput("dns",
                                                                    label="Select a Density Type",
                                                                    choices = list("Total",
                                                                                   "Sheltered",
                                                                                   "Unsheltered"))),
                                       conditionalPanel("input.meas == 'Count'",
                                                        # sheltered/unsheltered
                                                        radioButtons("uors", label = "Select a Homeless Type",
                                                                     choices = list("Sheltered Homeless" = "s", "Unsheltered Homeless" = "u"), 
                                                                     selected = "u")),
                                       conditionalPanel("input.meas == 'Count' & input.uors == 'u'",
                                                        # Only prompt for threshold when measurement = Count and unsheltered
                                                        selectInput("unshelter",
                                                                    label="Select an Unsheltered Homeless Demographics",
                                                                    choices = list("Total",
                                                                                   "Cars",
                                                                                   "Camper",
                                                                                   "Encampments",
                                                                                   "Vans",
                                                                                   "Tents")),
                                                        radioButtons("vorp", label = "",
                                                                     choices = list("Vacant_Property" = "v", "Problematic Encampment" = "p", "Null" = "n"),
                                                                     selected = "n"
                                                        )
                                       ),
                                       conditionalPanel("input.meas == 'Count' & input.uors == 's'",
                                                        # Only prompt for threshold when measurement = Count and sheltered
                                                        selectInput("shelter",
                                                                    label="Select a Sheltered Homeless Demographics",
                                                                    choices = list("Total",
                                                                                   "Emergency Shelter",
                                                                                   "Transitional Housing"))),
                                       # slider
                                       sliderInput("tophc", label = "Top N Areas", min = 0,
                                                   max = 15, value = 10, round=TRUE),
                                       
                                       plotOutput("hcbar", height = 300)
                         )
                ),
                
                
                # 311 Calls
                tabPanel("311 Calls",
                         
                         # If not using custom CSS, set height of leafletOutput to a number instead of percent
                         leafletOutput("callsmap", width="1400", height="900"),
                         
                         # Shiny versions prior to 0.11 should use class = "modal" instead.
                         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                       draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                       width = 350, height = "auto",
                                       #title
                                       h2("Explorer"),
                                       # geo variables
                                       selectInput(inputId = "geovar",
                                                   label = "Select Geographical Variable",
                                                   choices = list ("Census Tract",
                                                                   "Council District"),
                                                   selected = "Census Tract"),
                                       # measurement
                                       selectInput(inputId = "measure",
                                                   label = "Select Measurement",
                                                   choices = list ("Count",
                                                                   "Density"),
                                                   selected = "Count"),
                                       # slider
                                       sliderInput("top", label = "Top N Areas", min = 0,
                                                   max = 15, value = 10, round=TRUE),
                                       
                                       plotOutput("callsbar", height = 350)
                         )
                ),
                
                # Crime
                tabPanel("Crime",
                         
                         # If not using custom CSS, set height of leafletOutput to a number instead of percent
                         leafletOutput("crimemap", width="1400", height="900"),
                         
                         # Shiny versions prior to 0.11 should use class = "modal" instead.
                         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                       draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                       width = 350, height = "auto",
                                       #title
                                       h2("Explorer"),
                                       # geo variables
                                       selectInput(inputId = "geolv",
                                                   label = "Select Geographical Variable",
                                                   choices = list ("Census Tract",
                                                                   "Council District"),
                                                   selected = "Census Tract"),
                                       # measurement
                                       selectInput(inputId = "measurement",
                                                   label = "Select Measurement",
                                                   choices = list ("Count",
                                                                   "Crime Index"),
                                                   selected = "Count"),
                                       # slider
                                       sliderInput("topcrime", label = "Top N Areas", min = 0,
                                                   max = 15, value = 10, round=TRUE),
                                       
                                       plotOutput("crimebar", height = 350)
                         )
                ),
                # Migration
                tabPanel("Migration",
                         
                         # If not using custom CSS, set height of leafletOutput to a number instead of percent
                         leafletOutput("migmap", width="1400", height="900"),
                         
                         # Shiny versions prior to 0.11 should use class = "modal" instead.
                         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                       draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                       width = 350, height = "auto",
                                       #title
                                       h2("Explorer"),
                                       
                                       # slider
                                       sliderInput("month", label = "Select Month", min = 1,
                                                   max = 10, value = 1, round=TRUE),
                                       # ct
                                       numericInput("ct", label = "Enter Census Tract ID", value = 206300)
                                       
                         )
                )
)

# Server
server <- function(input, output, session) {
  
  # Homeless Demographics
  data_hc <- reactive({
    switch(input$geolev,
           "Census Tract"=la_map_ct,
           "Council District"=la_map_cd)
  })
  
  
  bar_hc<-reactive({
    switch(input$geolev,
           "Census Tract"=lamap_ct,
           "Council District"=lamap_cd)
  })
  
  mea_hc=reactive({ if(input$meas == "Density") {
    switch(input$dns,
           "Total"=bar_hc()$tt_dnst,
           "Sheltered"=bar_hc()$s_dnsty,
           "Unsheltered"=bar_hc()$u_dnsty)
  } else if (input$meas == "Count" & input$uors == "s") {
    switch(input$shelter,
           "Total"=bar_hc()$s_pop,
           "Emergency Shelter"=bar_hc()$s_ES,
           "Transitional Housing"=bar_hc()$s_TH)
  } else if (input$meas == "Count" & input$uors == "u") {
    switch(input$unshelter,
           "Total"=bar_hc()$u_pop,
           "Cars"=bar_hc()$u_car,
           "Camper"=bar_hc()$u_campr,
           "Encampments"=bar_hc()$u_encmp,
           "Vans"=bar_hc()$u_van,
           "Tents"=bar_hc()$u_tent)
  }
    
  })
  
  bar2_hc=reactive({head(arrange(bar_hc(),desc(mea_hc())),input$tophc)})
  
  barmea_hc=reactive({ if(input$meas == "Density") {
    switch(input$dns,
           "Total"=bar2_hc()$tt_dnst,
           "Sheltered"=bar2_hc()$s_dnsty,
           "Unsheltered"=bar2_hc()$u_dnsty)
  } else if (input$meas == "Count" & input$uors == "s") {
    switch(input$shelter,
           "Total"=bar2_hc()$s_pop,
           "Emergency Shelter"=bar2_hc()$s_ES,
           "Transitional Housing"=bar2_hc()$s_TH)
  } else if (input$meas == "Count" & input$uors == "u") {
    switch(input$unshelter,
           "Total"=bar2_hc()$u_pop,
           "Cars"=bar2_hc()$u_car,
           "Camper"=bar2_hc()$u_campr,
           "Encampments"=bar2_hc()$u_encmp,
           "Vans"=bar2_hc()$u_van,
           "Tents"=bar2_hc()$u_tent)
  }
    
  })
  
  name_hc<-reactive({switch(input$geolev,
                            "Census Tract"=bar2_hc()$CT10,
                            "Council District"=bar2_hc()$distrct)})
  
  output$hcbar<-renderPlot({
    
    #barchart
    ggplot(bar2_hc(), aes(x=reorder(as.factor(name_hc()),desc(barmea_hc())),y=barmea_hc()))+
      geom_bar(stat="identity")+
      labs(x="",y="")
    
  })
  
  
  # hc basemap
  output$hcmap <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>%
      addTiles() %>%
      setView(-118.2515, 33.9, zoom = 10)
  })
  
  # homeless polygons
  observe({
    g<-input$geolev
    m<-input$meas
    d<-input$dns
    s<-input$shelter
    u<-input$unshelter
    us<-input$uors
    
    # Census Tract
    if (g == "Census Tract" & m == "Density" & d == "Total") {
      colorData_hc<-la_map_ct$tt_dnst
      pal_hc<-colorQuantile("YlOrRd", domain = colorData_hc, n=7)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people/ mi<sup>2</sup>",
        la_map_ct$CT10, la_map_ct$tt_dnst
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Census Tract" & m == "Density" & d == "Sheltered") {
      colorData_hc<-la_map_ct$s_dnsty
      bins <- c(0, 7, 20, 50, 80, 200, 400, 500, 1000, Inf)
      pal_hc <- colorBin("YlOrRd", domain = la_map_ct$s_dnsty, bins = bins)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people/ mi<sup>2</sup>",
        la_map_ct$CT10, la_map_ct$s_dnsty
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Census Tract" & m == "Density" & d == "Unsheltered") {
      colorData_hc<- la_map_ct$u_dnsty
      bins <- c(0, 7, 20, 50, 80, 200, 400, 500, 1000, Inf)
      pal_hc <- colorBin("YlOrRd", domain = la_map_ct$u_dnsty, bins = bins)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people/ mi<sup>2</sup>",
        la_map_ct$CT10, la_map_ct$u_dnsty
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Census Tract" & m == "Count" & us == "s" & s == "Total") {
      colorData_hc<-la_map_ct$s_pop
      bins <- c(0, 10, 20, 50, 100, 200, 400, 500, 1000, Inf)
      pal_hc <- colorBin("YlOrRd", domain = la_map_ct$s_pop, bins = bins)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people",
        la_map_ct$CT10, la_map_ct$s_pop
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Census Tract" & m == "Count" & us == "s" & s == "Emergency Shelter") {
      colorData_hc<-la_map_ct$s_ES
      bins <- c(0, 10, 20, 50, 100, 200, 400, 500, 1000, Inf)
      pal_hc <- colorBin("YlOrRd", domain = la_map_ct$s_ES, bins = bins)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people",
        la_map_ct$CT10, la_map_ct$s_ES
      ) %>% lapply(htmltools::HTML)
      
    }  else if (g == "Census Tract" & m == "Count" & us == "s" & s == "Transitional Housing") {
      colorData_hc<-la_map_ct$s_TH
      bins <- c(0, 10, 20, 50, 100, 200, 400, 500, 1000, Inf)
      pal_hc <- colorBin("YlOrRd", domain = la_map_ct$s_TH, bins = bins)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people",
        la_map_ct$CT10, la_map_ct$s_TH
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Census Tract" & m == "Count" & us == "u" & u == "Total") {
      colorData_hc<-la_map_ct$u_pop
      pal_hc<-colorQuantile("YlOrRd", domain = colorData_hc, n=8)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people",
        la_map_ct$CT10, la_map_ct$u_pop
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Census Tract" & m == "Count" & us == "u" & u == "Cars") {
      colorData_hc<-la_map_ct$u_car
      bins <- c(0, 5, 10, 20, 50, 70, Inf)
      pal_hc <- colorBin("YlOrRd", domain = la_map_ct$s_TH, bins = bins)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people",
        la_map_ct$CT10, la_map_ct$u_car
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Census Tract" & m == "Count" & us == "u" & u == "Camper") {
      colorData_hc<-la_map_ct$u_campr
      bins <- c(0, 10, 50, 80, 100, 130, 150, Inf)
      pal_hc <- colorBin("YlOrRd", domain = la_map_ct$s_TH, bins = bins)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people",
        la_map_ct$CT10, la_map_ct$u_campr
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Census Tract" & m == "Count" & us == "u" & u == "Encampments") {
      colorData_hc<-la_map_ct$u_encmp
      bins <- c(0, 10, 50, 80, 100, 130, 180, Inf)
      pal_hc <- colorBin("YlOrRd", domain = la_map_ct$s_TH, bins = bins)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people",
        la_map_ct$CT10, la_map_ct$u_encmp
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Census Tract" & m == "Count" & us == "u" & u == "Vans") {
      colorData_hc<-la_map_ct$u_van
      bins <- c(0, 5, 15, 20, 30, 50, Inf)
      pal_hc <- colorBin("YlOrRd", domain = la_map_ct$s_TH, bins = bins)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people",
        la_map_ct$CT10, la_map_ct$u_van
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Census Tract" & m == "Count" & us == "u" & u == "Tents") {
      colorData_hc<-la_map_ct$u_tent
      bins <- c(0, 10, 20, 50, 100, 200, 400, Inf)
      pal_hc <- colorBin("YlOrRd", domain = la_map_ct$s_TH, bins = bins)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people",
        la_map_ct$CT10, la_map_ct$u_tent
      ) %>% lapply(htmltools::HTML)
      
    } 
    
    # Council District
    else if (g == "Council District" & m == "Density" & d == "Total") {
      colorData_hc<-la_map_cd$tt_dnst
      pal_hc<-colorQuantile("YlOrRd", domain = colorData_hc, n=7)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people/ mi<sup>2</sup>",
        la_map_cd$distrct, la_map_cd$tt_dnst
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Council District" & m == "Density" & d == "Sheltered") {
      colorData_hc<-la_map_cd$s_dnsty
      pal_hc<-colorQuantile("YlOrRd", domain = colorData_hc, n=7)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people/ mi<sup>2</sup>",
        la_map_cd$distrct, la_map_cd$s_dnsty
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Council District" & m == "Density" & d == "Unsheltered") {
      colorData_hc<- la_map_cd$u_dnsty
      pal_hc<-colorQuantile("YlOrRd", domain = colorData_hc, n=7)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people/ mi<sup>2</sup>",
        la_map_cd$distrct, la_map_cd$u_dnsty
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Council District" & m == "Count" & us == "s" & s == "Total") {
      colorData_hc<-la_map_cd$s_pop
      pal_hc<-colorQuantile("YlOrRd", domain = colorData_hc, n=7)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people",
        la_map_cd$distrct, la_map_cd$s_pop
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Council District" & m == "Count" & us == "s" & s == "Emergency Shelter") {
      colorData_hc<-la_map_cd$s_ES
      bins <- c(0, 10, 20, 50, 100, 200, 400, 500, 1000, Inf)
      pal_hc <- colorBin("YlOrRd", domain = la_map_ct$s_ES, bins = bins)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people",
        la_map_cd$distrct, la_map_cd$s_ES
      ) %>% lapply(htmltools::HTML)
      
    }  else if (g == "Council District" & m == "Count" & us == "s" & s == "Transitional Housing") {
      colorData_hc<-la_map_cd$s_TH
      bins <- c(0, 10, 20, 50, 100, 200, 400, 500, 1000, Inf)
      pal_hc <- colorBin("YlOrRd", domain = la_map_ct$s_TH, bins = bins)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people",
        la_map_cd$distrct, la_map_cd$s_TH
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Council District" & m == "Count" & us == "u" & u == "Total") {
      colorData_hc<-la_map_cd$u_pop
      pal_hc<-colorQuantile("YlOrRd", domain = colorData_hc, n=8)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people",
        la_map_cd$distrct, la_map_cd$u_pop
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Council District" & m == "Count" & us == "u" & u == "Cars") {
      colorData_hc<-la_map_cd$u_car
      bins <- c(0, 50, 80, 100, 150, 200, Inf)
      pal_hc <- colorBin("YlOrRd", domain = la_map_ct$s_TH, bins = bins)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people",
        la_map_cd$distrct, la_map_cd$u_car
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Council District" & m == "Count" & us == "u" & u == "Camper") {
      colorData_hc<-la_map_cd$u_campr
      bins <- c(0, 100, 200, 300, 400, 500, 600, Inf)
      pal_hc <- colorBin("YlOrRd", domain = la_map_ct$s_TH, bins = bins)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people",
        la_map_cd$distrct, la_map_cd$u_campr
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Council District" & m == "Count" & us == "u" & u == "Encampments") {
      colorData_hc<-la_map_cd$u_encmp
      bins <- c(0, 50, 100, 200, 400, 600, 750, Inf)
      pal_hc <- colorBin("YlOrRd", domain = la_map_ct$s_TH, bins = bins)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people",
        la_map_cd$distrct, la_map_cd$u_encmp
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Council District" & m == "Count" & us == "u" & u == "Vans") {
      colorData_hc<-la_map_cd$u_van
      bins <- c(0, 50, 80, 100, 200, 300, Inf)
      pal_hc <- colorBin("YlOrRd", domain = la_map_ct$s_TH, bins = bins)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people",
        la_map_cd$distrct, la_map_cd$u_van
      ) %>% lapply(htmltools::HTML)
      
    } else if (g == "Council District" & m == "Count" & us == "u" & u == "Tents") {
      colorData_hc<-la_map_cd$u_tent
      pal_hc<-colorQuantile("YlOrRd", domain = colorData_hc, n=8)
      label_hc <- sprintf(
        "<strong>%s</strong><br/>%g people",
        la_map_cd$distrct, la_map_cd$u_tent
      ) %>% lapply(htmltools::HTML)
      
    } 
    
    
    
    if (input$vorp == "v"){
      leafletProxy("hcmap", data = data_hc()) %>%
        clearShapes()%>%
        clearMarkers()%>%
        clearMarkerClusters()%>%
        addPolygons(fillColor = ~pal_hc(colorData_hc),
                    weight = 2,
                    opacity = 1,
                    color = "black",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = label_hc,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))%>%
        addMarkers(data=vacant,
                   clusterOptions = markerClusterOptions()
        )
    } else if (input$vorp == "p") {
      leafletProxy("hcmap", data = data_hc()) %>%
        clearShapes()%>%
        clearMarkers()%>%
        clearMarkerClusters()%>%
        addPolygons(fillColor = ~pal_hc(colorData_hc),
                    weight = 2,
                    opacity = 1,
                    color = "black",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = label_hc,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))%>%
        addMarkers(data=prob.encamp,
                   clusterOptions = markerClusterOptions()
        )
    } else {
      leafletProxy("hcmap", data = data_hc()) %>%
        clearShapes()%>%
        clearMarkers()%>%
        clearMarkerClusters()%>%
        addPolygons(fillColor = ~pal_hc(colorData_hc),
                    weight = 2,
                    opacity = 1,
                    color = "black",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = label_hc,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))
      
    } 
  })
  
  
  
  # 311 Calls
  
  # Reactive expression for the data subsetted to what the user selected
  data_311 <- reactive({
    switch(input$geovar,
           "Census Tract"=ct_311,
           "Council District"=cd_311)
  })
  
  bar_311<-reactive({
    switch(input$geovar,
           "Census Tract"=ct311,
           "Council District"=cd311)
  })
  
  mea=reactive({switch(input$measure,
                       "Count"=bar_311()$cll_cnt,
                       "Density"=bar_311()$cll_dns)})
  
  bar2_311=reactive({head(arrange(bar_311(),desc(mea())),input$top)})
  
  barmea=reactive({switch(input$measure,
                          "Count"=bar2_311()$cll_cnt,
                          "Density"=bar2_311()$cll_dns)})
  
  name<-reactive({switch(input$geovar,
                         "Census Tract"=bar2_311()$CT10,
                         "Council District"=bar2_311()$distrct)})
  
  output$callsbar<-renderPlot({
    
    #barchart
    ggplot(bar2_311(), aes(x=reorder(as.factor(name()),desc(barmea())),y=barmea()))+
      geom_bar(stat="identity")+
      labs(x="",y="")
    
  })
  
  # 311 basemap
  output$callsmap <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>%
      addTiles() %>%
      setView(-118.2515, 33.9, zoom = 10)
  })
  
  # 311 polygons
  observe({
    geo<-input$geovar
    mea<-input$measure
    
    if (geo == "Census Tract" & mea == "Count") {
      colorData<-ct_311$cll_cnt
      pal<-colorQuantile("YlOrRd", domain = colorData, n=9)
      label <- sprintf(
        "<strong>%s</strong><br/>%g calls",
        ct_311$CT10, ct_311$cll_cnt
      ) %>% lapply(htmltools::HTML)
      
    } else if (geo == "Census Tract" & mea == "Density") {
      colorData<-ct_311$cll_dns
      pal<-colorQuantile("YlOrRd", domain = colorData, n=9)
      label<- sprintf(
        "<strong>%s</strong><br/>%g calls/ mi<sup>2</sup>",
        ct_311$CT10, ct_311$cll_dns
      ) %>% lapply(htmltools::HTML)
      
    } else if (geo == "Council District" & mea == "Count") {
      colorData<-cd_311$cll_cnt
      pal<-colorQuantile("YlOrRd", domain = colorData, n=5)
      label <- sprintf(
        "<strong>%s</strong><br/>%g calls",
        cd_311$distrct, cd_311$cll_cnt
      ) %>% lapply(htmltools::HTML)
      
    } else {
      colorData<-cd_311$cll_dns
      pal<-colorQuantile("YlOrRd", domain = colorData, n=5)
      label <- sprintf(
        "<strong>%s</strong><br/>%g calls/ mi<sup>2</sup>",
        cd_311$distrct, cd_311$cll_dns
      ) %>% lapply(htmltools::HTML)
      
    }
    
    leafletProxy("callsmap", data = data_311()) %>%
      clearShapes()%>%
      addPolygons(fillColor = ~pal(colorData),
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = label,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      )
  })
  
  # Crime
  
  data_crime <- reactive({
    switch(input$geolv,
           "Census Tract"=ct_crime,
           "Council District"=cd_crime)
  })
  
  bar_crime<-reactive({
    switch(input$geolv,
           "Census Tract"=ct_data,
           "Council District"=cd_data)
  })
  
  mea_crime=reactive({switch(input$measurement,
                             "Count"=bar_crime()$incdnts,
                             "Crime Index"=bar_crime()$crm_ndx)})
  
  bar2_crime=reactive({head(arrange(bar_crime(),desc(mea_crime())),input$topcrime)})
  
  barmea_crime=reactive({switch(input$measurement,
                                "Count"=bar2_crime()$incdnts,
                                "Crime Index"=bar2_crime()$crm_ndx)})
  
  name_crime<-reactive({switch(input$geolv,
                               "Census Tract"=bar2_crime()$CT10,
                               "Council District"=bar2_crime()$distrct)})
  
  output$crimebar<-renderPlot({
    
    #barchart
    ggplot(bar2_crime(), aes(x=reorder(as.factor(name_crime()),desc(barmea_crime())),y=barmea_crime()))+
      geom_bar(stat="identity")+
      labs(x="",y="")
    
  })
  
  output$crimemap <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>%
      addTiles() %>%
      setView(-118.2515, 33.9, zoom = 10)
  })
  
  
  observe({
    geoc<-input$geolv
    meac<-input$measurement
    
    if (geoc == "Census Tract" & meac == "Count") {
      colorData<-ct_crime$incdnts
      bins = c(0,10,100,300,400)
      pal <- colorBin("YlOrRd", domain = colorData, bins = bins)
      #pal = colorNumeric(palette = c("white","Navy"), domain = colorData)
      #pal<- colorQuantile("YlOrRd", domain = colorData, n=4)
      label <- sprintf(
        "<strong>%s</strong><br/>%g incidents",
        ct_crime$CT10, ct_crime$incdnts
      ) %>% lapply(htmltools::HTML)
      
    } else if (geoc == "Census Tract" & meac == "Density") {
      colorData<-ct_crime$crm_ndx^0.2
      bins = c(0,3,6,9,12)
      pal <- colorBin("YlOrRd", domain = colorData, bins = bins)
      #pal = colorNumeric(palette = c("white","Navy"), domain = colorData)
      #pal<- colorQuantile("YlOrRd", domain = colorData, n=4)
      label<- sprintf(
        "<strong>%s</strong><br/>%g crime.idx",
        ct_crime$CT10, round(ct_crime$crm_ndx^0.2,2)
      ) %>% lapply(htmltools::HTML)
      
    } else if (geoc == "Council District" & meac == "Count") {
      colorData<-cd_crime$incdnts
      bins = c(0,100,300,500,1000)
      pal <- colorBin("YlOrRd", domain = colorData, bins = bins)
      #pal = colorNumeric(palette = c("white","Navy"), domain = colorData)
      #pal<- colorQuantile("YlOrRd", domain = colorData, n=4)
      label <- sprintf(
        "<strong>%s</strong><br/>%g incidents",
        cd_crime$distrct, cd_crime$incdnts
      ) %>% lapply(htmltools::HTML)
      
    } else {
      colorData<-cd_crime$crm_ndx^0.5
      bins = c(0,20,40,60)
      pal <- colorBin("YlOrRd", domain = colorData, bins = bins)
      #pal = colorNumeric(palette = c("white","Navy"), domain = colorData)
      #pal<- colorQuantile("YlOrRd", domain = colorData, n=4)
      label <- sprintf(
        "<strong>%s</strong><br/>%g crime.idx",
        cd_crime$distrct, round(cd_crime$crm_ndx^0.5,2)
      ) %>% lapply(htmltools::HTML)
      
    }
    
    leafletProxy("crimemap", data = data_crime()) %>%
      clearShapes()%>%
      addPolygons(fillColor = ~pal(colorData),
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = label,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      )
  })
  
  
  # Migration
  data_month=reactive({filter(migration_data,createmonth==input$month) })
  data_ct=reactive({if (input$ct == 0) {
    data_month()
  } else {filter(data_month(),CT10==input$ct)}
  })
  output$migmap <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    
    leaflet() %>%
      addTiles() %>%
      setView(-118.2515, 33.9, zoom = 10)
    
  })
  observe({
    label_calls <- sprintf(
      "<strong>%s</strong>",
      ct_311$CT10
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("migmap", data = data_ct()) %>%
      clearShapes()%>%
      clearMarkers()%>%
      clearMarkerClusters()%>%
      addPolygons(data=la_map_ct,
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.5,
                    bringToFront = TRUE),
                  label = label_calls,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))%>%
      addMarkers(
        clusterOptions = markerClusterOptions()
      )
  })
}

# Run App
shinyApp(ui, server)