library(shiny)
library(tidyverse, quietly = T, warn.conflicts = F, verbose = F)
library(data.table, quietly = T, warn.conflicts = F, verbose = F)
library(sp)
library(htmltools)
library(leaflet)

# Load neighborhood polygons with 2016 population
## Download this file into local drive: 
## https://github.com/sergiosonline/data_sci_geo/tree/master/data/neighbourhoods_planning_areas_wgs84_SEB
## And update fileloc variable

fileloc <- "C:/Users/angel/OneDrive/Documents/GitHub/data_sci_geo/data/neighbourhoods_planning_areas_wgs84_SEB"
neighborhoods <- rgdal::readOGR(dsn = fileloc, layer = "NEIGHBORHOODS_WGS84")

# Load accidents data
accidents <- fread("https://raw.githubusercontent.com/sergiosonline/data_sci_geo/master/data/accidents.csv") %>%
  mutate(DATE = as.Date(DATE),
         TIME = paste0(Hour, ":", substr(TIME,nchar(TIME)-2+1, nchar(TIME)))) %>%
  group_by(ACCNUM) %>%
  mutate(parties_involved = n(),
         num_fatalities = sum(ACCLASS == "Fatal")) %>%
  ungroup()


shinyServer(function(input, output) {
  
  # Filter accident data
  filtered_accidents <- reactive({
    accidents %>%
      filter(DATE >= as.Date(input$acc_date[1]) & DATE <= as.Date(input$acc_date[2])) %>%
      filter(ACCLASS %in% input$fatal) %>%
      mutate()
  })
  
  # Color scheme for map
  pal2 <- colorFactor(palette = c('darkorchid', 'darkturquoise'), domain = accidents$ACCLASS)
  pal_pop <- colorFactor(palette = c('steel blue','indigo'), domain = neighborhoods$X2016pop)
  pal_pop2 <- colorNumeric(palette = hsv(1, seq(0,1,length.out = 12) , 1), neighborhoods$X2016pop, n = 5)
  
  # Pop-up label
  labs <- lapply(seq(nrow(accidents)), function(i) {
    paste0( '<p>', accidents[i, "DATE"], '<p></p>', 
            accidents[i, "STREET1"], ', ', 
            accidents[i, "STREET2"],'</p><p>', 
            accidents[i, "ACCLASS"], '</p>' ) 
  })
   
  output$acc_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircles(
        data = accidents,
        lng = ~ longitude, lat = ~ latitude,
        color = ~ pal2(ACCLASS),
        label = lapply(labs, HTML)
      ) %>%
      addLegend(
        pal = pal_pop2,
        values = neighborhoods$X2016pop,
        opacity = 1,
        title = "2016 Population",
        labFormat = labelFormat(digits=7)
      ) %>%
      addLegend(
        pal = pal2,
        values = accidents$ACCLASS,
        opacity = 1,
        title = 'Accident Class'
      )
  })
  
})
