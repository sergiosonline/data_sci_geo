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
# fileloc <- "NEIGHBORHOODS_WGS84.shp"
neighborhoods <- rgdal::readOGR(dsn = fileloc, layer = "NEIGHBORHOODS_WGS84")
# neighborhoods <- rgdal::readOGR(dsn = fileloc)

# Load accidents data
accidents <- fread("https://raw.githubusercontent.com/sergiosonline/data_sci_geo/master/data/accidents.csv") %>%
  mutate(DATE = as.Date(DATE),
         TIME = paste0(Hour, ":", substr(TIME,nchar(TIME)-2+1, nchar(TIME)))) %>%
  group_by(ACCNUM) %>%
  mutate(parties_involved = n(),
         num_fatalities = sum(ACCLASS == "Fatal")) %>%
  ungroup()

# Icons
# vehicle_icon <- iconList(
#   bicycle = makeIcon("bicycle icon.png", iconWidth = 25, iconHeight = 25)
# )


shinyServer(function(input, output) {
  # Filter accident data
  filtered_accidents <- reactive({
    accidents %>%
      filter(DATE >= as.Date(input$acc_date[1]) & DATE <= as.Date(input$acc_date[2])) %>%
      filter(ACCLASS %in% input$fatal) %>%
      mutate(i_auto = if_else(AUTOMOBILE == "Yes" | TRUCK == "Yes" | TRSN_CITY_VEH == "Yes" |
                                EMERG_VEH == "Yes", "Automobile", NA_character_),
             i_bike = if_else(CYCLIST == "Yes", "Bicycle", NA_character_),
             i_moto = if_else(MOTORCYCLE == "Yes", "Motorcycle", NA_character_),
             i_ped = if_else(PEDESTRIAN == "Yes", "Pedestrian", NA_character_)) %>%
      mutate(vehicles_involved = gsub(", NA", "", 
                                      gsub("NA, ", "", paste(i_auto, i_bike, i_moto, i_ped, sep = ", "))),
             vehicles_involved_input = paste(sort(input$auto_type), collapse = ", ")) %>%
      filter(vehicles_involved == vehicles_involved_input) %>%
      mutate(filter_out = if_else("Rain" %in% input$precip & "Snow" %in% input$precip & 
                                    Tot_rain > 0 & Tot_snow > 0, 0,
                                          if_else("Rain" %in% input$precip & Tot_rain > 0, 0,
                                                  if_else("Snow" %in% input$precip & Tot_snow > 0, 0, 1)))) %>%
      filter(filter_out == 0) %>%
      select(-filter_out) %>%
      filter(Hour >= input$acc_time[1] & Hour <= input$acc_time[2])
  })
  
  # Color scheme for map
  # colorpal <- reactive({
  #   colorFactor(palette = c('darkorchid', 'darkturquoise'), domain = filtered_accidents$ACCLASS)
  # })
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
    leaflet() %>% addTiles() %>%
      
      addCircles(
        data = accidents,
        lng = ~ longitude, lat = ~ latitude,
        color = ~ pal2(ACCLASS),
        label = lapply(labs, HTML)
      )%>%
      addLegend(
        pal = pal_pop2,
        values = neighborhoods$X2016pop,
        opacity = 1,
        title = "2016 Population",
        labFormat = labelFormat(digits=7)
      )%>%
      addLegend(
        pal = pal2,
        values = accidents$ACCLASS,
        opacity = 1,
        title = 'Accident Class'
        
      ) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  output$acc_data <- renderDataTable(filtered_accidents())
  
})
