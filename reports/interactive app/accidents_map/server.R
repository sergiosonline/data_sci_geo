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
accidents <- fread("https://raw.githubusercontent.com/sergiosonline/data_sci_geo/master/data/final/accidents.csv") %>%
  filter(acc_class != "Property Damage Only")

# Number of parties involved in an accident as well as number of fatalities
per_accident <- accidents %>%
  group_by(accident_key) %>%
  summarize(parties_involved = n(),
            num_fatalities = sum(injury == "Fatal"))

# Combine with per accident information
accidents <- accidents %>%
  left_join(per_accident, by = c("accident_key" = "accident_key")) %>%
  select(-division, -ward_num, -hood_num)

# Icons
# vehicle_icon <- iconList(
#   bicycle = makeIcon("bicycle icon.png", iconWidth = 25, iconHeight = 25)
# )

rm(per_accident)

shinyServer(function(input, output) {
  # Filter accident data
  filtered_accidents <- reactive({
    accidents %>%
      filter(date >= as.Date(input$acc_date[1]) & date <= as.Date(input$acc_date[2])) %>%
      filter(acc_class %in% input$fatal) %>%
      mutate(i_bike = if_else(inv_cyc == 1, "Bicycle", NA_character_),
             i_emerg = if_else(inv_emergveh == 1, "Emergency Vehicle", NA_character_),
             i_moto = if_else(inv_moto == 1, "Motorcycle", NA_character_),
             i_ped = if_else(inv_ped == 1, "Pedestrian", NA_character_),
             i_truck = if_else(inv_truck == 1, "Truck", NA_character_)) %>%
      mutate(vehicles_involved = gsub("NA", "", gsub(", NA", "", 
                                      gsub("NA, ", "", paste(i_bike, i_emerg, i_moto, i_ped, i_truck, 
                                                             sep = ", ")))),
             vehicles_involved_input = as.character(paste(sort(input$auto_type), collapse = ", "))) %>%
      filter(vehicles_involved == vehicles_involved_input) %>%
      select(-i_bike, -i_emerg, -i_moto, -i_ped, -i_truck, -vehicles_involved,
             -vehicles_involved_input) %>%
      filter(("Precipitated" %in% input$precip & tot_precip_mm > 0) | 
               ("Clear" %in% input$precip & tot_precip_mm == 0) | is.na(input$precip)) %>%
      filter(hour >= input$acc_time[1] & hour <= input$acc_time[2])
  })
  
  # Plotting data
  filtered_plot_accidents <- reactive({
    filtered_accidents() %>%
      group_by(accident_key) %>%
      summarize_at(.vars = vars(long, lat, acc_class, date, street1, street2, parties_involved,
                                acc_class),
                   .funs = funs(unique(.)))
  })
  
  # Color scheme for map
  # colorpal <- reactive({
  #   colorFactor(palette = c('darkorchid', 'darkturquoise'), domain = filtered_accidents$ACCLASS)
  # })
  pal2 <- colorFactor(palette = c('darkorchid', 'darkturquoise'), domain = accidents$acc_class)
  pal_pop <- colorFactor(palette = c('steel blue','indigo'), domain = neighborhoods$X2016pop)
  pal_pop2 <- colorNumeric(palette = hsv(1, seq(0,1,length.out = 12) , 1), neighborhoods$X2016pop, n = 5)
  
  # Pop-up label
  labs <- reactive({lapply(seq(nrow(filtered_plot_accidents())), function(i) {
    paste0('<b>', filtered_plot_accidents()[i, "acc_class"], "</b><br/>",
           filtered_plot_accidents()[i, "date"], '<br/>',
           filtered_plot_accidents()[i, "street1"], ', ', 
           filtered_plot_accidents()[i, "street2"],'<br/>',
           "Parties involved: ", filtered_plot_accidents()[i, "parties_involved"])})
  })
   
  output$acc_map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addCircles(
        data = filtered_plot_accidents(),
        lng = ~ long, lat = ~ lat,
        color = ~ pal2(acc_class),
        label = lapply(labs(), HTML)
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
        values = filtered_plot_accidents()$acc_class,
        opacity = 1,
        title = 'Accident Class'
        
      ) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  
  filtered_table_accidents <- reactive({
    if(is.null(input$acc_map_click))
      return(filtered_accidents())
    filtered_accidents() %>%
      filter(lat == input$acc_map_click[1] &
               long == input$acc_map_click[2])
  })
  
  output$acc_data <- renderDataTable(filtered_table_accidents())
  
})
