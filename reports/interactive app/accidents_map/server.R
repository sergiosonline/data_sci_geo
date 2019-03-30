library(shiny)

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
      filter(hour >= input$acc_time[1] & hour <= input$acc_time[2]) %>%
      filter(hood_name %in% input$hood) %>%
      filter(population >= input$population[1] & population <= input$population[2])
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
  
  # Pop-up label for accident
  labs_acc <- reactive({lapply(seq(nrow(filtered_plot_accidents())), function(i) {
    paste0('<b>', filtered_plot_accidents()[i, "acc_class"], "</b><br/>",
           filtered_plot_accidents()[i, "date"], '<br/>',
           filtered_plot_accidents()[i, "street1"], ', ', 
           filtered_plot_accidents()[i, "street2"],'<br/>',
           "Parties involved: ", filtered_plot_accidents()[i, "parties_involved"])})
  })
  
  # Pop-up label for neighborhood
  labs_hood <- paste0('<b>', gsub(" *\\(.*?\\) *", "", neighborhoods$AREA_NAME), '</b><br/>',
                      'Population: ', neighborhoods$X2016pop)
  
  # Leaflet map
  output$acc_map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addCircles(
        data = filtered_plot_accidents(),
        lng = ~ long, lat = ~ lat,
        color = ~ pal2(acc_class),
        label = lapply(labs_acc(), HTML)
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
      )%>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  # Overlays the population data on map
  observe({
    if(input$pop_label){
      leafletProxy("acc_map") %>%
        clearShapes() %>%
        addPolygons(
          data = neighborhoods[3],
          color = ~pal_pop2(X2016pop),
          fillOpacity = 0.5,
          weight = 1,
          highlight = highlightOptions(
            weight = 2,
            color = "#666"
          ),
          label = lapply(labs_hood, HTML)
        )%>%
        addCircles(
          data = filtered_plot_accidents(),
          lng = ~ long, lat = ~ lat,
          color = ~ pal2(acc_class),
          label = lapply(labs_acc(), HTML)
        )
    }
    else if(!input$pop_label){
      leafletProxy("acc_map") %>%
        clearShapes() %>%
        addCircles(
          data = filtered_plot_accidents(),
          lng = ~ long, lat = ~ lat,
          color = ~ pal2(acc_class),
          label = lapply(labs_acc(), HTML)
        )
    }
  })
  
  # Allows the data to be filtered by accident clicked
  filtered_table_accidents <- reactive({
    if(is.null(input$acc_map_click))
      return(filtered_accidents())
    filtered_accidents() %>%
      filter(lat == input$acc_map_click[1] &
               long == input$acc_map_click[2])
  })
  
  output$acc_data <- renderDataTable(filtered_table_accidents())
  
  output$acc_plot <- renderPlot({
    filtered_accidents() %>%
      mutate(month = as.yearmon(date)) %>%
      group_by(month, acc_class, accident_key) %>%
      summarize(num_accidents = n()) %>%
      ungroup() %>%
      group_by(month, acc_class) %>%
      summarize(num_accidents = sum(num_accidents, na.rm = T)) %>%
      ggplot(., aes(x = month, y = num_accidents, col = acc_class, group = acc_class)) + 
      geom_point() + geom_line()
  })
  
})
