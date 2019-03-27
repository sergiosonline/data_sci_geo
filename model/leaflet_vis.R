library(leaflet)
library(htmltools)
library(dplyr)
library(sp)

accidents <- read.csv("https://raw.githubusercontent.com/sergiosonline/data_sci_geo/master/data/accidents.csv",header=T, stringsAsFactors = FALSE)
neighborhoods = rgdal::readOGR(dsn = "~/Documents/Github/data_sci_geo/data/neighbourhoods_planning_areas_wgs84_SEB/", layer = "NEIGHBORHOODS_WGS84")

pal2 <- colorFactor(palette = c('darkorchid', 'darkturquoise'), domain = accidents$ACCLASS)
pal_pop <- colorFactor(palette = c('steel blue','indigo'), domain = neighborhoods$X2016pop)
pal_pop2 <- colorNumeric(palette = hsv(1, seq(0,1,length.out = 12) , 1), neighborhoods$X2016pop, n = 5)
labs <- lapply(seq(nrow(accidents)), function(i) {
  paste0( '<p>', accidents[i, "DATE"], '<p></p>', 
          accidents[i, "STREET1"], ', ', 
          accidents[i, "STREET2"],'</p><p>', 
          accidents[i, "ACCLASS"], '</p>' ) 
})

m <- leaflet() %>% addTiles() %>%

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
    
  )

