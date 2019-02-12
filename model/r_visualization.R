library(leaflet)
library(htmltools)

hda <- read.csv(file="hda.csv", header=TRUE, sep=",")
incidents <- read.csv(file="Automobile.csv", header=TRUE, sep=",")

# result = apply(incidents, 1, function(row_data) {
#   nrow(
#     hda[
#       hda$Latitude_SW <= as.numeric(row_data['LATITUDE'])
#       & as.numeric(row_data['LATITUDE']) <= hda$Latitude_NE
#       & hda$Longitude_SW <= as.numeric(row_data['LONGITUDE'])
#       & as.numeric(row_data['LONGITUDE']) <= hda$Longitude_NE
#       ,
#     ]
#   )
# })
# 
# incidents_in_hda <- hda[result>0,]

pal <- colorNumeric(c('#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026','#000000'), (0:2)/10)

m <- leaflet() %>% addTiles() %>%
  addRectangles(
    data = hda,
    lng1= ~ Longitude_NE, lat1= ~ Latitude_NE,
    lng2= ~ Longitude_SW, lat2= ~ Latitude_SW,
    fillColor = ~ pal(SeverityScore), fillOpacity = 0.8,
    stroke = FALSE,
    popup = ~ SeverityScore
  ) %>%
  addCircles(
    data = incidents,
    lng = ~ LONGITUDE, lat = ~ LATITUDE
  )
