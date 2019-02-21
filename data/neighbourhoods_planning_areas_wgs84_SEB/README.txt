Combination of 2016 census data and Toronto neighborhood polygons


Created with the below in R:

library(tibble); library(sp); library(dplyr)
population <- read.csv("~/Desktop/Grad_School/COURSEWORK/Spring 2019/Data Science/rough work/Wellbeing_TO_2016 Census_Total Pop_Total Change_Age Groups.csv",stringsAsFactors = FALSE,header=T)
require(sf)
shape <- read_sf(dsn = "~/Desktop/Grad_School/COURSEWORK/Spring 2019/Data Science/rough work/neighbourhoods_planning_areas_wgs84/.", layer = "NEIGHBORHOODS_WGS84")

neighborhoods <- shape

neighborhoods <- add_column(neighborhoods, '2016pop'=NA, 'x_coords' = NA, 'y_coords' = NA)

# Separating X and Y coordinates from polygon
for (hood in neighborhoods$AREA_NAME) {
  ## Adding population
  pop = as.numeric(neighborhoods[neighborhoods$AREA_NAME == hood,][["AREA_S_CD"]])
  neighborhoods[neighborhoods$AREA_NAME == hood,]$'2016pop' = 
    population[population$HoodID == pop,]$Pop2016
  ## Adding x-y
  temp = unlist(subset(neighborhoods,AREA_NAME == hood)$geometry[[1]])
  ll = length(temp)
  x_coord = list(temp[1:(ll/2)])
  y_coord = list(temp[((ll/2)+1):ll])
  neighborhoods[neighborhoods$AREA_NAME == hood,]$x_coords = x_coord
  neighborhoods[neighborhoods$AREA_NAME == hood,]$y_coords = y_coord
}

st_write(neighborhoods,"~/Desktop/Grad_School/COURSEWORK/Spring 2019/Data Science/rough work/neighbourhoods_planning_areas_wgs84_SEB/NEIGHBORHOODS_WGS84.shp",
         , delete_layer = TRUE)
         
#Checking if point is in given polygon
#point.in.polygon(43.66174538, -79.4268902, unlist(neighborhoods[66,]$y_coords),
#                 unlist(neighborhoods[66,]$x_coords), mode.checked=FALSE)


#Bloor and Condord (Bloor-Ossington) - Check with neighborhoods[66,] "Palmerston..." -> 1 "contained"
#-79.42689029, 43.66174538

