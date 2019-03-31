library(tidyverse, quietly = T, warn.conflicts = F, verbose = F)
library(data.table, quietly = T, warn.conflicts = F, verbose = F)
library(sp)
library(htmltools)
library(leaflet)
library(zoo)

# Load neighborhood polygons with 2016 population
## Download this file into local drive: 
## https://github.com/sergiosonline/data_sci_geo/tree/master/data/neighbourhoods_planning_areas_wgs84_SEB
## And update fileloc variable

fileloc <- "C:/Users/angel/OneDrive/Documents/GitHub/data_sci_geo/data/neighbourhoods_planning_areas_wgs84_SEB"
#fileloc <- "~/Documents/Github/data_sci_geo/data/neighbourhoods_planning_areas_wgs84_SEB/"
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

# 2016 Population for each neighborhood
population <- data.frame(neighborhood = as.numeric(neighborhoods$AREA_S_CD),
                         population = as.numeric(neighborhoods$X2016pop))

# Combine with per accident information
accidents <- accidents %>%
  left_join(per_accident, by = c("accident_key" = "accident_key")) %>%
  left_join(population, by = c("hood_num" = "neighborhood")) %>%
  select(-division, -ward_num, -hood_num)

rm(per_accident, population)