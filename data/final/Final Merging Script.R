###################################
# ENTER LOCATION OF FILES
# Accident level data
accident_location <- "https://raw.githubusercontent.com/sergiosonline/data_sci_geo/master/data/final/automobile.csv"
weather_location <- "https://raw.githubusercontent.com/sergiosonline/data_sci_geo/master/data/final/weather.csv"
pop_proj_location <- "https://raw.githubusercontent.com/sergiosonline/data_sci_geo/master/data/toronto_hood_projections_2007-2017.csv"
###################################

library(tidyverse)
library(data.table)

accidents_raw <- fread(accident_location) %>%
  mutate(date = as.Date(date))
weather_raw <- fread(weather_location) %>%
  filter(Station == "Toronto City") %>%
  rename(date = `Date/Time`, max_temp = `Max Temp (°C)`, min_temp = `Min Temp (°C)`, 
         tot_rain_mm = `Total Rain (mm)`,  tot_snow_cm = `Total Snow (cm)`,
         tot_precip_mm = `Total Precip (mm)`, ground_snow_cm = `Snow on Grnd (cm)`) %>%
  select(date, max_temp, min_temp, tot_rain_mm, tot_snow_cm, tot_precip_mm, ground_snow_cm) %>%
  mutate(date = as.Date(date))
pop_raw <- fread(pop_proj_location) %>%
  select(HoodID, starts_with("Pop2")) %>%
  gather(Year, Population, -HoodID) %>%
  mutate(Year = as.numeric(gsub("Pop", "", Year)))

accidents <- left_join(accidents_raw, weather_raw, by = c("date" = "date")) %>%
  mutate(year = year(date)) %>%
  left_join(pop_raw, by = c("hood_num" = "HoodID", "year" = "Year")) %>%
  select(-year) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

fwrite(accidents, file = "C:/Users/angel/Downloads/accidents.csv")
