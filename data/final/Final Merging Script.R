###################################
# ENTER LOCATION OF FILES
# Accident level data
accident_location <- "https://raw.githubusercontent.com/sergiosonline/data_sci_geo/master/data/final/automobile.csv"
weather_location <- "https://raw.githubusercontent.com/sergiosonline/data_sci_geo/master/data/final/weather.csv"
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

accidents <- left_join(accidents_raw, weather_raw, by = c("date" = "date")) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

# fwrite(accidents, file = "C:/Users/angel/Downloads/accidents.csv")
