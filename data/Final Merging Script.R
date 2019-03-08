library(tidyverse)
library(data.table)

accident_raw <- read.csv("https://raw.githubusercontent.com/sergiosonline/data_sci_geo/master/data/KSI.csv",
                         check.names = F) %>%
  mutate(DATE = as.Date(substr(DATE, 1, nchar(as.character(DATE)) - 13), format = "%Y-%m-%d")) %>%
  rename(longitude = `ï»¿X`, latitude = Y) %>%
  select(-Hood_Name)

weather_raw <- read.csv("https://raw.githubusercontent.com/sergiosonline/data_sci_geo/master/data/weather.csv", 
                        check.names = F) %>%
  select(`Date/Time`, Year, Month, Station, `Max Temp (°C)`, `Min Temp (°C)`, `Total Rain (mm)`, 
         `Total Snow (cm)`, `Total Precip (mm)`, `Total Rain (mm)`, `Total Snow (cm)`, `Snow on Grnd (cm)`) %>%
  filter(Station == "Toronto City") %>%
  filter(Year >= 2007) %>%
  mutate(`Date/Time` = as.Date(`Date/Time`)) %>%
  group_by(`Date/Time`) %>%
  summarize(`Max_temp` = max(`Max Temp (°C)`, na.rm = T),
            `Min_temp` = min(`Min Temp (°C)`, na.rm = T),
            `Tot_rain` = sum(`Total Rain (mm)`, na.rm = T),
            `Tot_snow` = sum(`Total Snow (cm)`, na.rm = T),
            `Tot_precip` = sum(`Total Precip (mm)`, na.rm = T),
            `Avg_rain` = mean(`Total Rain (mm)`, na.rm = T),
            `Avg_snow` = mean(`Total Snow (cm)`, na.rm = T),
            `Avg_precip` = mean(`Total Precip (mm)`, na.rm = T),
            `Max_rain` = max(`Total Rain (mm)`, na.rm = T),
            `Max_snow` = max(`Total Snow (cm)`, na.rm = T),
            `Max_precip` = max(`Total Precip (mm)`, na.rm = T),
            `Max_ground_snow` = max(`Snow on Grnd (cm)`, na.rm = T))

pop_raw <- read.csv("https://raw.githubusercontent.com/sergiosonline/data_sci_geo/master/data/toronto_hood_population_projections_2007-2017.csv",
                    check.names = F)
pop_raw <- pop_raw[-1] %>%
  select(-PopChg11t16, -YearlyGrowthRate) %>%
  gather(Year, Population, -HoodID, -Neighbourhood) %>%
  filter(Year != "Neighbourhood") %>%
  mutate(Year = as.numeric(gsub("Pop", "", Year)))

accidents <- accident_raw %>%
  left_join(weather_raw, by = c("DATE" = "Date/Time")) %>%
  left_join(pop_raw, by = c("YEAR" = "Year", "Hood_ID" = "HoodID"))

accidents[is.na(accidents)] <- 0
accidents[accidents == -Inf] <- 0
accidents[accidents == Inf] <- 0
accidents[accidents == NaN] <- 0

rm(accident_raw, weather_raw, pop_raw)