###################################
# ENTER LOCATION OF FILES
# Accident level data
accident_location <- "C:/Users/angel/Downloads/by_accident_7Mar.csv"
weather_location <- "C:/Users/angel/Downloads/weather.csv"
###################################

library(tidyverse)
library(data.table)

accidents_raw1 <- read.csv(accident_location, check.names = F) %>%
  select(-`Index_`) %>%
  distinct() %>% 
  group_by(YEAR, MONTH, Hood_ID) %>%
  summarize_at(.vars = vars(-YEAR, -MONTH, -Hood_ID, -DAY, -ACCNUM, -LATITUDE, -LONGITUDE, -Ward_ID, -Max_Temp, 
                            -Min_Temp, -Daily_dif, -Ave_Temp, -Rain_vol, -Snow_vol, -Geohash, 
                            -starts_with("Percent"), -starts_with("Avg")),
               .funs = funs(sum(., na.rm = T))) 
accidents_raw2 <- read.csv(accident_location, check.names = F) %>%
  select(-`Index_`) %>%
  distinct() %>% 
  group_by(YEAR, MONTH, Hood_ID) %>%
  filter_at(vars(starts_with("Percent"), starts_with("Avg")),
            any_vars(. != 0)) %>%
  summarize_at(.vars = vars(starts_with("Percent"), starts_with("Avg")),
               .funs = funs(mean(., na.rm = T)))

accidents_raw <- left_join(accidents_raw1, accidents_raw2, by = c("YEAR", "MONTH", "Hood_ID"))

weather_raw <- read.csv(weather_location, check.names = F) %>%
  select(Year, Month, Station, `Max Temp (°C)`, `Min Temp (°C)`, `Total Rain (mm)`, `Total Snow (cm)`,
          `Total Precip (mm)`, `Total Rain (mm)`, `Total Snow (cm)`, `Snow on Grnd (cm)`) %>%
  filter(Station == "Toronto City") %>%
  filter(Year >= 2007) %>%
  group_by(Year, Month) %>%
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

accidents <- data.frame(YEAR = rep(2007:2017, each = 140*12),
                        MONTH = rep(1:12, each = 140*11),
                        Hood_ID = rep(1:140, times = 11*12)) %>%
  left_join(accidents_raw, by = c("YEAR", "MONTH", "Hood_ID")) %>%
  left_join(weather_raw, by = c("YEAR" = "Year", "MONTH" = "Month"))

accidents[is.na(accidents)] <- 0
accidents[accidents == -Inf] <- 0
accidents[accidents == NaN] <- 0