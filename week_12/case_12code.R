library(tidyverse)
library(htmlwidgets)
library(widgetframe)
library(dygraphs)


library(xts)
library(openmeteo)


d<- weather_history(c(43.00923265935055, -78.78494250958327),start = "2023-01-01",end=today(),
                    daily=list("temperature_2m_max","temperature_2m_min","precipitation_sum")) %>% 
  mutate(daily_temperature_2m_mean=(daily_temperature_2m_max+daily_temperature_2m_min)/2)

dtemp <- select(d, daily_precipitation_sum)

daily_max_temp <- xts(dtemp, order.by = d$date)

dygraph(daily_max_temp, main = "Daily Precipitation Sum") %>%
  dyRangeSelector(dateWindow = c("2023-01-01", "2024-10-31"))
