library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(shinyWidgets)
library(shinythemes)
library(viridis)

load("ShinyAllData.Rdata")
#brings in object "dta2"
load("PracticeDataLocationInfo.Rdata")

stationInfo$MonitoringLocationIdentifier <- gsub("-ORDEQ", "", stationInfo$MonitoringLocationIdentifier)

stations <- stationInfo[,c(4:7)]
stations$Latitude <- as.numeric(stations$Latitude)
stations$Longitude <- as.numeric(stations$Longitude)

#Solves issue of lasar ids coming in as factors
#BUT MAYBE YOU WOULD LIKE THEM AS FACTORS AFTER ALL!
dta2$lasar_id <- as.numeric(levels(dta2$lasar_id)[dta2$lasar_id])
mapData <- merge(dta2, stations,
                 by.x = "lasar_id",
                 by.y = "MonitoringLocationIdentifier",
                 all.x = TRUE, all.y = FALSE)
# Added 1/22/2019
# Removes 1155 lasar id NAs
# Those IDs refer to two sites from Tillamook Estuary Partnership Sites. They need to be assigned codes.
md2 <- mapData %>%
  filter(!is.na(lasar_id)) %>% 
  filter(!is.na(datetime))


#newer version, looks like hot trash
mapData %>% 
  group_by(month = floor_date(datetime, "month")) %>% 
  mutate(n_samples = n()) %>% 
  mutate(season = factor(month.abb[month(datetime)],
                         levels = c("May", "Jun", "Jul", "Aug", "Oct"))) %>% 
  plot_ly(x = ~month, y = ~n_samples, type = "bar",
          color = ~season, colors = viridis_pal()(3),
          width = 1.5) %>% 
  layout(yaxis = list(title = 'Count'), barmode = 'group')

#older version, dots instead of bar
mapData %>% 
  group_by(month = floor_date(datetime, "month")) %>% 
  mutate(n_samples = n()) %>% 
  mutate(season = factor(month.abb[month(datetime)], levels=c("May", "Jun", "Jul", "Aug", "Oct"))) %>% 
  plot_ly(x = ~month, y = ~n_samples) %>% 
  add_markers(color = ~season, colors = "Set3", size = ~n_samples)



mapData %>%
  group_by(lasar_id, month = floor_date(datetime, "month")) %>% 
  mutate(min = min(do)) %>% 
  plot_ly(x = ~month, y = ~min, color = ~lasar_id,
          colors = viridis_pal(option = "A")(5),
          text = ~paste('Site:', MonitoringLocationName))


mapData %>%
  group_by(lasar_id, month = floor_date(datetime, "month")) %>% 
  mutate(min = min(do)) %>% 
  plot_ly(x = ~month, y = ~min, color = ~lasar_id,
          text = ~paste('Site:', MonitoringLocationName))


boxplotData <- mapData %>% 
  mutate(month = floor_date(datetime, "month")) %>%
  mutate(season = factor(month.abb[month(datetime)], levels=c("May", "Jun", "Jul", "Aug", "Oct")))
plot_ly(boxplotData, x = ~lasar_id, y = ~do, color = ~season, type = "box",
        text = ~paste('Site: ', MonitoringLocationName)) %>%
  layout(boxmode = "group")



# Remove NAs --------------------------------------------------------------

md2 %>% 
  group_by(month = floor_date(datetime, "month")) %>% 
  mutate(n_samples = n()) %>% 
  mutate(season = factor(month.abb[month(datetime)],
                         levels = c("May", "Jun", "Jul", "Aug", "Oct"))) %>% 
  plot_ly(x = ~month, y = ~n_samples, type = "bar",
          color = ~season, colors = viridis_pal()(3),
          width = 1.5) %>% 
  layout(yaxis = list(title = 'Count'), barmode = 'group')

md2 %>%
  group_by(lasar_id, month = floor_date(datetime, "month")) %>% 
  mutate(min = min(do)) %>% 
  plot_ly(x = ~month, y = ~min, color = ~lasar_id,
          colors = viridis_pal(option = "A")(5),
          text = ~paste('Site:', MonitoringLocationName)) %>% 
  layout(
    xaxis = list(range = c(min(month.p), max(month.p))))


# Active plotly version, hard coded x axis --------------------------------


wdi <- md2 %>%
  group_by(lasar_id, month.p = floor_date(datetime, "month")) %>% 
  mutate(min = min(do))

plot_ly(wdi, x = ~month.p, y = ~min, color = ~as.factor(lasar_id),
        colors = viridis_pal(option = "C")(5),
        text = ~paste('Site:', MonitoringLocationName)) %>%
  layout(
    xaxis = list(range = c("2007-01-01", "2016-12-31")))

max(wdi$month.p)

summary(wdi)
glimpse(wdi)
typeof(wdi$month)

mapData %>%
  group_by(lasar_id, month = floor_date(datetime, "month")) %>% 
  mutate(min = min(do)) %>% 
  plot_ly(x = ~month, y = ~min, color = ~lasar_id,
          text = ~paste('Site:', MonitoringLocationName))


boxplotData <- mapData %>% 
  mutate(month = floor_date(datetime, "month")) %>%
  mutate(season = factor(month.abb[month(datetime)], levels=c("May", "Jun", "Jul", "Aug", "Oct")))
plot_ly(boxplotData, x = ~lasar_id, y = ~do, color = ~season, type = "box",
        text = ~paste('Site: ', MonitoringLocationName)) %>%
  layout(boxmode = "group")
