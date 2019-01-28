library(readr)
library(dplyr)

stationsuses <- read_csv("stationsuses.csv")

lasarid <- data.frame(unique(dta2$lasar_id))
names(lasarid) <- "lasar_id"
write.csv(lasarid, "lasarids.csv")

NA_lasar <- dta2 %>% 
  filter(is.na(lasar_id))

load("ShinyAllData.Rdata")
#brings in object "dta2"
load("PracticeDataLocationInfo.Rdata")
#brings in object "stationInfo"

stationInfo$MonitoringLocationIdentifier <- gsub("-ORDEQ", "", stationInfo$MonitoringLocationIdentifier)
stationInfo <- stationInfo %>% 
  select(MonitoringLocationIdentifier, Latitude, Longitude, MonitoringLocationName)
stationInfo$Latitude1 <- as.numeric(stationInfo$Latitude)
stationInfo$Longitude1 <- as.numeric(stationInfo$Longitude)

# filters missing LASAR ID, for now
dta3 <- dta2 %>% 
  filter(!is.na(lasar_id))

#this step filters all of the station ids that arent the 5 digit LASAR type
#visually inspected data to make sure no LASAR IDs were longer than 5
stationsuses1 <- stationsuses %>% 
  filter(nchar(Station_ID) < 6) %>% 
  select(-Comments) %>% 
  rename(Latitude2 = DECIMAL_LAT) %>% 
  rename(Longitude2 = DECIMAL_LONG)


# first join, simple list
mapData1 <- merge(dta3, stationInfo,
                  by.x = "lasar_id",
                  by.y = "MonitoringLocationIdentifier",
                  all.x = TRUE)

mapData.s <- merge(dta3, stationsuses1,
                  by.x = "lasar_id",
                  by.y = "Station_ID",
                  all.x = TRUE, all.y = FALSE) %>% 
  select(lasar_id, DO_use) %>% 
  unique()




mapData2 <- merge(mapData1, stationsuses1,
                 by.x = "lasar_id",
                 by.y = "Station_ID",
                 all.x = TRUE, all.y = FALSE)

m1 <- mapData2 %>% 
  filter(MonitoringLocationName != Station_Description)
#all of the names match, so this can be removed


submap <- mapData2 %>% 
  select(lasar_id, Latitude1, Latitude2, Longitude1, Longitude2) %>%
  arrange(!is.na(Latitude2), Latitude2)
# submap <- mapData2 %>% 
#   select(lasar_id, Latitude.x, Latitude.y, Longitude.x, Longitude.y) %>%
#   # filter(Latitude.x != Latitude.y) %>% 
#   arrange(!is.na(Latitude.y), Latitude.y)

submap.x <- mapData2 %>% 
  select(lasar_id, Latitude.x, Latitude.y, Longitude.x, Longitude.y) %>%
  # filter(Latitude.x != Latitude.y) %>% 
  arrange(!is.na(Latitude.x), Latitude.x)

dtasummary <- unique(submap)
write_csv(dtasummary, "summary.csv")

summary(submap)

leaflet() %>%
  addProviderTiles("Esri.WorldImagery", group = "Esri Satellite Map") %>%
  addMarkers(data = mapData,
             ~DECIMAL_LONG, 
             ~DECIMAL_LAT,
label = ~paste0("Station ID: ", lasar_id, "
                                 Site Name: ", Station_Description),
             labelOptions = labelOptions(textOnly = FALSE),
             layerId = mapData$lasar_id)
