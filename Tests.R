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





# 1/30/2019 Joins for DO rules --------------------------------------------
library(tidyverse)
library(lubridate)

#original data, called dta2
load("ShinyAllData.Rdata")
# stations list with geodata from Lesley Merrick
stations <- read_csv("TEP_StationsInfo_anna.csv")
# the DO lookup table, from Travis Pritchard
DO_lvls <- read_csv("DO_crit.csv")
# spawning dates, also from Lesley Merrick.
# dates may come in in weird format
spn <- read_csv("IRDatabase_CriteriaTables.csv")

#removes stations without a lasar id, might have to be remedied later
dta3 <- dta2 %>% 
  filter(!is.na(lasar_id))

#join the station list to the spawn codes
spawnjoin <- left_join(stations, spn, by = c("SpawnCode" = "SpawnCode"))

# change station keys to factors for join with dta3
spawnjoin$station_key <- as.factor(spawnjoin$station_key)

#join spawn dates to all of the continuous DO data
dataspawn <- left_join(dta3, spawnjoin, by = c("lasar_id" = "station_key"))

#join DO baseline criteria to above
DO_base <- left_join(dataspawn, DO_lvls, by = "DO_code")

#----
#A test case for spawn date coding, see actual case below
short <- DO_base %>% 
  select(datetime, SpawnStart, SpawnEnd, crit_Instant)

# Adapted from Travis Prichard:
# https://github.com/TravisPritchardODEQ/IR2018/blob/master/Parameters/DO/fun_DO_spawn.R
nt <- short %>% 
  mutate(SpawnStart = ifelse(!is.na(SpawnStart) & SpawnStart != "NULL", paste0(SpawnStart, "/", year(datetime) ), SpawnStart ),
         SpawnEnd = ifelse(!is.na(SpawnEnd) & SpawnEnd != "NULL", paste0(SpawnEnd, "/", year(datetime)), SpawnEnd),
         SpawnStart = mdy(SpawnStart),
         SpawnEnd = mdy(SpawnEnd),
         SpawnEnd = if_else(SpawnEnd < SpawnStart, SpawnEnd + years(1), SpawnEnd),
         in_spawn = ifelse(datetime >= SpawnStart & datetime <= SpawnEnd & !is.na(SpawnStart), 1, 0 ))
#----

# most of this is the same as Travis's code
# R doesn't like to work with day/month without year associated, so
# this is a way around that

# changed the last few lines so that if the spawning season was active, 
# the do limit would be 11, and otherwise would be the value assigned from
# the DO_code to crit_Instant (either 6.5 or 8 for grab samples, for these sites)

#the last step compares the actual DO to the DO recorded
# and assigns pass, fail, or other
# assigned other to evaluate any unexpected cases
# allowed pass for greater than OR EQUAL TO.

dtasp <- DO_base %>% 
  mutate(SpawnStart = ifelse(!is.na(SpawnStart) & SpawnStart != "NULL", paste0(SpawnStart, "/", year(datetime) ), SpawnStart ),
         SpawnEnd = ifelse(!is.na(SpawnEnd) & SpawnEnd != "NULL", paste0(SpawnEnd, "/", year(datetime)), SpawnEnd),
         SpawnStart = mdy(SpawnStart),
         SpawnEnd = mdy(SpawnEnd),
         SpawnEnd = if_else(SpawnEnd < SpawnStart, SpawnEnd + years(1), SpawnEnd),
         in_spawn = ifelse(datetime >= SpawnStart & datetime <= SpawnEnd & !is.na(SpawnStart), 1, 0 ),
         DO_lim = ifelse(in_spawn == 1, 11, crit_Instant),
         DO_status = ifelse(do < DO_lim, 0,
                            ifelse(do >= DO_lim, 1, "other")))
#results in a "failed to parse warning" - this should be ok
  
summary(as.factor(dtasp$DO_status))
# results in NAs, these items have DO sat only

d <- dtasp[1:6,]
write.csv(d, "tablesample.csv")

names(dtasp)
names(dta1)

dta1 <- dtasp %>% 
  filter(!is.na(DO_status)) %>% 
  select(-c(lasar_id, CollMethod, MonLocType, AU_ID, STATE, Reachcode, Measure, LLID, ReachRes,
            crit_30D, crit_7Mi, crit_Min, Spawn_dates, Created_Date, UserName, EcoRegion2,
            OWRD_Basin, UseNHD_LL, FishCode, SpawnCode, WaterTypeCode, WaterBodyCode,
            BacteriaCode, DO_code, ben_use_code, pH_code, DO_SpawnCode, QC_Comm,
            Conf_Score, SnapDist_ft, SnapDate, HUC10, HUC8, HUC4_Name, HUC6_Name,
            HUC12, EcoRegion3, T_R_S, Comments)) %>% 
  select(MLocID, StationDes, everything())

save(dta1, file = "dataforwqapp.RData")



# Plotly subplot situation ------------------------------------------------

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(shinyWidgets)
library(shinythemes)
library(viridis)
library(RColorBrewer)

load("dataforwqapp.Rdata")

md2 <- dta1 %>%
  filter(!is.na(datetime)) %>% 
  mutate(Site = paste(MLocID, StationDes))


ps <- md2 %>% 
  filter(grepl("11856", Site)) %>% 
  select(datetime, temp, ph, cond, do)

pp <- ps %>% 
  tidyr::gather(parameter, value, -datetime) %>% 
  transform(id = as.integer(factor(parameter)))

qs <- ps %>% 
  tidyr::gather(parameter, value, -datetime) %>% 
  transform(id = as.integer(factor(parameter))) %>%
  plot_ly(x = ~datetime,
          y = ~value,
          color = ~parameter,
          colors = "Dark2",
          yaxis = ~paste0("y", id),
          type = "scatter") %>% 
  subplot(nrows = 4, shareX = TRUE)
qs



d <- plot_ly(data = ps,
        x = ps$datetime, y = ps$do,
        type = "scatter")
c <- plot_ly(data = ps,
             x = ps$datetime, y = ps$cond,
             type = "scatter")
t <- plot_ly(data = ps,
             x = ps$datetime, y = ps$temp,
             type = "scatter")
h <- plot_ly(data = ps,
             x = ps$datetime, y = ps$ph,
             type = "scatter")
sp <- subplot(d,c,t,h, nrows = 4, shareX = TRUE)
sp %>% 
  lay



# Summary - how often is a site failing? ----------------------------------

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(shinyWidgets)
library(shinythemes)
library(viridis)
library(RColorBrewer)

load("dataforwqapp.Rdata")

md2 <- dta1 %>%
  filter(!is.na(datetime)) %>% 
  mutate(Site = paste(MLocID, StationDes))

sm <- md2 %>% 
  select(MLocID, datetime, DO_status) %>% 
  mutate(month = floor_date(datetime, "month")) %>% 
  group_by(MLocID, month) %>% 
  count(DO_status = 0)

sm2 <- md2 %>% 
  select(MLocID, datetime, DO_status) %>% 
  mutate(month = floor_date(datetime, "month")) %>% 
  group_by(MLocID, month, DO_status) %>% 
  tally()
#gives the wrong thing, still gives count of passes or fails

sm2 <- md2 %>% 
  select(MLocID, datetime, DO_status) %>% 
  mutate(month = floor_date(datetime, "month")) %>% 
  filter(DO_status == "fail") %>% 
  group_by(MLocID, month) %>% 
  tally()



# nplot dev ---------------------------------------------------------------

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(shinyWidgets)
library(shinythemes)
library(viridis)
library(RColorBrewer)

load("dataforwqapp.Rdata")
# stations <- read_csv("TEP_StationsInfo_anna.csv")
# stations$Lat <- as.numeric(stations$Lat_DD)
# stations$Long <- as.numeric(stations$Long_DD)

md2 <- dta1 %>%
  filter(!is.na(datetime)) %>% 
  mutate(Site = paste(MLocID, StationDes))

#colors
coul <- colorRampPalette(brewer.pal(9, "Set3"))(14)

  md2 %>% 
    group_by(month = floor_date(datetime, "month")) %>% 
    mutate(n_samples = n()) %>% 
    mutate(season = factor(month.abb[month(datetime)],
                           levels = c("May", "Jun", "Jul", "Aug", "Oct"))) %>% 
    plot_ly(x = ~month, y = ~n_samples, type = "bar",
            color = ~season, colors = viridis_pal()(3)) %>%
    layout(yaxis = list(title = 'Count'),
           xaxis = list(title = "Date",
                        range = c("2007-01-01", "2016-12-31")),
           barmode = 'group')

    md2 %>% 
      group_by(month = floor_date(datetime, "month")) %>% 
      mutate(n_samples = n()) %>% 
      mutate(season = factor(month.abb[month(datetime)],
                             levels = c("May", "Jun", "Jul", "Aug", "Oct"))) %>% 
      plot_ly(x = ~month, y = ~n_samples,
              color = ~season, colors = viridis_pal()(3)) %>%
      add_bars() %>%
      layout(yaxis = list(title = 'Count'),
             xaxis = list(title = "Date",
                          range = c("2007-01-01", "2016-12-31")),
             barmode = 'group')
    
    md2 %>% 
      group_by(month = floor_date(datetime, "month")) %>% 
      mutate(n_samples = n()) %>% 
      mutate(season = factor(month.abb[month(datetime)],
                             levels = c("May", "Jun", "Jul", "Aug", "Oct"))) %>% 
      plot_ly(x = ~month, y = ~n_samples, type = "bar",
              color = ~season, colors = viridis_pal()(3)) %>%
      layout(yaxis = list(title = 'Count'),
             xaxis = list(title = "Date",
                          range = c("2007-01-01", "2016-12-31")),
             barmode = 'group')
    
    
    maptabledata <- md2 %>% 
      formatDate(3, "toLocaleStrong")
    