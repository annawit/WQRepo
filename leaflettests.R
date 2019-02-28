library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(lubridate)
library(shinyWidgets)
library(shinythemes)
library(viridis)
library(RColorBrewer)
library(rgdal)


load("dataforwqapp.Rdata")

md2 <- dta1 %>% 
  mutate(DO_status = ifelse(DO_status == 0, "Below limit", "Above limit"))

load("sitesummary.Rdata")
sd <- sites

#colors
coul <- colorRampPalette(brewer.pal(9, "Set3"))(14)

map <- leaflet() %>%
    addProviderTiles("Esri.WorldImagery", group = "Esri Satellite Map") %>%
    addMarkers(data = sd,
               lat = ~Lat_DD,
               lng = ~Long_DD,
               label = ~paste0("Station ID: ", MLocID, "
                                 Site Name: ", StationDes),
               labelOptions = labelOptions(textOnly = FALSE),
               layerId = sd$MLocID) %>%
    addLayersControl(baseGroups = c("Esri Satellite Map"))
