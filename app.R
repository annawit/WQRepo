##install.packages("shiny"); install.packages("shinythemes"); install.packages("shinyWidgets")
##install.packages("leaflet"); install.packages("leaflet.extras")
##install.packages("plotly"); install.packages("ggplot2")
##install.packages("viridis")
##install.packages("RColorBrewer")
library(shiny); library(shinythemes); library(shinyWidgets)
library(leaflet); library(leaflet.extras)
library(plotly);library(ggplot2)
library(viridis)
library(RColorBrewer)
library(tools); library(tidyr)
library(tidyverse); library(lubridate); library(dplyr)

load("finaldata.RData")

# 1. UI ----
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage("Continuous Dissolved Oxygen Visualizer",
             
             # 1.1 Select from map ----
             tabPanel("Select from map",
                      
                      sidebarLayout(position = "right",
                                    
                                    sidebarPanel(
                                      style = "background: white",
                                      
                                      # __1.1.1 Station summary text and table ----
                                      wellPanel(
                                        uiOutput("youhavechosen"),
                                        hr(),
                                        plotlyOutput("samplect") # sample counts
                                      ),
                                      
                                      # __1.1.2 Table columns selector ----
                                      wellPanel(
                                        checkboxGroupButtons(
                                          inputId = "tablecolumnselector",
                                          label = "Select table columns to display:",
                                          choices = names(dta[,c(1:19,22:25)]),
                                          selected = c("Station Description",
                                                       "Sample Time",
                                                       "Dissolved Oxygen (mg/L)",
                                                       "Dissolved Oxygen Saturation (%)",
                                                       "DO Status")
                                        )
                                      )
                                    ),
                                    # __1.1.3 Leaflet map and data table ----
                                    mainPanel(
                                      wellPanel(
                                        tags$style(type = "text/css", "#map {height: calc(80vh - 80px) !important;}"),
                                        leafletOutput("map")),
                                      wellPanel(
                                        DT::dataTableOutput("table"),
                                        style = "background: white")
                                    )
                      )
             ),
             
             
             
             
             
             # 1.2 Overview plots ----
             tabPanel("Overview Plots",
                      
                      tabsetPanel(
                        # __1.2.1 Sample Summary ----
                        tabPanel("Sample Summary",
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     width = 3,
                                     wellPanel(
                                       selectInput(inputId = "plottype",
                                                   "Select:",
                                                   choices = c("By Site" = "MLocID",
                                                               "By Time" = "month",
                                                               "By Spawning" = "Spawning",
                                                               "By DO Criteria" = "DO Limit")
                                       )
                                     ),
                                     wellPanel(
                                       tableOutput("site")
                                       
                                     )
                                   ),
                                   mainPanel(
                                     wellPanel(
                                       HTML(paste(tags$h5("Click and drag in the figure to zoom. Double click to zoom out."))),
                                       style = "background: white"
                                     ),
                                     wellPanel(
                                       plotlyOutput("summaryplot", height = 800),
                                       style = "background: white"
                                     )
                                   )
                                 )
                        ),
                        
                        # __1.2.2 Minimum Dissolved Oxygen ----
                        tabPanel("Minimum Dissolved Oxygen",
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     width = 3,
                                     
                                     checkboxGroupInput(
                                       inputId = "estuary_site",
                                       label = "Select Estuary Sites:",
                                       choices = c("10523-ORDEQ   Nestucca R at Cloverdale",
                                                   "11856-ORDEQ   Nehalem River at Foley Road (Roy Creek Campground)",
                                                   "13421-ORDEQ   Wilson River at Hwy 101",
                                                   "13428-ORDEQ   Dougherty Slough at Hwy 101",
                                                   "13429-ORDEQ   Dougherty Slough at Wilson River Loop Road (Tillamook)",
                                                   "13430-ORDEQ   Hoquarten Slough at Hwy 101 (Tillamook)",
                                                   "13431-ORDEQ   Trask River at Netarts Road (Hwy. 6)",
                                                   "34440-ORDEQ   Hall Slough at Goodspeed Road (Tillamook, OR)"),
                                       selected = c("10523-ORDEQ   Nestucca R at Cloverdale",
                                                    "11856-ORDEQ   Nehalem River at Foley Road (Roy Creek Campground)")
                                     ),
                                     
                                     checkboxGroupInput(
                                       inputId = "river_site",
                                       label = "Select River/Stream Sites:",
                                       choices = c("13368-ORDEQ   Nehalem River at River Mile 15.0",
                                                   "21800-ORDEQ   Nestucca River at River Mile 38.57",
                                                   "22394-ORDEQ   Nestucca River at first bridge ramp (upstream of Beaver)",
                                                   "23509-ORDEQ   Nehalem River downstream of Humbug Creek at Lower Nehalem Road",
                                                   "29292-ORDEQ   Nehalem River at Salmonberry River",
                                                   "29302-ORDEQ   Nehalem River at Spruce Run Creek"),
                                       selected = c("13368-ORDEQ   Nehalem River at River Mile 15.0",
                                                    "21800-ORDEQ   Nestucca River at River Mile 38.57")
                                     )
                                     
                                   ),
                                   
                                   mainPanel(
                                     
                                     tabsetPanel(type = "tabs",
                                                 
                                                 tabPanel("Min DO (mg/L)",
                                                          wellPanel(
                                                            plotlyOutput("mindoplot_e", height = 500),
                                                            
                                                            br(),
                                                            br(),
                                                            
                                                            plotlyOutput("mindoplot_r", height = 500),
                                                            
                                                            style = "background:white")
                                                 ),
                                                 
                                                 tabPanel("Min DO Saturation (%)",
                                                          wellPanel(
                                                            plotlyOutput("mindosplot_e", height = 500),
                                                            
                                                            br(),
                                                            br(),
                                                            
                                                            plotlyOutput("mindosplot_r", height = 500),
                                                            
                                                            style = "background: white")
                                                 )
                                                 
                                     )
                                   )
                                 )
                        ),
                        
                        # __1.2.3 Boxplots ----
                        tabPanel("Boxplots",
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     width = 3,
                                     selectInput("boxplot",
                                                 "Select x-axis:",
                                                 choices = c(
                                                   "Site" = "MLocID",
                                                   "Season" = "season",
                                                   "Year" = "year"
                                                 )
                                     ),
                                     
                                     selectInput("boxplotgrp",
                                                 "Select color:",
                                                 choices = c(
                                                   "Site" = "MLocID",
                                                   "Season" = "season",
                                                   "Year" = "year"
                                                 )
                                     ),
                                     
                                     pickerInput("boxplotsites",
                                                 "Select sites:",
                                                 choices = list(
                                                   Estuarine = 
                                                     c("10523-ORDEQ   Nestucca R at Cloverdale",
                                                       "11856-ORDEQ   Nehalem River at Foley Road (Roy Creek Campground)",
                                                       "13421-ORDEQ   Wilson River at Hwy 101",
                                                       "13428-ORDEQ   Dougherty Slough at Hwy 101",
                                                       "13429-ORDEQ   Dougherty Slough at Wilson River Loop Road (Tillamook)",
                                                       "13430-ORDEQ   Hoquarten Slough at Hwy 101 (Tillamook)",
                                                       "13431-ORDEQ   Trask River at Netarts Road (Hwy. 6)",
                                                       "34440-ORDEQ   Hall Slough at Goodspeed Road (Tillamook, OR)"), 
                                                   "River/Stream" = 
                                                     c("13368-ORDEQ   Nehalem River at River Mile 15.0",
                                                       "21800-ORDEQ   Nestucca River at River Mile 38.57",
                                                       "22394-ORDEQ   Nestucca River at first bridge ramp (upstream of Beaver)",
                                                       "23509-ORDEQ   Nehalem River downstream of Humbug Creek at Lower Nehalem Road",
                                                       "29292-ORDEQ   Nehalem River at Salmonberry River",
                                                       "29302-ORDEQ   Nehalem River at Spruce Run Creek")
                                                 ),
                                                 selected = c("10523-ORDEQ   Nestucca R at Cloverdale",
                                                              "13368-ORDEQ   Nehalem River at River Mile 15.0"),
                                                 multiple = TRUE
                                     )),
                                   
                                   mainPanel(
                                     width = 9,
                                     wellPanel(
                                       style = "background: white",
                                       plotlyOutput("summaryboxplot"),
                                       br(),
                                       br(),
                                       plotlyOutput("summaryboxplot_s")
                                     )
                                   )
                                 )
                        )
                      )
             ),
             
             
             
             # 1.3 Display continuous data ----
             tabPanel("Display Continuous Data",
                      fluidPage(
                        tabsetPanel(
                          
                          # __1.3.1 Plots ----
                          tabPanel("Plots",
                                   sidebarLayout(
                                     sidebarPanel(
                                       width = 3,
                                       pickerInput(inputId = "station_selection", 
                                                   label = h3("Select sites:"), 
                                                   choices = list(
                                                     Estuarine = 
                                                       c("10523-ORDEQ   Nestucca R at Cloverdale",
                                                         "11856-ORDEQ   Nehalem River at Foley Road (Roy Creek Campground)",
                                                         "13421-ORDEQ   Wilson River at Hwy 101",
                                                         "13428-ORDEQ   Dougherty Slough at Hwy 101",
                                                         "13429-ORDEQ   Dougherty Slough at Wilson River Loop Road (Tillamook)",
                                                         "13430-ORDEQ   Hoquarten Slough at Hwy 101 (Tillamook)",
                                                         "13431-ORDEQ   Trask River at Netarts Road (Hwy. 6)",
                                                         "34440-ORDEQ   Hall Slough at Goodspeed Road (Tillamook, OR)"), 
                                                     "River/Stream" = 
                                                       c("13368-ORDEQ   Nehalem River at River Mile 15.0",
                                                         "21800-ORDEQ   Nestucca River at River Mile 38.57",
                                                         "22394-ORDEQ   Nestucca River at first bridge ramp (upstream of Beaver)",
                                                         "23509-ORDEQ   Nehalem River downstream of Humbug Creek at Lower Nehalem Road",
                                                         "29292-ORDEQ   Nehalem River at Salmonberry River",
                                                         "29302-ORDEQ   Nehalem River at Spruce Run Creek")
                                                   ),
                                                   selected = c("34440-ORDEQ   Hall Slough at Goodspeed Road (Tillamook, OR)"),
                                                   multiple = FALSE
                                       ),
                                       
                                       selectInput(
                                         inputId = "x",
                                         label = "X-axis",
                                         choices = c("Sample Time" = "Sample Time", # "selectInput item" = "data variable name" (both need to be "" no matter it is long name or short name)
                                                     "Temperature (\u00B0C)" = "Temperature (?C)", 
                                                     "Temperature Grade" = "Temperature Grade", 
                                                     "pH" = "pH", 
                                                     "pH Grade" = "pH Grade",
                                                     "Specific Conductivity (\u03BCS)" = "Specific Conductivity (?S)", 
                                                     "Specific Conductivity Grade" = "Specific Conductivity Grade", 
                                                     "Dissolved Oxygen Grade" = "Dissolved Oxygen Grade",
                                                     "Dissolved Oxygen Saturation (%)" = "Dissolved Oxygen Saturation (%)", 
                                                     "Dissolved Oxygen Saturation Grade" = "Dissolved Oxygen Saturation Grade", 
                                                     "Data Source" = "Data Source"),
                                         selected = "Sample Time" # "data variable name"
                                       ),
                                       
                                       selectInput(
                                         inputId = "y2",
                                         label = "2nd y-axis",
                                         choices = c("Sample Time" = "Sample Time", 
                                                     "Temperature (\u00B0C)" = "Temperature (?C)", 
                                                     "Temperature Grade" = "Temperature Grade", 
                                                     "pH" = "pH", 
                                                     "pH Grade" = "pH Grade",
                                                     "Specific Conductivity (\u03BCS)" = "Specific Conductivity (?S)", 
                                                     "Specific Conductivity Grade" = "Specific Conductivity Grade", 
                                                     "Dissolved Oxygen Grade" = "Dissolved Oxygen Grade",
                                                     "Dissolved Oxygen Saturation (%)" = "Dissolved Oxygen Saturation (%)", 
                                                     "Dissolved Oxygen Saturation Grade" = "Dissolved Oxygen Saturation Grade", 
                                                     "Data Source" = "Data Source"),
                                         selected = "Temperature (\u00B0C)"
                                       ),
                                       
                                       selectInput(
                                         inputId = "y3",
                                         label = "3rd y-axis",
                                         choices = c("Sample Time" = "Sample Time", 
                                                     "Temperature (\u00B0C)" = "Temperature (?C)", 
                                                     "Temperature Grade" = "Temperature Grade", 
                                                     "pH" = "pH", 
                                                     "pH Grade" = "pH Grade",
                                                     "Specific Conductivity (\u03BCS)" = "Specific Conductivity (?S)", 
                                                     "Specific Conductivity Grade" = "Specific Conductivity Grade", 
                                                     "Dissolved Oxygen Grade" = "Dissolved Oxygen Grade",
                                                     "Dissolved Oxygen Saturation (%)" = "Dissolved Oxygen Saturation (%)", 
                                                     "Dissolved Oxygen Saturation Grade" = "Dissolved Oxygen Saturation Grade", 
                                                     "Data Source" = "Data Source"),
                                         selected = "Specific Conductivity (?S)"
                                       ),
                                       
                                       selectInput(
                                         inputId = "y4",
                                         label = "4th y-axis",
                                         choices = c("Sample Time" = "Sample Time", 
                                                     "Temperature (\u00B0C)" = "Temperature (?C)", 
                                                     "Temperature Grade" = "Temperature Grade", 
                                                     "pH" = "pH", 
                                                     "pH Grade" = "pH Grade",
                                                     "Specific Conductivity (\u03BCS)" = "Specific Conductivity (?S)", 
                                                     "Specific Conductivity Grade" = "Specific Conductivity Grade", 
                                                     "Dissolved Oxygen Grade" = "Dissolved Oxygen Grade",
                                                     "Dissolved Oxygen Saturation (%)" = "Dissolved Oxygen Saturation (%)", 
                                                     "Dissolved Oxygen Saturation Grade" = "Dissolved Oxygen Saturation Grade", 
                                                     "Data Source" = "Data Source"),
                                         selected = "pH"
                                       ),
                                       
                                       dateRangeInput(
                                         inputId = "daterange",
                                         label = "Select dates:",
                                         start = min(dta1$Date),
                                         end = max(dta1$Date),
                                         min = min(dta1$Date),
                                         max = max(dta1$Date),
                                         separator = "to",
                                         format = "yyyy-mm-dd",
                                         startview = "year",
                                         weekstart = 0
                                       )
                                       
                                     ),
                                     
                                     mainPanel(
                                       plotlyOutput("subplot", height = 800)
                                     )
                                   )
                          ),
                          # __1.3.2 Table ----
                          tabPanel("Table",
                                   wellPanel(
                                     h3("Table of Plot Data"),
                                     DT::dataTableOutput("plottable"),
                                     style = "background: white"
                                   )
                          )
                        )
                        
                      )
             )
             
  )
) # UI END ----

# 2. SERVER ----

server <- function(input,output,session){
  
  # 2.1 Leaflet map ----
  pctcolor <- colorFactor(palette = c("blue","green","orange","red"), domain = meets$pctbin)
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Esri.WorldImagery",group = "Esir Satellite Map") %>% 
      addProviderTiles("OpenTopoMap", group = "Open Topo Map") %>% 
      addProviderTiles("CartoDB", group = "Carto Map") %>% 
      addLayersControl(baseGroups = c("Esir Satellite Map","Open Topo Map","Carto Map")) %>% 
      addCircleMarkers(data = s,
                       lat = ~ Lat,
                       lng = ~ Long,
                       color = ~ pctcolor(pctbin),
                       radius = ~ ((n.x)^(3/9)),
                       fillOpacity = 0.5,
                       label = ~ paste0("Station ID:", MLocID, " ", `Station Description`),
                       labelOptions = labelOptions(textOnly = FALSE),
                       layerId = s$`Station Description`) %>% 
      addLegend("bottomright",
                pal = pctcolor,
                values = meets$pctbin,
                title = "Note:<br>Piont/Site size represents sample size<br><br>Color Legend") %>% 
      addResetMapButton() %>% 
      addFullscreenControl(pseudoFullscreen = TRUE)
    
  })
  
  # 2.2 Map table and info ----
  maptable <- reactive({
    dta %>%
      filter(MLocID == input$map_marker_click$id) %>% 
      select(input$tablecolumnselector)
    
  })
  
  output$table <- DT::renderDataTable({
    shiny::validate(need(!is.null(input$map_marker_click$id),
                         "Click on a station on the map to view data.",
                         br()))
    DT::datatable(
      data = maptable(),
      style = 'bootstrap',
      extensions = 'Buttons',
      options = list(dom = 'Bfrtilp',
                     pageLength = 10,
                     compact = TRUE,
                     nowrap = TRUE,
                     scorllX = TRUE,
                     buttons = list('print',
                                    list(extend = 'collection',
                                         buttons = c('csv','excel','pdf'),
                                         text = 'Download')
                     )),
      rownames = FALSE,
      filter = 'bottom'
    ) %>% 
      DT::formatDate("Sample Time","toLocaleString")
  }, server = FALSE)
  
  output$youhavechosen <- renderUI({
    req(input$map_marker_click$id)
    
    d <- s %>% 
      filter(MLocID == input$map_marker_click$id)
    
    HTML(paste(
      tags$h4(d$'Station Description'),
      hr(),
      tags$h5("MLocID: ", d$MLocID),
      tags$h5("Waterbody Type: ", d$Type),
      tags$h5("Lat/Long: ", round(d$Lat,4),",",round(d$Long,4)),
      tags$h5("River Miles: ", round(d$RiverMile, 2)),
      tags$h5("Spawn Dates: ", d$Spawn_dates),
      tags$h5("Number of Samples: ", scales::comma(d$n.x)),
      tags$h5("Percent of all samples meeting criteria: ", round(d$pctmeets*100),"%"),
      tags$br(),
      tags$br(),
      tags$h5("Click and drag in the figure to zoom. Double click to zoom out.")
    )
    )
    
  })
  
  output$samplect <- renderPlotly({
    shiny::validate(need(!is.null(input$map_marker_click$id),
                         "Click on a station on the map to view data.",
                         br()))
    
    dta1 %>%
      filter(MLocID == input$map_marker_click$id) %>%
      group_by(month = floor_date(datetime,"month")) %>% 
      mutate(year = floor_date(datetime,"year")) %>% 
      summarize(n_samples = n()) %>%
      plot_ly(x = ~ month,
              y = ~ n_samples,
              type = "bar") %>% 
      layout(yaxis = list(title = "Sample Counts"),
             xaxis = list(title = "Month",
                          range = c(min(dta1$datetime),max(dta1$datetime))))
  })
  
  # 2.3 Sample summary plots ----
  ss <- reactive({
    dta1 %>% 
      select(MLocID, StationDes,datetime,in_spawn,DO_lim,DO_sat_lim,DO_status) %>% 
      group_by(month = floor_date(datetime,"month")) %>% 
      group_by(MLocID,StationDes,month,in_spawn,DO_lim,DO_sat_lim,DO_status) %>% 
      summarise(n=n()) %>% 
      ungroup() %>% 
      spread(DO_status,n,fill = 0) %>% 
      rename(over = "Meets criteria",
             under = "Excursion") %>% 
      mutate(month = as.Date(month),
             Spawning = dplyr::recode(as.factor(in_spawn),
                                      "TRUE" = "In spawning",
                                      "FALSE" = "Not in spawning"),
             `DO Limit` = dplyr::recode_factor(DO_lim,
                                               "6.5" = "6.5 mg/L (Estuarine)",
                                               "8" = "8 mg/L or 90% (Cold Water - Aquatic Life)",
                                               "11" = "11 mg/L or 95% (Spawning)"))
  })
  
  output$summaryplot <- renderPlotly({
    plot_ly(ss(), x = ~get(input$plottype)) %>% 
      add_trace(y = ~over,
                name = 'Meets criteria',
                marker = list(color="blue"),
                type = 'bar') %>% 
      add_trace(y = ~under,
                name = 'Excursion',
                marker = list(color="red"),
                type = 'bar') %>% 
      layout(yaxis = list(title = 'Sample Counts'),
             xaxis = list(title = tools::toTitleCase(input$plottype)))
    
  })
  
  output$site <- renderTable({
    sites[,c(1,2)]
  })
  
  # 2.4 Min DO plots ----
  # set colors
  pal <- viridis_pal(option = "D",direction = -1)(23)
  pal <- setNames(pal,unique(dta1$Site))
  # col <- colorRampPalette(brewer.pal(9,"Set3"))(14)
  # sitecol <- viridis_pal(option = "D")(14)
  # marginlist <- list(
  #  l = 60,
  #  r = 20,
  #  b = 50,
  # t = 20
  # )
  #
  wdi_e <- reactive({
    
    dta1 %>% 
      select(MLocID,Site,StationDes,datetime,do) %>% 
      group_by(MLocID, Site, StationDes, month = floor_date(datetime,"month")) %>% 
      filter(Site %in% input$estuary_site) %>% 
      summarize(min = min(do, na.rm = TRUE))
    
  })
  
  output$mindoplot_e <- renderPlotly({
    req(wdi_e())
    plot_ly(wdi_e(),
            x = ~month,
            y = ~min,
            text = ~paste("Site:", Site),
            color = ~Site,
            colors = pal,
            name = ~StationDes,
            mode = "markers",
            type = "scatter",
            marker = list(size = 10,alpha = 0.8)) %>%
      layout(title = list(text = "Estuary Sites",x = 0),
             showlegend = T,
             xaxis = list(title = "Month",
                          range = c("2007-01-01", "2017-01-31")),
             yaxis = list(title = "Dissolved Oxygen (mg/L)",
                          range = c(-1, 13)),
             annotations = list(text = "6.5 mg/L (Estuarine)",
                                x = "2016-05-01",
                                y = 6.5,
                                showarrow = TRUE)) %>% 
      add_segments(x ="2007-01-01",
                   xend = "2017-01-31",
                   y = 6.5,
                   yend = 6.5,
                   color = I("red"),
                   line = list(dash = "dash"),
                   marker = list(size = 0),
                   showlegend = FALSE)
    
  })
  
  wdi_r <- reactive({
    
    dta1 %>% 
      select(MLocID,Site,StationDes,datetime,do,DO_lim,in_spawn) %>% 
      group_by(MLocID, Site, StationDes, DO_lim, in_spawn, month = floor_date(datetime,"month")) %>% 
      filter(Site %in% input$river_site) %>% 
      summarize(min = min(do, na.rm = TRUE)) %>% 
      mutate(Spawning = dplyr::recode(as.factor(in_spawn),
                                      "TRUE" = "In spawning",
                                      "FALSE" = "Not in spawning"))
    
  })
  
  output$mindoplot_r <- renderPlotly({
    
    plot_ly(wdi_r(),
            x = ~month,
            y = ~min,
            text = ~paste("Site:", Site, " (",Spawning,")"),
            color = ~Site,
            colors = pal,
            name = ~paste(StationDes,Spawning,sep="\n"),
            mode = "markers",
            type = "scatter",
            marker = list(size = 10,alpha = 0.8),
            symbol = ~in_spawn,
            symbols = c("circle","x")) %>%
      layout(title = list(text = "River/Stream Sites",x = 0),
             showlegend = T,
             # margin = marginlist,
             # paper_bgcolor = "#ecf0f1",
             # plot_bgcolor = "#ecf0f1",
             xaxis = list(title = "Month",
                          range = c("2007-01-01", "2017-01-31")),
             yaxis = list(title = "Dissolved Oxygen (mg/L)",
                          range = c(-1, 13))) %>% 
      add_segments(x ="2007-01-01",
                   xend = "2017-01-31",
                   y = 11,
                   yend = 11,
                   color = I("red"),
                   line = list(dash = "dash"),
                   marker = list(size = 0),
                   showlegend = FALSE) %>% 
      add_segments(x ="2007-01-01",
                   xend = "2017-01-31",
                   y = 8,
                   yend = 8,
                   color = I("red"),
                   line = list(dash = "dash"),
                   marker = list(size = 0),
                   showlegend = FALSE) %>% 
      add_annotations(x = "2016-05-01",
                      y = 11,
                      xref = "x",
                      yref = "y",
                      text = "11 mg/L (Spawning)",
                      showarrow = TRUE) %>% 
      add_annotations(x = "2016-05-01",
                      y = 8,
                      xref = "x",
                      yref = "y",
                      text = "8 mg/L\n(Cold Water - Aquatic Life)",
                      showarrow = TRUE)
    
  })
  
  
  
  # 2.5 Min DO Sat plots ----
  
  wdis_e <- reactive({
    
    dta1 %>% 
      select(MLocID,Site,StationDes,datetime,do_sat_cor) %>% 
      group_by(MLocID, Site, StationDes, month = floor_date(datetime,"month")) %>% 
      filter(Site %in% input$estuary_site) %>% 
      summarize(min = min(do_sat_cor, na.rm = TRUE))
  })
  
  output$mindosplot_e <- renderPlotly({
    req(wdis_e())
    plot_ly(wdis_e(),
            x = ~month,
            y = ~min,
            text = ~paste("Site:", Site),
            color = ~Site,
            colors = pal,
            name = ~StationDes,
            mode = "markers",
            type = "scatter",
            marker = list(size = 10,alpha = 0.8)) %>%
      layout(title = list(text = "Estuary Sites",x = 0),
             showlegend = T,
             xaxis = list(title = "Month",
                          range = c("2007-01-01", "2017-01-31")),
             yaxis = list(title = "Dissolved Oxygen Saturation (%)",
                          range = c(0,110))
      )
  })
  
  wdis_r <- reactive({
    
    dta1 %>% 
      select(MLocID,Site,StationDes,datetime,do_sat_cor,DO_lim,in_spawn) %>% 
      group_by(MLocID, Site, StationDes, DO_lim, in_spawn, month = floor_date(datetime,"month")) %>% 
      filter(Site %in% input$river_site) %>% 
      summarize(min = min(do_sat_cor, na.rm = TRUE)) %>% 
      mutate(Spawning = dplyr::recode(as.factor(in_spawn),
                                      "TRUE" = "In spawning",
                                      "FALSE" = "Not in spawning"))
  })
  
  output$mindosplot_r <- renderPlotly({
    req(wdis_r())
    plot_ly(wdis_r(),
            x = ~month,
            y = ~min,
            text = ~paste("Site:", Site, " (",Spawning,")"),
            color = ~Site,
            colors = pal,
            name = ~paste(StationDes,Spawning,sep="\n"),
            mode = "markers",
            type = "scatter",
            marker = list(size = 10,alpha = 0.8),
            symbol = ~in_spawn,
            symbols = c("circle","x")) %>%
      layout(title = list(text = "River/Stream Sites",x = 0),
             showlegend = T,
             # margin = marginlist,
             # paper_bgcolor = "#ecf0f1",
             # plot_bgcolor = "#ecf0f1",
             xaxis = list(title = "Month",
                          range = c("2007-01-01", "2017-01-31")),
             yaxis = list(title = "Dissolved Oxygen Saturation (%)",
                          range = c(0, 130))) %>% 
      add_segments(x ="2007-01-01",
                   xend = "2017-01-31",
                   y = 95,
                   yend = 95,
                   color = I("red"),
                   line = list(dash = "dash"),
                   marker = list(size = 0),
                   showlegend = FALSE) %>% 
      add_segments(x ="2007-01-01",
                   xend = "2017-01-31",
                   y = 90,
                   yend = 90,
                   color = I("red"),
                   line = list(dash = "dash"),
                   marker = list(size = 0),
                   showlegend = FALSE) %>% 
      add_annotations(x = "2016-05-01",
                      y = 95,
                      text = "95% (Spawning)",
                      showarrow = T) %>% 
      add_annotations(x = "2015-10-01",
                      y = 90,
                      yshift = -10,
                      text = "90% (Cold Water - Aquatic Life)",
                      showarrow = F)
    
  })
  
  
  
  # 2.6 Boxplots ----
  boxplotdata <- reactive({
    
    dta1 %>% 
      mutate(month = floor_date(datetime,"month")) %>% 
      mutate(season = factor(month.abb[month(month)],
                             levels = c("May","Jun","Jul","Aug", "Oct","Nov"))) %>% 
      mutate(year = factor(year(floor_date(datetime,"year")))) %>% 
      filter(Site %in% input$boxplotsites)
  })
  
  output$summaryboxplot <- renderPlotly({
    plot_ly(boxplotdata(),
            x = ~get(input$boxplot),
            y = ~do,
            color = ~get(input$boxplotgrp),
            # colors = viridis_pal(option = "D")(6),
            type = "box") %>% 
      layout(boxmode = 'group',
             xaxis = list(title = tools::toTitleCase(input$boxplot)),
             yaxis = list(title = "Dissolved Oxygen (mg/L)"))
  })
  
  output$summaryboxplot_s <- renderPlotly({
    plot_ly(boxplotdata(),
            x = ~get(input$boxplot),
            y = ~do_sat_cor,
            color = ~get(input$boxplotgrp),
            # colors = viridis_pal(option = "D")(6),
            type = "box") %>% 
      layout(boxmode = 'group',
             xaxis = list(title = tools::toTitleCase(input$boxplot)),
             yaxis = list(title = "Dissolved Oxygen Saturation (%)"))
  })
  
  # 2.7 Display continuous data: Plots ----
  do_col <- setNames(c("blue","red"),unique(dta$`DO Status`))
  
  stations_subset <- reactive({
    dta %>% 
      filter(Site == input$station_selection) %>% 
      filter(Date >= input$daterange[1],
             Date <= input$daterange[2])
  })
  
  
  output$subplot <- renderPlotly({
    
    a <- plot_ly(stations_subset(),
                 x = ~get(input$x),
                 y = ~`Dissolved Oxygen (mg/L)`,
                 type = "scatter",
                 mode = "markers",
                 color = ~`DO Status`,
                 colors = do_col,
                 showlegend = TRUE) %>% 
      layout(yaxis = list(title = "Dissolved Oxygen (mg/L)"),
             xaxis = list(showline = TRUE, zeroline = FALSE))
    
    b <- plot_ly(stations_subset(),
                 x = ~get(input$x),
                 y = ~get(input$y2),
                 type = "scatter",
                 mode = "markers",
                 marker = list(color = "black"),
                 showlegend = FALSE) %>% 
      layout(yaxis = list(title = toTitleCase(input$y2)),
             xaxis = list(showline = TRUE, zeroline = FALSE))
    
    c <- plot_ly(stations_subset(),
                 x = ~get(input$x),
                 y = ~get(input$y3),
                 type = "scatter",
                 mode = "markers",
                 marker = list(color = "green"),
                 showlegend = FALSE) %>% 
      layout(yaxis = list(title = toTitleCase(input$y3)),
             xaxis = list(showline = TRUE, zeroline = FALSE))
    
    d <- plot_ly(stations_subset(),
                 x = ~get(input$x),
                 y = ~get(input$y4),
                 type = "scatter",
                 mode = "markers",
                 marker = list(color = "purple"),
                 showlegend = FALSE) %>% 
      layout(yaxis = list(title = toTitleCase(input$y4)),
             xaxis = list(showline = TRUE, zeroline = FALSE))
    
    sp <- subplot(a,b,c,d, nrows = 4, shareX = TRUE, titleY = TRUE)
    
    sp %>% 
      layout(
        xaxis = list(
          title = toTitleCase(input$x),
          rangeselector = list()
        )
      )
    
  })
  
  # 2.8 Display continuous data: Table ----
  
  output$plottable <- DT::renderDataTable({
    
    stations_subset_tb <- reactive({
      stations_subset() %>% 
        select(MLocID, `Station Description`, `Sample Time`, `Dissolved Oxygen (mg/L)`, `Dissolved Oxygen Grade`,
               `Dissolved Oxygen Saturation (%)`, `Dissolved Oxygen Saturation Grade`, `DO Status`, `Temperature (°C)`,
               `Temperature Grade`, pH, `pH Grade`, `Specific Conductivity (µS)`, `Specific Conductivity Grade`, `Data Source`) %>% 
        select(MLocID,`Station Description`, `Sample Time`, `Dissolved Oxygen (mg/L)`, input$x, input$y2, input$y3, input$y4)
      
    })
    
    DT::datatable(
      data = stations_subset_tb(),
      style = 'bootstrap',
      extensions = 'Buttons',
      options = list(dom = 'Bfrtilp',
                     pageLength = 10,
                     compact = TRUE,
                     nowrap = TRUE,
                     scorllX = TRUE,
                     buttons = list('print',
                                    list(extend = 'collection',
                                         buttons = c('csv','excel','pdf'),
                                         text = 'Download')
                     )),
      rownames = FALSE,
      filter = 'top'
    ) 
  }, server = FALSE)
  
}


shinyApp(ui,server)