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
load("PracticeDataLocationInfo.Rdata")

# stationInfo$MonitoringLocationIdentifier <- gsub("-ORDEQ", "", stationInfo$MonitoringLocationIdentifier)
stationInfo$MonitoringLocationIdentifier <- gsub("-ORDEQ", "", stationInfo$MonitoringLocationIdentifier)
stations <- stationInfo[,c(4:7)]

stations$Latitude <- as.numeric(stations$Latitude)
stations$Longitude <- as.numeric(stations$Longitude)

#Solves issue of lasar ids coming in as factors
dta2$lasar_id <- as.numeric(levels(dta2$lasar_id)[dta2$lasar_id])
mapData <- merge(dta2, stations,
                 by.x = "lasar_id",
                 by.y = "MonitoringLocationIdentifier",
                 all.x = TRUE, all.y = FALSE)

md2 <- mapData %>%
  filter(!is.na(lasar_id)) %>% 
  filter(!is.na(datetime))


# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
    "A Shiny App for Dissolved Oxygen",

# Main Tab 1 --------------------------------------------------------------
tabPanel("Select from map",
         tabsetPanel(
           

# Subpanel 1.1 ------------------------------------------------------------
           tabPanel("Table",
                    sidebarLayout(
                      sidebarPanel(
                        leafletOutput("map"),
                        textOutput("temp"),
                        checkboxGroupButtons(
                          inputId = "checkboxtablesorter",
                          label = "Select table columns to display:",
                          choices = names(mapData),
                          selected = c("lasar_id", "datetime", "temp", "ph",
                                       "do", "cond", "data_source",
                                       "MonitoringLocationName")
                          ),
                        HTML("Select a station and variables,
                             then hit 'Download data'."),
                        br(), br()
                        # ,
                        # downloadButton(
                        #   outputId = "download_data",
                        #   label = "Download table data")
                      ),
                      mainPanel(DT::dataTableOutput("table"))
                      )
                    ),

# Subpanel 1.2 ------------------------------------------------------------
tabPanel("Plot of total number of samples per year",
         plotlyOutput("nplot"),
         plotlyOutput("avgdoplot")),

# Subpanel 1.3 ------------------------------------------------------------

tabPanel("Other overview visualizations",
         plotlyOutput("summaryboxplot"),
         plotlyOutput("summaryboxplot2"),
         plotlyOutput("summaryboxplot3"))
         )),

# Main Tab 2 --------------------------------------------------------------

tabPanel(
  "Search by Station",
  fluidPage(
    tabsetPanel(
      tabPanel("Plots",
               sidebarPanel(
                 selectInput(
                   inputId = "station_selection",
                   label = "Select station:",
                   choices = paste(levels(as.factor(md2$lasar_id)), md2$MonitoringLocationName)
                 ),
                 selectInput(
                   inputId = "x",
                   label = "x-axis",
                   choices = names(md2),
                   selected = "datetime"
                 ),
                 selectInput(
                   inputId = "y",
                   label = "y-axis",
                   choices = names(md2),
                   selected = "do"
                 ),
                 selectInput(
                   inputId = "y2",
                   label = "2nd y-axis",
                   choices = names(md2),
                   selected = "temp"
                 ),
                 selectInput(
                   inputId = "y3",
                   label = "3rd y-axis",
                   choices = names(md2),
                   selected = "cond"
                 )
               ),
               mainPanel(
                 plotlyOutput("subsetscatter"),
                 plotlyOutput("subsetscatter2"),
                 plotlyOutput("subsetscatter3")
               )),
      tabPanel("Data Selected From Graph, in a Table",
               DT::dataTableOutput("graph_to_table"))
      )
    )
  ),

# Main Tab 3 --------------------------------------------------------------

tabPanel("Search by DO pass/fail",
         fluidPage(
           sidebarLayout(
             sidebarPanel(
               selectizeInput(
                 inputId = "station_pf",
                 label = "Select station(s):",
                 choices = levels(mapData$lasar_id),
                 multiple = TRUE
               ),
               radioButtons(
                 inputId = "DOPassFailRadio",
                 label = "Select:",
                 choices = c("Pass", "Fail", "All"),
                 selected = "All"
               )
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Map"),
                 tabPanel("Table")
                 )
               )
             )
           )
         )
)
) # End Fluid Page

# Server ------------------------------------------------------------------
server <- function(input, output, session) {

# Main Tab 1 Items --------------------------------------------------------
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri Satellite Map") %>%
      addProviderTiles("OpenTopoMap", group = "Open Topo Map") %>%
      addProviderTiles("CartoDB", group = "Carto") %>%
      addMarkers(data = stations,
                 label = ~paste0("Station ID: ", MonitoringLocationIdentifier, "
                                 Site Name: ", MonitoringLocationName),
                 labelOptions = labelOptions(textOnly = FALSE),
                 layerId = stations$MonitoringLocationIdentifier) %>%
      addLayersControl(baseGroups = c("Esri Satellite Map",
                                      "Open Topo Map",
                                      "Carto"))
  })
  
  output$temp <- renderPrint({
    req(input$map_marker_click$id)
    cat("The station you have chosen is: ", input$map_marker_click$id)
  })
  
  output$download_data <- downloadHandler(
    filename = "ShinyDataTableDownload.csv",
    content = function(file) { 
      write.csv(mapData %>% 
                  select(input$checkboxtablesorter %>%
                           filter(lasar_id == input$map_marker_click$id)))
    }
  )
  
  output$table <- DT::renderDataTable({
    shiny::validate(need(!is.null(input$map_marker_click$id),
                         "Click on a station to view data"))
    maptabledata <- md2 %>% 
      select(input$checkboxtablesorter) %>% 
      filter(lasar_id == input$map_marker_click$id)
    DT::datatable(
      data = maptabledata,
      options = list(pageLength = 10),
      rownames = FALSE,
      filter = 'bottom'
    )
  })
  
  # output$dateplot <- renderPlot()
  output$nplot <- renderPlotly({
    md2 %>% 
      group_by(month = floor_date(datetime, "month")) %>% 
      mutate(n_samples = n()) %>% 
      mutate(season = factor(month.abb[month(datetime)],
                             levels = c("May", "Jun", "Jul", "Aug", "Oct"))) %>% 
      plot_ly(x = ~month, y = ~n_samples, type = "bar",
              color = ~season, colors = viridis_pal()(3)) %>% 
      layout(yaxis = list(title = 'Count'), barmode = 'group')
  })
  
  output$avgdoplot <- renderPlotly({
    wdi <- md2 %>%
      group_by(lasar_id, month.p = floor_date(datetime, "month")) %>% 
      mutate(min = min(do))
    
    plot_ly(wdi, x = ~month.p, y = ~min, color = ~as.factor(lasar_id),
            colors = viridis_pal(option = "C")(5),
            text = ~paste('Site:', MonitoringLocationName)) %>%
      layout(
        xaxis = list(range = c("2007-01-01", "2016-12-31")))
  })
  
  output$summaryboxplot <- renderPlotly({
    boxplotData <- md2 %>% 
      mutate(month = floor_date(datetime, "month")) %>%
      mutate(season = factor(month.abb[month(datetime)], levels = c("May", "Jun", "Jul", "Aug", "Oct")))
    
    plot_ly(boxplotData,
            x = ~as.factor(lasar_id),
            y = ~do,
            color = ~season,
            # colors = viridis_pal(option = "D")(5), 
            type = "box",
            text = ~paste('Site: ', MonitoringLocationName)) %>%
      layout(boxmode = "group")
  })
  
  output$summaryboxplot2 <- renderPlotly({
    boxplotData <- md2 %>% 
      mutate(month = floor_date(datetime, "month")) %>%
      mutate(season = factor(month.abb[month(datetime)],
                             levels = c("May", "Jun", "Jul", "Aug", "Oct")))
    plot_ly(boxplotData,
            x = ~season,
            y = ~do,
            color = ~lasar_id, 
            colors = viridis_pal(option = "D")(5), type = "box",
            text = ~paste('Site: ', MonitoringLocationName)) %>%
      layout(boxmode = "group")
  })
  
  output$summaryboxplot3 <- renderPlotly({
    boxplotData <- md2 %>% 
      mutate(year = factor(floor_date(datetime, "year")))
    # mutate(season = factor(month.abb[month(datetime)], levels=c("May", "Jun", "Jul", "Aug", "Oct")))
    plot_ly(boxplotData, x = ~year, y = ~do, color = ~lasar_id, 
            colors = viridis_pal(option = "D")(5), type = "box",
            text = ~paste('Site: ', MonitoringLocationName)) %>%
      layout(boxmode = "group")
  })
  
  
  # Main Tab 2 Items --------------------------------------------------------
  
  
  stations_subset <- reactive({
    req(input$station_selection)
    filter(md2, lasar_id %in% input$station_selection)
  })
  
  output$subsetscatter <- renderPlotly({
    plot_ly(data = stations_subset(),
            x = ~get(input$x), y = ~get(input$y),
            type = "scatter",
            source = "firstplotselection")
  }) 
  
  output$subsetscatter2 <- renderPlotly({
    plot_ly(data = stations_subset(),
            x = ~get(input$x), y = ~get(input$y2),
            type = "scatter")
  }) 
  
  output$subsetscatter3 <- renderPlotly({
    plot_ly(data = stations_subset(),
            x = ~get(input$x), y = ~get(input$y3),
            type = "scatter")
  })
  
  
  output$graph_to_table <- DT::renderDT({
    d <- event_data("plotly_selected", source = "A")
    if (is.null(d) == T) return(NULL)
    # else stations_subset() %>% filter(input$x > min(d$x) )
    else stations_subset() %>% filter(between(input$x, min(d$x), max(d$x)))
    })

  } #End Server

shinyApp(ui = ui, server = server)

