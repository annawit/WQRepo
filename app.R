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

md2 <- dta1
# md2 <- dta1 %>%
#   filter(!is.na(datetime)) %>% 
#   mutate(Site = paste(MLocID, StationDes))

#colors
coul <- colorRampPalette(brewer.pal(9, "Set3"))(14)


# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
    "Continuous Dissolved Oxygen Visualizer",
    
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
                              choices = names(md2),
                              selected = c("MLocID", "datetime", "temp", "ph",
                                           "do", "cond", "data_source",
                                           "StationDes")
                            ),
                            HTML("Select a station and variables,
                             then hit 'Download data'."),
                            br(), br(),
                            downloadButton(
                              outputId = "download_data",
                              label = "Download table data")
                          ),
                          mainPanel(DT::dataTableOutput("table"))
                        )
               ),
               
               # Subpanel 1.2 ------------------------------------------------------------
               tabPanel("Placeholder"),
               
               # Subpanel 1.3 ------------------------------------------------------------
               
               tabPanel("Placeholder2"),
             )),
    
    # Main Tab 2 --------------------------------------------------------------
    tabPanel("Data Overview",
             # Subpanel Samples per year ------------------------------------------------------------
             tabsetPanel(
               tabPanel("Plot of total number of samples per year",
                        plotlyOutput("nplot")),
               tabPanel("DO at each site",
                        plotlyOutput("avgdoplot")),
               
               # Subpanel boxplots ------------------------------------------------------------
               
               tabPanel("Other overview visualizations",
                        plotlyOutput("summaryboxplot"),
                        plotlyOutput("summaryboxplot2"),
                        plotlyOutput("summaryboxplot3"))
             )),
    
    # Main Tab 3 --------------------------------------------------------------
    
    tabPanel(
      "Search by Station",
      fluidPage(
        tabsetPanel(
          tabPanel("Plots",
                   sidebarPanel(
                     selectInput(
                       inputId = "station_selection",
                       label = "Select station:",
                       choices = md2$Site
                     ),
                     selectInput(
                       inputId = "x",
                       label = "x-axis",
                       choices = names(md2),
                       selected = "datetime"
                     ),
                     # selectInput(
                     #   inputId = "y",
                     #   label = "y-axis",
                     #   choices = names(md2),
                     #   selected = "do"
                     # ),
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
                     ),
                     selectInput(
                       inputId = "y4",
                       label = "4th y-axis",
                       choices = names(md2),
                       selected = "ph"
                     )
                   ),
                   mainPanel(
                     textOutput("tab2text"),
                     plotlyOutput("subplot"),
                     verbatimTextOutput("brush")
                   )),
          tabPanel("Data Selected From Graph, in a Table",
                   DT::dataTableOutput("graph_to_table"))
        )
      )
    ),
    
    # Main Tab 4 --------------------------------------------------------------
    
    tabPanel("Search by DO over/under limit",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   selectizeInput(
                     inputId = "station_pf",
                     label = "Select station(s):",
                     choices = levels(md2$MLocID),
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
      addMarkers(data = md2,
                 lat = ~Lat_DD,
                 lng = ~Long_DD,
                 label = ~paste0("Station ID: ", MLocID, "
                                 Site Name: ", StationDes),
                 labelOptions = labelOptions(textOnly = FALSE),
                 layerId = md2$MLocID) %>%
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
      m <- md2 %>% 
        select(input$checkboxtablesorter %>%
                 filter(MLocID == input$map_marker_click$id))
      write.csv(m)
    }
  )

# table -------------------------------------------------------------------
  
  output$table <- DT::renderDataTable({
    shiny::validate(need(!is.null(input$map_marker_click$id),
                         "Click on a station to view data"))
    maptabledata <- md2 %>% 
      select(input$checkboxtablesorter) %>% 
      filter(MLocID == input$map_marker_click$id)
    DT::datatable(
      data = maptabledata,
      options = list(pageLength = 10,
                     scrollX = TRUE),
      rownames = FALSE,
      filter = 'bottom'
    ) %>% 
      DT::formatDate("datetime", "toLocaleString")
  })

# Subpanel 1.1 items ------------------------------------------------------
  output$nplot <- renderPlotly({
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
  })
  
  output$avgdoplot <- renderPlotly({
    wdi <- md2 %>%
      group_by(MLocID, month.p = floor_date(datetime, "month")) %>% 
      mutate(min = min(do))
    
    plot_ly(wdi, x = ~month.p, y = ~min,
            text = ~paste('Site:', StationDes)) %>%
      add_markers(color = ~Site,
                  colors = coul) %>% 
      layout(xaxis = list(title = "Date",
                          range = c("2007-01-01", "2016-12-31")),
             yaxis = list(title = "Minimum Dissolved Oxygen"))
  })
  
  output$summaryboxplot <- renderPlotly({
    boxplotData <- md2 %>% 
      mutate(month = floor_date(datetime, "month")) %>%
      mutate(season = factor(month.abb[month(datetime)], levels = c("May", "Jun", "Jul", "Aug", "Oct")))
    
    plot_ly(boxplotData,
            x = ~as.factor(MLocID),
            y = ~do,
            color = ~season,
            # colors = viridis_pal(option = "D")(5), 
            type = "box",
            text = ~paste('Site: ', StationDes)) %>%
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
            color = ~MLocID, 
            colors = viridis_pal(option = "D")(5), type = "box",
            text = ~paste('Site: ', StationDes)) %>%
      layout(boxmode = "group")
  })
  
  output$summaryboxplot3 <- renderPlotly({
    boxplotData <- md2 %>% 
      mutate(year = factor(floor_date(datetime, "year")))
    # mutate(season = factor(month.abb[month(datetime)], levels=c("May", "Jun", "Jul", "Aug", "Oct")))
    plot_ly(boxplotData, x = ~year, y = ~do, color = ~MLocID, 
            colors = viridis_pal(option = "D")(5), type = "box",
            text = ~paste('Site: ', StationDes)) %>%
      layout(boxmode = "group")
  })
  
  
  # Main Tab 3 Items --------------------------------------------------------
  
  output$tab2text <- renderUI({
    str1 <- paste("You have selected", input$station_selection)
    str2 <- paste("The elevation at this site is")
    
    HTML(paste(str1, str2, sep = '<br/>'))
    
  })
  
  stations_subset <- reactive({
    req(input$station_selection)
    filter(md2, Site %in% input$station_selection)
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

# Subplot -----------------------------------------------------------------

  output$subplot <- renderPlotly({
    
    a <- plot_ly(data = stations_subset(),
            x = ~get(input$x),
            y = ~do,
            type = "scatter",
            source = "A") %>% 
      add_markers(color = ~DO_status)
    b <- plot_ly(data = stations_subset(),
                 x = ~get(input$x),
                 y = ~get(input$y2),
                 type = "scatter")
    c <- plot_ly(data = stations_subset(),
                 x = ~get(input$x), y = ~get(input$y3),
                 type = "scatter")
    d <- plot_ly(data = stations_subset(),
              x = ~get(input$x), y = ~get(input$y4),
              type = "scatter")
    sp <- subplot(a, b, c, d, nrows = 4, shareX = TRUE)
    sp %>% 
      layout(autosize = FALSE, width = 1000, height = 800)
  })

# graph to table ----------------------------------------------------------

  output$brush <- renderPrint({
    d <- event_data("plotly_selected")
    if (is.null(d)) "Click and drag events appear here" else d
  })
  
  
  output$graph_to_table <- DT::renderDT({
    d <- event_data("plotly_selected", source = "A")
    if (is.null(d)) return(NULL)
    # else stations_subset() %>% filter(input$x > min(d$x) )
    else stations_subset() %>%
      filter(between(input$x, min(d$x), max(d$x)))
    })

  } #End Server

shinyApp(ui = ui, server = server)

