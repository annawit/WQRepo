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

# Data -------------------------------------------------------


load("dataforwqapp.Rdata")

md2 <- dta1 %>% 
  mutate(DO_status = ifelse(DO_status == 0, "Below limit", "Above limit"))

load("sitesummary.Rdata")
sd <- sites

#colors
coul <- colorRampPalette(brewer.pal(9, "Set3"))(14)


# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
    "Continuous Dissolved Oxygen Visualizer",
    
    # Select from map --------------------------------------------------------------
    tabPanel("Select from map",
             sidebarLayout(position = "right",
                           sidebarPanel(
                             wellPanel(
                               textOutput("temp"),
                               hr(),
                               plotlyOutput("mininplot")),
                             wellPanel(
                               checkboxGroupButtons(
                                 inputId = "checkboxtablesorter",
                                 label = "Select table columns to display:",
                                 choices = names(md2),
                                 selected = c("MLocID", "datetime", "temp", "ph",
                                              "do", "cond", "data_source",
                                              "StationDes")
                               )),
                             wellPanel(
                               HTML("Select a station and variables,
                                             then hit 'Download data'."),
                               hr(),
                               downloadButton(
                                 outputId = "download_data",
                                 label = "Download table data")
                             )),
                           mainPanel(
                             wellPanel(leafletOutput("map")),
                             wellPanel(DT::dataTableOutput("table"))
                           )
                           
             )),
    
    # Data overview --------------------------------------------------------------
    tabPanel("Data Overview",
             # Subpanel Samples per year ------------------------------------------------------------
             tabsetPanel(
               tabPanel("Plot of total number of samples per year",
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            selectInput(inputId = "plottype",
                                        "Select:", 
                                        choices = c("All data" = "nplot",
                                                    "All over DO limit",
                                                    "All under DO limit",
                                                    "Stacked")
                            )),
                          mainPanel(plotlyOutput("nplot"),
                                    plotlyOutput("nstackplot"))
                        )
               ),
               # Subpanel DO at each site ----------------
               tabPanel("DO at each site",
                        sidebarLayout(
                          sidebarPanel(position = "right",
                            checkboxGroupInput("SiteCheckGroup", 
                                               label = h3("Sites"), 
                                               choices = unique(md2$Site),
                                               selected = unique(md2$Site))
                          ),
                          mainPanel(
                            plotlyOutput("avgdoplot")
                          )
                        )
               ),
               
               # Subpanel boxplots ------------------------------------------------------------
               
               tabPanel("Other overview visualizations",
                        plotlyOutput("summaryboxplot"),
                        plotlyOutput("summaryboxplot2"),
                        plotlyOutput("summaryboxplot3"))
             )),
    
    # Search by Station --------------------------------------------------------------
    
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
                     # dateRangeInput(inputId = 'dateRange2',
                     #                label = "Select dates:",
                     #                start = min(md2$datetime), end = max(md2$datetime),
                     #                min = min(md2$datetime), max = max(md2$datetime),
                     #                separator = " to ", format = "mm/dd/yy",
                     #                startview = 'year', weekstart = 0
                     # )
                   ),
                   mainPanel(
                     plotlyOutput("subplot"),
                     verbatimTextOutput("brush")
                   )),
          tabPanel("Data Selected From Graph, in a Table",
                   DT::dataTableOutput("graph_to_table"))
        )
      )
    ),
    
    # Search by DO --------------------------------------------------------------
    
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
      addMarkers(data = sd,
                 lat = ~Lat_DD,
                 lng = ~Long_DD,
                 label = ~paste0("Station ID: ", MLocID, "
                                 Site Name: ", StationDes),
                 labelOptions = labelOptions(textOnly = FALSE),
                 layerId = sd$MLocID) %>%
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
        select(input$checkboxtablesorter) %>%
        filter(MLocID == input$map_marker_click$id)
      write.csv(m, file)
    }
  )
  
  # output$download_data <- downloadHandler({
  #   
  #   m <- md2 %>% 
  #     select(input$checkboxtablesorter) %>%
  #     filter(MLocID == input$map_marker_click$id)
  #   
  #   filename = function() {
  #     paste(m, ".csv", sep = "")
  #   }
  #   content = function(file) {
  #     write.csv(m, file, row.names = FALSE)
  #   }
  # })
  

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
                     compact = TRUE,
                     nowrap = TRUE,
                     scrollX = TRUE),
      rownames = FALSE,
      filter = 'bottom'
    ) %>% 
      DT::formatDate("datetime", "toLocaleString")
  })


## mini nplot --------------------------------------------------------------

  output$mininplot <- renderPlotly({
    shiny::validate(need(!is.null(input$map_marker_click$id),
                         "Click on a station on the map to view data"))
    md2 %>% 
      filter(MLocID == input$map_marker_click$id) %>% 
      group_by(month = floor_date(datetime, "month")) %>% 
      mutate(n_samples = n()) %>% 
      mutate(season = factor(month.abb[month(datetime)],
                             levels = c("May", "Jun", "Jul", "Aug", "Oct"))) %>% 
      plot_ly(x = ~month,
              y = ~n_samples,
              type = "bar",
              color = ~season,
              colors = viridis_pal()(3)) %>%
      layout(yaxis = list(title = 'Count'),
             xaxis = list(title = "Date",
                          range = c(min(md2$datetime), max(md2$datetime))),
             barmode = 'group')
  })  
  
  
  # DATA OVERVIEW TAB----- 
  n_sum <- reactive({
    md2 %>% 
      select(MLocID, StationDes, datetime, DO_status) %>% 
      group_by(month = floor_date(datetime, "month")) %>% 
      mutate(n_samples = n()) %>% 
      mutate(season = factor(month.abb[month(datetime)],
                             levels = c("May", "Jun", "Jul", "Aug", "Oct")))
  })
  
  
  
# nplot ------------------------------------------------------
  output$nplot <- renderPlotly({
    # md2 %>% 
    #   group_by(month = floor_date(datetime, "month")) %>% 
    #   mutate(n_samples = n()) %>% 
    #   mutate(season = factor(month.abb[month(datetime)],
    #                          levels = c("May", "Jun", "Jul", "Aug", "Oct"))) %>% 
      plot_ly(n_sum(),
              x = ~as.factor(month),
              y = ~n_samples,
              type = "bar",
              color = ~season, colors = viridis_pal()(3)) %>%
      layout(yaxis = list(title = 'Count'),
             xaxis = list(title = "Date",
                          tickangle = 55
                          # ,
                          # range = c("2007-01-01", "2016-12-31")
                          )
      )
             # ,
             # barmode = 'group')
  })

# stacked do count --------------------------------------------------------

  output$nstackplot <- renderPlotly({
    n2 <- n_sum() %>%
      group_by(MLocID, month, DO_status) %>% 
      summarise(n = n()) %>% 
      ungroup() %>% 
      spread(DO_status, n, fill = 0) %>% 
      rename(over = "Above limit",
             under = "Below limit")
    
    plot_ly(n2,
            x = ~as.factor(month),
            y = ~under,
            type = 'bar', name = 'Under Limit') %>%
      add_trace(y = ~over, name = 'Over Limit') %>%
      layout(yaxis = list(title = 'Count'), barmode = 'stack')
  
  })
# do summary plot ---------------------------------------------------------

  
  output$avgdoplot <- renderPlotly({
    wdi <- md2 %>%
      group_by(MLocID, month.p = floor_date(datetime, "month")) %>% 
      filter(Site %in% input$SiteCheckGroup) %>% 
      mutate(min = min(do))
    
    plot_ly(wdi, x = ~month.p, y = ~min,
            text = ~paste('Site:', StationDes),
            color = ~Site,
            colors = coul,
            marker = list(size = 10,
                          line = list(color = "#000000",
                                      width = 0.6),
                          alpha = 0.8)) %>%
      layout(showlegend = FALSE,
             xaxis = list(title = "Date",
                          range = c("2007-01-01", "2016-12-31")),
             yaxis = list(title = "Minimum Dissolved Oxygen"),
             autosize = FALSE, width = 1000, height = 800)
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
  
  # output$tab2text <- renderUI({
  #   str1 <- paste("You have selected", input$station_selection)
  #   str2 <- paste("The elevation at this site is")
  #   
  #   HTML(paste(str1, str2, sep = '<br/>'))
  #   
  # })
  
  stations_subset <- reactive({
    req(input$station_selection)
    b <- md2 %>% filter(Site %in% input$station_selection) 
    # %>% 
    #   filter(mdy(datetime) > mdy(input$daterange2[1]) & mdy(datetime) < mdy(input$daterange2[2]))
      b
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
    DO_pal <- c("#000000", "#325C62", "#F4A767")
    DO_pal <- setNames(DO_pal, c("do", "Above limit", "Below limit"))

    a <- plot_ly(data = stations_subset(),
                 x = ~get(input$x),
                 y = ~do,
                 colors = DO_pal,
                 type = "scatter",
                 source = "A",
                 showlegend = FALSE) %>% 
      add_markers(color = ~DO_status,
                  colors = DO_pal,
                  showlegend = TRUE)
    b <- plot_ly(data = stations_subset(),
                 x = ~get(input$x),
                 y = ~get(input$y2),
                 name = input$y2,
                 marker = list(color = "#3C3545"),
                 type = "scatter")
    c <- plot_ly(data = stations_subset(),
                 x = ~get(input$x),
                 y = ~get(input$y3),
                 name = input$y3,
                 marker = list(color = "#49805F"),
                 type = "scatter")
    d <- plot_ly(data = stations_subset(),
                 x = ~get(input$x), 
                 y = ~get(input$y4),
                 name = input$y4,
                 marker = list(color = "#959B4F"),
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

  

# DO map ------------------------------------------------------------------

  output$DOmap <- renderLeaflet({
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
  } #End Server

shinyApp(ui = ui, server = server)

