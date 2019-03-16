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
library(tools)


# Data -------------------------------------------------------


load("dataforwqapp.Rdata")



md2 <- dta1 %>% 
  mutate(DO_status = ifelse(DO_status == 0, "Excursion", "Meets criteria"))

# sts <- md2 %>% 
#   group_by(MLocID, DO_status) %>% 
#   count() %>% 
#   filter(DO_status == "Excursion")

#gives percent meeting criteria at each site
meets <- md2 %>% 
  group_by(MLocID, DO_status) %>% 
  count() %>% 
  group_by(MLocID) %>% 
  mutate(pctmeets = n/sum(n)) %>% 
  filter(DO_status == "Meets criteria") %>% 
  mutate(pctbin = cut(pctmeets, c(0, 0.5, 0.99, 1),
                      include.lowest = TRUE, labels = c("<50", "50-99", ">99")))

pctcolor <- colorFactor(palette = "RdYlGn", meets$pctbin)

plot <- ggplot(data = meets, aes(x = MLocID, y = pctmeets, fill = DO_status)) +
  geom_bar(stat = 'identity', position = 'dodge')
plot


md2

load("sitesummary.Rdata")
sd <- sites

s <- left_join(sd, meets, by = "MLocID")

marginlist <- list(
  l = 60,
  r = 20, 
  b = 50,
  t = 20,
  pad = 8
)
# pal <- c("red", "blue", "green")
# pal <- setNames(pal, c("virginica", "setosa", "versicolor"))

pal <- viridis_pal(option = "D", direction = -1)(14)
pal <- setNames(pal, unique(md2$Site))


#colors
coul <- colorRampPalette(brewer.pal(9, "Set3"))(14)
sitecol <- viridis_pal(option = "D")(14)

color_map <- c("Meets criteria" = "#F4A767", "Excursion" = "#325C62")
DO_status_levels <- c("Meets criteria", "Excursion")
DO_status_colors <- c("#F4A767", "#325C62")
names(DO_status_colors) <- DO_status_levels


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  navbarPage(
    "Continuous Dissolved Oxygen Visualizer",
    
    # Select from map --------------------------------------------------------------
    tabPanel("Select from map",
             sidebarLayout(position = "right",
                           sidebarPanel(
                             wellPanel(
                               # Station summary text
                               uiOutput("youhavechosen"),
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
                             wellPanel(DT::dataTableOutput("table"),
                                       br(),
                                       br())
                           )
                           
             )),
    
    # Overview plots --------------------------------------------------------------
    tabPanel("Overview Plots",
             # Subpanel Samples per year ------------------------------------------------------------
             tabsetPanel(
               
               tabPanel("Sample summary",
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            selectInput(inputId = "plottype",
                                        "Select:", 
                                        choices = c("By time" = "month",
                                                    "By site" = "MLocID",
                                                    "By spawning" = "Spawning",
                                                    "By DO limit" = "DO Limit")
                            )),
                          mainPanel(
                            wellPanel(
                              plotlyOutput("nstackplot", height = 500),
                              verbatimTextOutput("click")
                            )
                          )
                        )
               ),
               # Min DO ----------------
               tabPanel("Minimum DO per sampling event",
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            checkboxGroupInput("SiteCheckGroup", 
                                               label = h3("Sites"), 
                                               choices = unique(md2$Site),
                                               selected = max(unique(md2$Site)))
                          ),
                          mainPanel(
                            width = 9,
                            wellPanel(
                              h4("Minimum DO at a given site for a given sampling event."),
                              plotlyOutput("mindoplot")
                            )
                          )
                        )
               ),
               
               # boxplots ------------------------------------------------------------
               
               tabPanel("Boxplots",
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            selectInput("boxplotx",
                                        "Select x-axis:",
                                        choices = c("By site" = "MLocID", "By season" = "season", "By year" = "year")),
                            selectInput("boxplotgroup",
                                        "Select color:",
                                        choices = c("By site" = "MLocID", "By season" = "season", "By year" = "year")),
                            radioButtons("boxplotradio",
                                         "Water body type",
                                         c("Estuary only" = "6.5",
                                           "River only" = "8")),
                            checkboxGroupInput("boxplotsites", 
                                               label = h3("Select sites"), 
                                               choices = unique(md2$Site),
                                               selected = unique(md2$Site))
                            ),
                          mainPanel(
                            width = 9,
                            br(),
                            wellPanel(
                              plotlyOutput("summaryboxplot")
                            )
                            # ,
                            # plotlyOutput("summaryboxplot2"),
                            # plotlyOutput("summaryboxplot3")
                          )
                        )
               ))
    ),
    
    # Display Continuous  --------------------------------------------------------------
    
    tabPanel(
      "Display Continuous Data",
      fluidPage(
        tabsetPanel(
          tabPanel("Plots",
                   br(),
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
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
                       ),
                       dateRangeInput(inputId = 'daterange',
                                      label = "Select dates:",
                                      start = min(md2$datetime),
                                      end = max(md2$datetime)-1,
                                      min = min(md2$datetime), max = "2016-12-31",
                                      separator = " to ", format = "mm/dd/yy",
                                      startview = 'year', weekstart = 0
                       )
                     ),
                     mainPanel(
                       fluidRow(
                         column(8,
                                wellPanel(
                                  plotlyOutput("subplot", height = 800)
                                )
                                
                         ),
                         column(4,
                                wellPanel(
                                  # hr(),
                                  plotlyOutput("summaryplot"),
                                  hr(),
                                  DT::dataTableOutput("plot.summ")
                                )
                         )
                         
                         # ,
                         # verbatimTextOutput("brush")
                       )
                     ))
          ),
          # ,
          tabPanel("Data Selected From Graph, in a Table",
                   # verbatimTextOutput("brush"),
                   DT::dataTableOutput("graph_to_table"))
        )
      )
    ),
    
    # Search by DO --------------------------------------------------------------
    
    tabPanel("Search by DO criteria",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   # selectizeInput(
                   #   inputId = "station_pf",
                   #   label = "Select station(s):",
                   #   choices = md2$Site,
                   #   multiple = TRUE
                   # ),
                   radioButtons(
                     inputId = "DOPassFailRadio",
                     label = "Select:",
                     choices = c("Meets criteria" = 1, "Excursion" = 0),
                     selected = "All"
                   )
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Map",
                              leafletOutput("DOmap")),
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
      addCircleMarkers(data = s,
                 lat = ~Lat_DD,
                 lng = ~Long_DD,
                 color = ~pctcolor(pctbin),
                 radius = ~(n.y^(3/9)),
                 fillOpacity = 0.5,
                 label = ~paste0("Station ID: ", MLocID, " ", StationDes),
                 labelOptions = labelOptions(textOnly = FALSE),
                 layerId = sd$MLocID) %>%
      addLayersControl(baseGroups = c("Esri Satellite Map",
                                      "Open Topo Map",
                                      "Carto"))
  })
  
  output$youhavechosen <- renderUI({
    req(input$map_marker_click$id)
    
    d <- s %>% 
      filter(MLocID == input$map_marker_click$id)
    
    HTML(paste(tags$h4(d$StationDes),
               "MLocID: ", d$MLocID, "<br/>",
               "Lat/long: ", round(d$Lat_DD, 4), ", ", round(d$Long_DD, 4), "<br/>",
               "River mile: ", round(d$RiverMile, 2), "<br/>",
               "Spawn dates: ", d$Spawn_dates, "<br/>",
               "Number of samples: ", scales::comma(d$n.x), "<br/>",
               "Percent of all samples meeting criteria: ", round(d$pctmeets*100), "%"
               ))
    
    
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
                         "Click on a station on the map to view data", br()))
    maptabledata <- md2 %>% 
      select(input$checkboxtablesorter) %>% 
      filter(MLocID == input$map_marker_click$id)
    DT::datatable(
      data = maptabledata,
      style = 'bootstrap',
      extensions = 'Buttons',
      
      options = list(dom = 'Bfrtip',
                     pageLength = 10,
                     compact = TRUE,
                     nowrap = TRUE,
                     scrollX = TRUE,
                     buttons = c('excel', 'csv')
                     ),
      rownames = FALSE,
      filter = 'bottom'
    ) %>% 
      DT::formatDate("datetime", "toLocaleString")
  })
  
  
  
 # mapclickmd2 <-  reactive({
 #    md2 %>% 
 #      filter(MLocID == input$map_marker_click$id) %>% 
 #      group_by(month = floor_date(datetime, "month")) %>% 
 #      mutate(n_samples = n()) %>% 
 #      mutate(season = factor(month.abb[month(datetime)],
 #                             levels = c("May", "Jun", "Jul", "Aug", "Oct"))) %>% 
 #  })

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
              colors = viridis_pal()(5)) %>%
      layout(margin = list(b = 20),
             yaxis = list(title = 'Count'),
             xaxis = list(title = "Date",
                          range = c(min(md2$datetime), max(md2$datetime))),
             barmode = 'group')
  })  
  
  
  # overview plots----- 
  n_sum <- reactive({
    md2 %>% 
      select(MLocID, StationDes, Site, datetime, in_spawn, DO_lim, DO_status) %>% 
      group_by(month = floor_date(datetime, "month")) %>% 
      mutate(n_samples = n()) %>% 
      mutate(season = factor(month.abb[month(datetime)],
                             levels = c("May", "Jun", "Jul", "Aug", "Oct")))
  })


# summary --------------------------------------------------------
n2 <- reactive({
n_sum() %>%
    group_by(MLocID, month, StationDes, Site, in_spawn, DO_lim, DO_status) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    spread(DO_status, n, fill = 0) %>% 
    rename(over = "Meets criteria",
           under = "Excursion") %>% 
    mutate(month = as.factor(month),
           Spawning = recode(in_spawn, "0" = "Not in spawning",
                             "1" = "In spawning"),
           `DO Limit` = DO_lim)
})
  
  output$nstackplot <- renderPlotly({

    plot_ly(n2(),
            x = ~get(input$plottype)) %>% 
      add_trace(
            y = ~under,
            name = 'Excursion',
            marker = list(color = "#DB532A"),
            type = 'bar') %>%
      add_trace(y = ~over,
                name = 'Meets criteria',
                marker = list(color = 'rgb(49,130,189)'),
                type = 'bar') %>%
      layout(
        margin = list(
          l = 60,
          r = 20, 
          b = 100,
          t = 20,
          pad = 8
        ),
        yaxis = list(title = 'Count'),
        xaxis = list(title = toTitleCase(input$plottype)),
        barmode = 'group')
  
  })

  output$click <- renderPrint({
    pc <- event_data("plotly_click")
    if (is.null(pc)) "Click events appear here (double-click to clear)" else pc
  })
  
  
# min do plot ---------------------------------------------------------

 wdi <- reactive({
   md2 %>%
     select(MLocID, datetime, do, Site, StationDes) %>% 
     group_by(MLocID, Site, StationDes, month.p = floor_date(datetime, "month")) %>% 
     filter(Site %in% input$SiteCheckGroup) %>% 
     summarize(min = min(do))
 })

  
  output$mindoplot <- renderPlotly({
    req(wdi())
    plot_ly(wdi(),
            x = ~month.p,
            y = ~min,
            text = ~paste('Site:', StationDes),
            color = ~Site,
            colors = pal,
            # colors = coul,
            mode = "markers",
            type = "scatter",
            marker = list(size = 10,
                          line = list(color = "#000000",
                                      width = 0.2),
                          alpha = 0.8)) %>%
      layout(showlegend = FALSE,
             margin = marginlist,
             # paper_bgcolor = "#ecf0f1",
             # plot_bgcolor = "#ecf0f1",
             xaxis = list(title = "Date",
                          range = c("2007-01-01", "2017-01-31")),
             yaxis = list(title = "Minimum Dissolved Oxygen",
                          range = c(-1, 13))
             # ,
             # autosize = FALSE, width = 1000, height = 800
             )
  })

# boxplot -----------------------------------------------------------------

  
  boxplotdata <-  reactive({
    md2 %>% 
      mutate(month = floor_date(datetime, "month")) %>%
      mutate(season = factor(month.abb[month(datetime)],
                             levels = c("May", "Jun", "Jul", "Aug", "Oct"))) %>% 
      mutate(year = factor(year(floor_date(datetime, "year")))) %>%
      filter(crit_Instant == input$boxplotradio) %>% 
      filter(Site %in% input$boxplotsites)
  })
  
  output$summaryboxplot <- renderPlotly({
    
    plot_ly(boxplotdata(),
            x = ~get(input$boxplotx),
            y = ~do,
            color = ~get(input$boxplotgroup),
            # colors = viridis_pal(option = "D")(5), 
            type = "box",
            text = ~paste('Site: ', StationDes,
                          "<br>", DO_lim)) %>%
      layout(boxmode = "group")
  })
  
  output$summaryboxplot2 <- renderPlotly({

    plot_ly(boxplotdata(),
            x = ~season,
            y = ~do,
            color = ~MLocID, 
            colors = viridis_pal(option = "D")(5), type = "box",
            text = ~paste('Site: ', StationDes,
                          "<br>", DO_lim)) %>% 
      layout(boxmode = "group")
  })
  
  output$summaryboxplot3 <- renderPlotly({
    plot_ly(boxplotdata(),
            x = ~year,
            y = ~do,
            color = ~MLocID, 
            colors = viridis_pal(option = "D")(5), type = "box",
            text = ~paste('Site: ', StationDes,
                          "<br>", DO_lim)) %>%
      layout(boxmode = "group")

    })
  
  
  # Display Continuous  --------------------------------------------------------

  
  stations_subset <- reactive({
    req(input$station_selection)
    md2 %>% filter(Site %in% input$station_selection) %>% 
      select(-c(Lat_DD, Long_DD, LLID, RiverMile, Spawn_dates, SpawnStart, SpawnEnd, Site))
      # filter(datetime > input$daterange[1] & datetime < input$daterange[2])
    # %>% 
    #   filter(mdy(datetime) > mdy(input$daterange2[1]) & mdy(datetime) < mdy(input$daterange2[2]))
    
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

# DO Subplot -----------------------------------------------------------------

  output$subplot <- renderPlotly({
    DO_pal <- c("#000000", "#325C62", "#F4A767")
    DO_pal <- setNames(DO_pal, c("do", "Meets criteria", "Excursion"))

    a <- plot_ly(data = stations_subset(),
                 x = ~get(input$x),
                 y = ~do,
                 colors = DO_pal,
                 type = "scatter",
                 source = "A",
                 showlegend = FALSE) %>% 
      add_markers(color = ~DO_status,
                  colors = DO_pal,
                  showlegend = TRUE) %>% 
      layout(
        yaxis = list(
          title = "DO, mg/L"
        )
      )
    b <- plot_ly(data = stations_subset(),
                 x = ~get(input$x),
                 y = ~get(input$y2),
                 name = toTitleCase(input$y2),
                 title = toTitleCase(input$y2),
                 marker = list(color = "#3C3545"),
                 type = "scatter") %>% 
      layout(
        yaxis = list(
          title = toTitleCase(input$y2)
        )
      )
    c <- plot_ly(data = stations_subset(),
                 x = ~get(input$x),
                 y = ~get(input$y3),
                 name = toTitleCase(input$y3),
                 title = toTitleCase(input$y3),
                 marker = list(color = "#49805F"),
                 type = "scattergl") %>% 
      layout(
        yaxis = list(
          title = input$y3
        )
      )
    d <- plot_ly(data = stations_subset(),
                 x = ~get(input$x), 
                 y = ~get(input$y4),
                 name = toTitleCase(input$y4),
                 title = toTitleCase(input$y4),
                 marker = list(color = "#959B4F"),
                 type = "scattergl") %>% 
      layout(
        yaxis = list(
          title = toTitleCase(input$y4)
        )
      )
    sp <- subplot(a, b, c, d, nrows = 4, shareX = TRUE, titleY = TRUE)
    sp %>% 
      layout(
        # plot_bgcolor = "#ecf0f1",
        paper_bgcolor = "#ecf0f1",
        # dragmode = "select",
        xaxis = list(
          title = toTitleCase(input$x),
          rangeselector = list()
          # ,
          # rangeslider = list(type = "date")
          )
        # ,
        # autosize = FALSE, width = 1000, height = 800
        )
  } )

# graph to table ----------------------------------------------------------

  # creates main df from selection of do data joined back with other data
  
  plot.subset <- reactive({
    event.data <- event_data("plotly_selected", source = "A")
    if (is.null(event.data)) return(NULL)
    
    Meets_criteria <- subset(stations_subset(),
                             DO_status == "Meets criteria")[subset(event.data, curveNumber == 2)$pointNumber + 1,]
    Excursion <- subset(stations_subset(),
                        DO_status == "Excursion")[subset(event.data,
                                                         curveNumber == 1)$pointNumber + 1,]
    
    plot.subset <- rbind(Meets_criteria, Excursion)
    
    plot.subset
  })
  
  # creates mini df of summary of meets/fails do observations within selection window
  plot.summ <- reactive({
    
    plot.subset() %>%
      group_by(DO_status) %>%
      summarize(Count = n())
  })
  
  
  # main table of plotly data joined with other data
  output$graph_to_table <- DT::renderDT({
    req(plot.subset())
    
    DT::datatable(
      data = plot.subset(),
      style = "bootstrap",
      extensions = 'Buttons',
      options = list(dom = 'Bfrtip',
                     pageLength = 10,
                     compact = TRUE,
                     nowrap = TRUE,
                     scrollX = TRUE,
                     buttons = c('excel', 'csv', 'pdf')
      ),
      rownames = FALSE,
      filter = 'top'
    ) %>% 
      DT::formatDate("datetime", "toLocaleString")
    
  })
  
  
  
  

# plotly summary bar plot -------------------------------------------------------

  output$summaryplot <- renderPlotly({
    req(plot.summ())
    
    plot_ly(plot.summ(),
            x = ~DO_status,
            y = ~Count,
            type = "bar",
            color = ~color_map[DO_status]) %>%
      layout(title = "Count",
             paper_bgcolor = "#ecf0f1"
             # ,
             # plot_bgcolor = "#dee5ef"
             # ,
             # yaxis = list(domain = c(0, 1))
             )
  })
  
  #  mini table with summary data
  output$plot.summ <- DT::renderDT({
    req(plot.summ())
    plot.summ() %>% 
      mutate(Percent = round(Count/sum(Count)*100))
  })
  
  # output$meetscriteriapct <- renderUI({
  #   req(event.data)
  #   
  #   plot.summ %>% 
  #     mutate(pct = Count/sum(Count))
  #     
  # 
  #   
  #   HTML(
  #     paste(tags$h5(),
  #           "MLocID: ", d$MLocID, "<br/>"
  #           
  #     ))
  # 
  # 
  # })


# DO map ------------------------------------------------------------------

userdomd2 <- reactive({
    md2 
  # %>% 
  #     filter(DO_status %in% input$DOPassFailRadio)
  })
  
  
  
  
  output$DOmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri Satellite Map") %>%
      addProviderTiles("OpenTopoMap", group = "Open Topo Map") %>%
      addProviderTiles("CartoDB", group = "Carto") %>%
      addMarkers(data = userdomd2(),
                 lat = ~Lat_DD,
                 lng = ~Long_DD,
                 label = ~paste0("Station ID: ", MLocID, "
                                 Site Name: ", StationDes),
                 labelOptions = labelOptions(textOnly = FALSE),
                 clusterOptions = markerClusterOptions(),
                 layerId = userdomd2()$do) %>%
      addLayersControl(baseGroups = c("Esri Satellite Map",
                                      "Open Topo Map",
                                      "Carto"))
  })
  
  } #End Server

shinyApp(ui = ui, server = server)

