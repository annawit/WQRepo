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
# library(rgdal)
library(tools)
library(leaflet.extras)


# Data -------------------------------------------------------


load("data/dataforwqapp.Rdata")



md2 <- dta1 %>% 
  rename(`Station Description` = StationDes,
         `Sample Time` = datetime,
         Temp = temp,
         `Temp Grade` = grade_temp,
         pH = ph,
         `pH Grade` = grade_ph,
         Conductivity = cond,
         `Conductivity Grade` = grade_cond,
         DO = do,
         `DO Grade` = grade_do,
         `DO Sat` = do_sat,
         `DO Sat Grade` = grade_do_sat,
         `Data Source` = data_source,
         Lat = Lat_DD,
         Long = Long_DD,
         Type = MonLocType)

#gives percent meeting criteria at each site for leaflet map
meets <- md2 %>% 
  group_by(MLocID, DO_status) %>%
  count() %>%
  group_by(MLocID) %>%
  mutate(pctmeets = n/sum(n)) %>%
  filter(DO_status == "Meets criteria") %>%
  mutate(pctbin = cut(pctmeets, c(0, 0.5, 0.99, 1),
                      include.lowest = TRUE,
                      labels = c("Fewer than 50% meet criteria",
                                 "50%-99% meet criteria",
                                 "Greater than 99% meet criteria")))

pctcolor <- colorFactor(palette = "RdYlGn", meets$pctbin)

load("data/sitesummary.Rdata")

s <- left_join(sites, meets, by = "MLocID")

marginlist <- list(
  l = 60,
  r = 20, 
  b = 50,
  t = 20,
  pad = 8
)

pal <- viridis_pal(option = "D", direction = -1)(14)
pal <- setNames(pal, unique(md2$Site))


#colors
coul <- colorRampPalette(brewer.pal(9, "Set3"))(14)
sitecol <- viridis_pal(option = "D")(14)

color_map <- c("Meets criteria" = "#F4A767", "Excursion" = "#DB532A")
DO_status_levels <- c("Meets criteria", "Excursion")
DO_status_colors <- c("#F4A767", "#DB532A")
names(DO_status_colors) <- DO_status_levels


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  navbarPage(
    "Continuous Dissolved Oxygen Visualizer",
    
# Select from map --------------------------------------------------------------
tabPanel("Select from map",
         sidebarLayout(
           # puts sidebar on the right side
           position = "right",
           sidebarPanel(
             wellPanel(
               # Station summary text
               uiOutput("youhavechosen"),
               # hr adds a subtle line and space in the UI
               hr(),
               # this is the sample counts bar plot
               plotlyOutput("mininplot")),
             wellPanel(
               # These are the table columns selector
               checkboxGroupButtons(
                 inputId = "checkboxtablesorter",
                 label = "Select table columns to display:",
                 choices = names(md2),
                 selected = c("MLocID", "Sample Time", "Temp", "pH",
                              "DO", "Conductivity",
                              "Station Description"))),
             wellPanel(
               HTML("Select a station and variables,
                    then hit 'Download data'."),
               hr(),
               downloadButton(
                 # rest of the code in the server
                 outputId = "download_data",
                 label = "Download table data")
             )
           ),
           mainPanel(
             wellPanel(leafletOutput("map")),
             wellPanel(DT::dataTableOutput("table"),
                       br(),
                       br())
           )
         )
         ), #end "Select from map" panel
    
# Overview plots --------------------------------------------------------------
tabPanel("Overview Plots",
# Sample summary ------------------------------------------------------------
tabsetPanel( #creates panels inside of "Overview plots"
  tabPanel("Sample summary",
           br(), # a space to make it pretty
           sidebarLayout(
             sidebarPanel(
               width = 3, # makes the sidebar panel smaller to make the plot bigger
               selectInput(inputId = "plottype",
                           "Select:", 
                           # "What the user sees in the menu" = "the column in the data"
                           choices = c("By time" = "month",
                                       "By site" = "MLocID",
                                       "By spawning" = "Spawning",
                                       "By DO limit" = "DO Limit")
               )),
             mainPanel(
               wellPanel(
                 # this is the sample summary plot
                 # it shows number of samples that meet criteria/are excursions
                 # a lot more user selection could be added here
                 plotlyOutput("nsummaryplot", height = 500)
               )
             )
           )
  ),
# Minimum DO ----------------
  tabPanel("Minimum DO",
           br(),
           sidebarLayout(
             sidebarPanel(
               width = 3,
               pickerInput(inputId = "SiteCheckGroup", 
                           label = h3("Select sites:"), 
                           choices = list(
                             # This creates sub-lists in the dropdown menu
                             # I'm sorry for this hacky way I did this
                             # If I have time I'll come back and fix it
                             Estuarine = c("13431-ORDEQ Trask River at Netarts Road (Hwy. 6)","34440-ORDEQ Hall Slough at Goodspeed Road (Tillamook, OR)","10523-ORDEQ Nestucca R at Cloverdale", "13421-ORDEQ Wilson River at Hwy 101", "11856-ORDEQ Nehalem River at Foley Road (Roy Creek Campground)"),
                             Freshwater = c("22394-ORDEQ Nestucca River at first bridge ramp (upstream of Beaver)", "21800-ORDEQ Nestucca River at River Mile 38.57", "13428-ORDEQ Dougherty Slough at Hwy 101", "13429-ORDEQ Dougherty Slough at Wilson River Loop Road (Tillamook)", "13430-ORDEQ Hoquarten Slough at Hwy 101 (Tillamook)", "23509-ORDEQ Nehalem River downstream of Humbug Creek at Lower Nehalem Road", "29302-ORDEQ Nehalem River at Spruce Run Creek", "13368-ORDEQ Nehalem River at River Mile 15.0", "29292-ORDEQ Nehalem River at Salmonberry River")
                           ),
                           selected = "13430-ORDEQ Hoquarten Slough at Hwy 101 (Tillamook)",
                           multiple = TRUE
               )
             ),
             mainPanel(
               width = 9,
               wellPanel(
                 # writes text directly into the interface at heading 4 size
                 h4("Minimum DO at a given site for a given sampling window"),
                 plotlyOutput("mindoplot", height = 500)
               )
             )
           )
  ),
  
# Boxplots ------------------------------------------------------------
  
  tabPanel("Boxplots",
           br(),
           sidebarLayout(
             sidebarPanel(
               width = 3,
               selectInput("boxplotx", # shiny id to use in other places
                           "Select x-axis:", # label the user sees
                           choices = c("Site" = "MLocID", # options
                                       "Season" = "season", # "Pretty for the User" = "colName"
                                       "Year" = "year")
                           ), 
               
               selectInput("boxplotgroup",
                           "Select color:",
                           choices = c("Site" = "MLocID",
                                       "Season" = "season",
                                       "Year" = "year")),
               radioButtons("boxplotradio", # shiny id to use in other places
                            "Water body type", # label the user sees
                            c("Estuary sites" = "6.5", # "Pretty for the User" = "colName"
                              "Freshwater sites" = "8")),
               pickerInput(inputId = "boxplotestsites", 
                           label = "Select sites:", 
                           choices = list(
                             Estuarine = 
                               c("13431-ORDEQ Trask River at Netarts Road (Hwy. 6)", 
                                 "34440-ORDEQ Hall Slough at Goodspeed Road (Tillamook, OR)",
                                 "10523-ORDEQ Nestucca R at Cloverdale", 
                                 "13421-ORDEQ Wilson River at Hwy 101", 
                                 "11856-ORDEQ Nehalem River at Foley Road (Roy Creek Campground)"),
                             Freshwater = 
                               c("22394-ORDEQ Nestucca River at first bridge ramp (upstream of Beaver)", 
                                 "21800-ORDEQ Nestucca River at River Mile 38.57", 
                                 "13428-ORDEQ Dougherty Slough at Hwy 101", 
                                 "13429-ORDEQ Dougherty Slough at Wilson River Loop Road (Tillamook)", 
                                 "13430-ORDEQ Hoquarten Slough at Hwy 101 (Tillamook)", 
                                 "23509-ORDEQ Nehalem River downstream of Humbug Creek at Lower Nehalem Road", 
                                 "29302-ORDEQ Nehalem River at Spruce Run Creek", 
                                 "13368-ORDEQ Nehalem River at River Mile 15.0", 
                                 "29292-ORDEQ Nehalem River at Salmonberry River")
                           ),
                           selected = unique(md2$Site),
                           multiple = TRUE
                           )
             ),
             mainPanel(
               width = 9,
               wellPanel(
                 plotlyOutput("summaryboxplot")
               )
             )
           )
  ))
    ),
    
# Display Continuous  --------------------------------------------------------------
    
tabPanel( "Display Continuous Data",
  fluidPage(
    tabsetPanel(
      tabPanel("Plots",
               br(),
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   pickerInput(inputId = "station_selection", 
                               label = h3("Select sites:"), 
                               choices = list(
                                 Estuarine = 
                                   c("13431-ORDEQ Trask River at Netarts Road (Hwy. 6)", 
                                     "34440-ORDEQ Hall Slough at Goodspeed Road (Tillamook, OR)",
                                     "10523-ORDEQ Nestucca R at Cloverdale", 
                                     "13421-ORDEQ Wilson River at Hwy 101", 
                                     "11856-ORDEQ Nehalem River at Foley Road (Roy Creek Campground)"),
                                 Freshwater = 
                                   c("22394-ORDEQ Nestucca River at first bridge ramp (upstream of Beaver)", 
                                     "21800-ORDEQ Nestucca River at River Mile 38.57", 
                                     "13428-ORDEQ Dougherty Slough at Hwy 101", 
                                     "13429-ORDEQ Dougherty Slough at Wilson River Loop Road (Tillamook)", 
                                     "13430-ORDEQ Hoquarten Slough at Hwy 101 (Tillamook)", 
                                     "23509-ORDEQ Nehalem River downstream of Humbug Creek at Lower Nehalem Road", 
                                     "29302-ORDEQ Nehalem River at Spruce Run Creek", 
                                     "13368-ORDEQ Nehalem River at River Mile 15.0", 
                                     "29292-ORDEQ Nehalem River at Salmonberry River")
                               ),
                               multiple = FALSE),
                   selectInput(
                     inputId = "x", # the shiny id to use other places
                     label = "X-axis", # what the user sees in the UI
                     choices = c("Sample Time", "Temp", "Temp Grade", "pH",
                                "pH Grade",
                                "Conductivity",
                                "Conductivity Grade",
                                "DO Grade", "DO Sat", "DO Sat Grade", "Data Source"),
                     selected = "Sample Time" # what I want it to start on by default
                   ),
                   selectInput(
                     inputId = "y2",
                     label = "2nd y-axis",
                     choices = c("Sample Time", "Temp", "Temp Grade", "pH",
                                 "pH Grade", "Conductivity", "Conductivity Grade",
                                 "DO Grade", "DO Sat", "DO Sat Grade", "Data Source"),
                     selected = "Temp"
                   ),
                   selectInput(
                     inputId = "y3",
                     label = "3rd y-axis",
                     choices = c("Sample Time", "Temp", "Temp Grade", "pH",
                                 "pH Grade", "Conductivity", "Conductivity Grade",
                                 "DO Grade", "DO Sat", "DO Sat Grade", "Data Source"),
                     selected = "Conductivity"
                   ),
                   selectInput(
                     inputId = "y4",
                     label = "4th y-axis",
                     choices = c("Sample Time", "Temp", "Temp Grade", "pH",
                                 "pH Grade", "Conductivity", "Conductivity Grade",
                                 "DO Grade", "DO Sat", "DO Sat Grade", "Data Source"),
                     selected = "pH"
                   ),
                   dateRangeInput(inputId = 'daterange',
                                  label = "Select dates:",
                                  start = min(md2$`Sample Time`),
                                  end = max(md2$`Sample Time`) - 1,
                                  min = min(md2$`Sample Time`), max = "2016-12-31",
                                  separator = " to ", format = "mm/dd/yy",
                                  startview = 'year', weekstart = 0
                   )
                 ),
                 mainPanel(
                   fluidRow(
                     column(8,
                            wellPanel(
                              # subplot is the large plot with four panels
                              # on the display continuous data page
                              plotlyOutput("subplot", height = 800),
                              DT::dataTableOutput("stations_subset")
                            )
                     ),
                     column(4,
                            wellPanel(
                              # the mini plot that pops up when the subplot
                              # to the left is selected
                             plotlyOutput("summaryplot"),
                              hr(),
                             # a mini table that summarizes number of samples 
                             #meeting /not meeting criteria and %
                              DT::dataTableOutput("plot.summ")
                            )
                     )
                   )
                 ))
      ),
      # ,
      tabPanel("Data Selected From Graph, in a Table",
               # when the data on the subplot is selected, the data in this tab
               # is populated
               DT::dataTableOutput("graph_to_table"))
    )
  )
)
  )
) # End Fluid Page & UI

# Server ------------------------------------------------------------------
server <- function(input, output, session) {

# Leaflet map --------------------------------------------------------
  
  # answer at bottom super helpful coloring circle markers
  # https://stackoverflow.com/questions/32940617/change-color-of-leaflet-marker
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri Satellite Map") %>%
      addProviderTiles("OpenTopoMap", group = "Open Topo Map") %>%
      addProviderTiles("CartoDB", group = "Carto") %>%
      addCircleMarkers(data = s,
                 lat = ~Lat,
                 lng = ~Long,
                 color = ~pctcolor(pctbin),
                 radius = ~(n.y^(3/9)), #takes the cube root of the number of samples
                 # in order to size the circles properly
                 # i left it as 3/9 in case they needed to be resized in the future
                 fillOpacity = 0.5,
                 label = ~paste0("Station ID: ", MLocID, " ", `Station Description`),
                 labelOptions = labelOptions(textOnly = FALSE),
                 layerId = s$MLocID) %>%
      addLegend("bottomright",
                pal = pctcolor,
                values = meets$pctbin,
                title = "Percent of Samples at a Site Meeting Criteria") %>%
      addResetMapButton() %>% 
      addLayersControl(baseGroups = c("Esri Satellite Map",
                                      "Open Topo Map",
                                      "Carto"))
  })
  
  output$youhavechosen <- renderUI({
    req(input$map_marker_click$id) #intended to suppress errors
    
    #filters the sites based on the leaflet map click
    d <- s %>% 
      filter(MLocID == input$map_marker_click$id)
    
    #prints out station information based on the map click
    HTML(paste(tags$h4(d$`Station Description`),
               "MLocID: ", d$MLocID, "<br/>",
               "Lat/long: ", round(d$Lat, 4), ", ", round(d$Long, 4), "<br/>",
               "River mile: ", round(d$RiverMile, 2), "<br/>",
               "Spawn dates: ", d$Spawn_dates, "<br/>",
               "Number of samples: ", scales::comma(d$n.x), "<br/>",
               "Percent of all samples meeting criteria: ", round(d$pctmeets*100), "%"
               ))

  })
  
  
  #download button on the first page
  output$download_data <- downloadHandler(
    
    filename = "ShinyDataTableDownload.csv", #name the user sees when they download
    content = function(file) {
      #sends data filtered by the column selector checkboxes
      #and filtered by site from the map click
      m <- md2 %>%
        select(input$checkboxtablesorter) %>%
        filter(MLocID == input$map_marker_click$id)
      write.csv(m, file)
    })
  

# table -------------------------------------------------------------------
  
  maptabledata <- reactive({
    md2 %>% 
      select(input$checkboxtablesorter) %>% 
      filter(MLocID == input$map_marker_click$id)
  })
  
  output$table <- DT::renderDataTable({
    shiny::validate(need(!is.null(input$map_marker_click$id),
                         "Click on a station on the map to view data", br()))
    DT::datatable(
      data = maptabledata(),
      style = 'bootstrap',
      extensions = 'Buttons',
      
      options = list(dom = 'Bfrtilp',
                     pageLength = 10,
                     compact = TRUE,
                     nowrap = TRUE,
                     scrollX = TRUE,
                     buttons = c('excel', 'csv')
                     ),
      rownames = FALSE,
      filter = 'bottom'
    ) %>% 
      DT::formatDate("Sample Time", "toLocaleString")
    
  }, server = FALSE)
  

  

## mini nplot --------------------------------------------------------------

  output$mininplot <- renderPlotly({
    shiny::validate(need(!is.null(input$map_marker_click$id),
                         "Click on a station on the map to view data"))
    md2 %>% 
      filter(MLocID == input$map_marker_click$id) %>% 
      group_by(month = floor_date(`Sample Time`, "month")) %>% 
      mutate(n_samples = n()) %>% 
      mutate(season = factor(month.abb[month(`Sample Time`)],
                             levels = c("May", "Jun", "Jul", "Aug", "Oct"))) %>% 
      plot_ly(x = ~month,
              y = ~n_samples,
              type = "bar",
              color = ~season,
              colors = viridis_pal()(5)) %>%
      layout(margin = list(b = 20),
             yaxis = list(title = 'Count'),
             xaxis = list(title = "Date",
                          range = c(min(md2$`Sample Time`), max(md2$`Sample Time`))),
             barmode = 'group')
  })  
  
  
  # Overview Plots----- 
  n_sum <- reactive({
    md2 %>% 
      select(MLocID, `Station Description`, Site, `Sample Time`, in_spawn, DO_lim, DO_status) %>% 
      group_by(month = floor_date(`Sample Time`, "month")) %>% 
      mutate(n_samples = n()) %>% 
      mutate(season = factor(month.abb[month(`Sample Time`)],
                             levels = c("May", "Jun", "Jul", "Aug", "Oct")))
  })


# summary --------------------------------------------------------
n2 <- reactive({
n_sum() %>%
    group_by(MLocID, month, `Station Description`, Site, in_spawn, DO_lim, DO_status) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    spread(DO_status, n, fill = 0) %>% 
    rename(over = "Meets criteria",
           under = "Excursion") %>% 
    mutate(month = as.factor(month),
           Spawning = recode(in_spawn, "0" = "Not in spawning",
                             "1" = "In spawning"),
           `DO Limit` = recode_factor(DO_lim, "6.5" = "6.5 (Estuarine)", "8" = "8 (Cold Water - Aquatic Life)",
                               "11" = "11 (Spawning)")
    )
})
  
  output$nsummaryplot <- renderPlotly({

    plot_ly(n2(),
            x = ~get(input$plottype)) %>% 
      add_trace(y = ~over,
                name = 'Meets criteria',
                marker = list(color = 'rgb(49,130,189)'),
                type = 'bar') %>%
      add_trace(
            y = ~under,
            name = 'Excursion',
            marker = list(color = "#DB532A"),
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
   req(input$SiteCheckGroup)
   
   md2 %>%
     select(MLocID, `Sample Time`, DO, Site) %>% 
     group_by(MLocID, Site, month.p = floor_date(`Sample Time`, "month")) %>% 
     filter(Site %in% input$SiteCheckGroup) %>% 
     summarize(min = min(DO))
 })

  
  output$mindoplot <- renderPlotly({
    req(wdi())
    plot_ly(wdi(),
            x = ~month.p,
            y = ~min,
            text = ~paste("Site:", Site),
            color = ~Site,
            colors = pal,
            # colors = coul,
            mode = "markers",
            type = "scatter",
            marker = list(size = 10,
                          line = list(color = "#000000",
                                      width = 0.4),
                          alpha = 0.8)) %>%
      layout(showlegend = FALSE,
             margin = marginlist,
             # paper_bgcolor = "#ecf0f1",
             # plot_bgcolor = "#ecf0f1",
             xaxis = list(title = "Date",
                          range = c("2007-01-01", "2017-01-31")),
             yaxis = list(title = "Minimum Dissolved Oxygen, mg/L",
                          range = c(-1, 13))
             # ,
             # autosize = FALSE, width = 1000, height = 800
             )
  })

# boxplot -----------------------------------------------------------------

  
  boxplotdata <-  reactive({
    md2 %>% 
      mutate(month = floor_date(`Sample Time`, "month")) %>%
      mutate(season = factor(month.abb[month(`Sample Time`)],
                             levels = c("May", "Jun", "Jul", "Aug", "Oct"))) %>% 
      mutate(year = factor(year(floor_date(`Sample Time`, "year")))) %>%
      filter(crit_Instant == input$boxplotradio) %>%
      filter(Site %in% c(input$boxplotfreshsites, input$boxplotestsites))
  })
  
  output$summaryboxplot <- renderPlotly({
    
    plot_ly(boxplotdata(),
            x = ~get(input$boxplotx),
            y = ~DO,
            color = ~get(input$boxplotgroup),
            # colors = viridis_pal(option = "D")(5), 
            type = "box",
            text = ~paste('Site: ', `Station Description`,
                          "<br>", DO_lim)) %>%
      layout(boxmode = "group",
             xaxis = list(title = toTitleCase(input$boxplotx)))
  })
  
  
  # Display Continuous  --------------------------------------------------------

  
  stations_subset <- reactive({
    req(input$station_selection)
    m <- paste0("Conductivity in ", intToUtf8(0x03BC), "S")
    md2 %>% filter(Site %in% input$station_selection) %>% 
      select(-c(Lat, Long, LLID, RiverMile, Spawn_dates, SpawnStart, SpawnEnd, Site)) %>% 
      mutate(paste0("Conductivity in ", intToUtf8(0x03BC), "S") = Conductivity)
  })
  
 output$stations_subset <- DT::renderDataTable({
    stations_subset()
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

# Multi-plot -----------------------------------------------------------------
# This plot contains multiple y axes that share the `Sample Time` axis
  
  
  output$subplot <- renderPlotly({
    shiny::validate(
      need(input$station_selection,
      "Select a station to the left to view data"))
    
    DO_pal <- c("#000000", "#3182bd", "#DB532A")
    DO_pal <- setNames(DO_pal, c("DO", "Meets criteria", "Excursion"))
    

    a <- plot_ly(data = stations_subset(),
                 x = ~get(input$x),
                 y = ~DO,
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
                 type = "scatter") %>% 
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
                 type = "scatter") %>% 
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
        ) %>% 
      config(displaylogo = FALSE,
             collaborate = FALSE,
             modeBarButtonsToRemove = list(
               'sendDataToCloud',
               # 'zoom2d',
               'pan2d',
               # 'select2d',
               # 'lasso2d',
               'zoomIn2d',
               'zoomOut2d',
               'autoScale2d',
               # 'resetScale2d',
               'toggleSpikelines'
             ))
  } )

# graph to table ----------------------------------------------------------

  # creates main df from selection of do data joined back with other data
  
  plot.subset <- reactive({
    event.data <- event_data("plotly_selected", source = "A")
    if (is.null(event.data)) return(NULL)
    
    Meets_criteria <- subset(
      stations_subset(),
      DO_status == "Meets criteria")[subset(event.data,
                                            curveNumber == 2)$pointNumber + 1,]
    Excursion <- subset(
      stations_subset(),
      DO_status == "Excursion")[subset(event.data,
                                       curveNumber == 1)$pointNumber + 1,]
    
    plot.subset <- rbind(Meets_criteria, Excursion)
    
    plot.subset
  })
  
  # creates mini df of summary of meets/fails do observations within selection window
  plot.summ <- reactive({
    req(plot.subset())
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
      DT::formatDate("Sample Time", "toLocaleString")
    
  })
  
  
  
  

# plotly summary bar plot -------------------------------------------------------

  output$summaryplot <- renderPlotly({
    shiny::validate(
      need(plot.subset(),
           "Select a station to the left to view data"))
    
    req(plot.subset())
    
    DO_pal <- c("#000000", "#3182bd", "#DB532A")
    DO_pal <- setNames(DO_pal, c("DO", "Meets criteria", "Excursion"))
    
    plot_ly(plot.summ(),
            x = ~DO_status,
            y = ~Count,
            type = "bar",
            name = ~DO_status,
            colors = DO_pal,
            color = ~DO_status) %>%
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

  
  } #End Server

shinyApp(ui = ui, server = server)

