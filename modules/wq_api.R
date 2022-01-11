library(shiny)
# library(httr)
library(httpcache)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinydashboard)

#function to create ui - it need id to represent server id objects
#user interface section
#name space needs to be different for each inidivudal user interface element
#ns is wrapped around the input ids to keep the namespace uniqe
#the namepace will append the module id to the input id
#a module server can contain many elements, these can include rendering outputs, observevents etc.
#you can use any variable provided in the function anywhere in the server section
#we could call the outputs that we set up in teh module server (ns) append the output id to the input id
wqdata_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style("body{
      height: 584px;
      width: 1005px;
      margin: auto;
          }"),
      tags$style(type='text/css', ".selectize-input { font-size: 14px; line-height: 23px;} 
      .selectize-dropdown { font-size: 14px; line-height: 28px; }")
    ),
    tags$h3("Realtime Monitoring "),
    div(
      style = "display: inline-block;vertical-align:top; width: 250px;",
      radioButtons(
        ns("monitoring_station"),
        label = h5("Monitoring Sensor"),
        choices = list(
          "Riviera West" = "riviera",
          "Clearlake Oaks" = "oaks"
        ),
        selected = "riviera"
      )
    ),
    div(style = "display: inline-block;vertical-align:top; width: 300px;",
        uiOutput(ns(
          "water_variable_select_ui"
        ))),
    div(
      style = "display: inline-block;vertical-align:top; width: 300px;",
      dateRangeInput(
        ns('dateRange'),
        label = h5('Date Range Input: YYYY-MM-DD'),
        start = Sys.Date() - 4,
        end = Sys.Date(),
        min = Sys.Date() - 90,
        max = Sys.Date()
      )
    ),
    br(),
    br(),
    tabsetPanel(
      tabPanel("Plot", plotlyOutput(ns("wq_plot"))),
      tabPanel("Sensor Map", leafletOutput(ns("sensor_map")))
    ),
    br(),
    tags$p(
      "Clear Lake is a natural freshwater lake in Lake County in
                  the U.S. state of California, north of Napa County and
                  San Francisco. It is the largest natural freshwater lake
                  wholly within the state, with 68 square miles (180 square kilometers) of
                  surface area. At 480,000 years, it is the oldest lake in
                  North America.[2] It is the latest lake to occupy a site
                  with a history of lakes stretching back at
                  least 2,500,000 years.[3] The data for Clear Lake is collected from two sensors,
                  one on the west side of Clear Lake (Riviera West), and another on the east side of 
                  Clear Lake (Clearlake Oaks).
                  The dashboard visualizes the hourly data of interest from WQData Live 
                  for the past 90 days. Use the pull-down menu to visualize the data, 
                  hover the mouse over the graph to find the value of a specific hour, and
                  drag the mouse over the chart to zoom in on the graph."
      ,style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
    )
  )
  
}

wq_data_server <- function(input, output, session) {
  #cache the parameters and save it as a vector
  ns <- session$ns
  #Declarations
  #put this in global.r file
  #same as function
  selected_station <- reactive({
    
    req(input$monitoring_station)
    
    if (input$monitoring_station == "riviera") {
      #access api based on station code to find the monitoring variables
      api_riviera_wq <-
        wq_parameters$name[1:8]
      #map new wq variable to the variables from api
      #apply function recode to the variables from api using the mapped variables
      #Display the recoded variables as the dropdown menu
      do.call(recode,
              c(
                list(api_riviera_wq),
                setNames(edited_riviera_wq, api_riviera_wq)
              ))
    } else if (input$monitoring_station == "oaks") {
      api_oaks_wq <- wq_parameters$name[9:20]
      do.call(recode,
              c(
                list(api_oaks_wq),
                setNames(edited_oaks_wq, api_oaks_wq)
              ))
    }
  })
  
  
  #create water variable selection ui
  output$water_variable_select_ui <- renderUI({
    #wait for this selection
    req(input$monitoring_station)
    #
    water_variable_choices <-
      selected_station()
    #function returns UI
    selectInput(
      ns("water_variable"),
      label = h5("Water Quality Indicator"),
      selected = NULL,
      choices = water_variable_choices,
      width = '250px',
      multiple = FALSE
    )
  })
  
  # Query function
  # Access device data parameters
  get_data <-
    function (query_start_date,
              query_end_date,
              monitoring_station,
              water_variable) {
      query_end_date <- query_end_date + 1
      if (monitoring_station == "riviera") {
        #create a key value pair where key is the wq variables and values are the api associated id
        riviera_vals <- get_station_parameters(riviera_id)$id[13:20]
        names(riviera_vals) <- edited_riviera_wq
        #call associated parameter id based wq variabe
        parameter_id <- riviera_vals[water_variable]
        data_url <-
          paste0(
            base_url,
            "/",
            riviera_id,
            "/",
            "parameters/",
            parameter_id,
            "/data?",
            apikey,
            "&from=",
            query_start_date,
            '%2008:00:00&to=',
            query_end_date,
            "%2008:00:00"
          )
      } else if (monitoring_station == 'oaks') {
        oaks_vals <- get_station_parameters(oaks_id)$id[13:24]
        names(oaks_vals) <- edited_oaks_wq
        parameter_id <- oaks_vals[water_variable]
        data_url <- paste0(
          base_url,
          "/",
          oaks_id,
          "/",
          "parameters/",
          parameter_id,
          "/data?",
          apikey,
          "&from=",
          query_start_date,
          '%2010:00:00&to=',
          query_end_date,
          "%2010:00:00"
        )
      }
      variable_json_data <- GET(url = data_url)
      variable_info <-
        fromJSON(rawToChar(variable_json_data$content))
      raw_data <- variable_info$data
      # View(raw_data)
      
    }
  dataInput <- reactive({
    req(input$water_variable,input$dateRange[1], input$dateRange[2])
    
    get_data(
      input$dateRange[1],
      input$dateRange[2],
      input$monitoring_station,
      input$water_variable
    ) %>%
      mutate_at('value', as.numeric) %>% 
      mutate("timestamp" = ymd_hms(timestamp) - hours(8),
             "date" = as.Date(timestamp))
  })
  
  #Visualization
  output$wq_plot <- renderPlotly({
    req(input$water_variable, input$dateRange[1], input$dateRange[2])
    
    #Check whether string starts with an opening bracket and closing bracket
    unit <- stringr::str_extract(string = input$water_variable,
                                 pattern = "(?<=\\().*(?=\\))")
    hover_label <-
      paste0(
        "<br>" ,
        stringr::str_extract(string = input$water_variable, pattern = "^[^\\(]*"),
        ": "
      )
    formatted_title <-
      paste(
        input$water_variable,
        "from",
        str_to_title(input$monitoring_station),
        "Monitoring Sensor at Clear Lake"
      )
    dataInput() %>%
      plot_ly(
        x = ~ timestamp,
        y = ~ value,
        type = 'scatter',
        mode = 'lines',
        hoverinfo = 'text',
        text = ~ paste(
          "<br>Time: ",
          paste(format(timestamp, format ="%H:%M"), "PST"),
          "<br>Date: ",
          date,
          hover_label,
          paste(value, unit)
        )
      ) %>%
      layout(
        title = (list(text = formatted_title, y = 0.97)),
        xaxis = list(title = 'Date'),
        yaxis = list(title = input$water_variable)
      ) %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::config(showLink = FALSE)
  })%>% bindCache(input$dateRange[1], input$dateRange[2],input$water_variable)
  
  selected_sensor <- reactive({
    monitoring_stations %>% 
      filter(station_name == input$monitoring_station)
  })
  
  output$sensor_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>% 
      addCircleMarkers(data = monitoring_stations,
                       radius = 6,
                       fillOpacity =.4,
                       weight =2,
                       color = "#2e2e2e",
                       fillColor = "#555555",
                       opacity = .4) %>% 
      setView(lng = -122.708927, lat= 39.000284, zoom = 12)
  })
  observeEvent(input$water_variable,{
    leafletProxy("sensor_map") %>%
      clearGroup("selected_sensor") %>%
      addCircleMarkers(data = selected_sensor(),
                       fillOpacity = .8,
                       weight = 2,
                       color = "#2e2e2e",
                       fillColor = "#28b62c",
                       opacity = 1,
                       group = "selected_sensor")
      # setView(zoom = 4)
  })
}
