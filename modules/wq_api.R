library(shiny)
library(httr)
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
          }")
    ),
    tags$h3("Clear Lake Live Data "),
    radioButtons(
      ns("stations"),
      label = h4("Monitoring Stations"),
      choices = list("Riviera Monitoring Station" = "riviera",
                     "Oaks Monitoring Station" = "oaks"),
      selected = "riviera"
    ),
    selectInput(
      ns("water_variable"),
      label = h4("Water Quality"),
      choices = list("Surface Temperature" = "temperature_(C)",
                     "Surface Conductivity" = "conductivity_(uS/cm)",
                     "Surface Optical Dissolved Oxygen (Percent Air Saturation)" = "dissolved_oxygen_(%)",
                     "Surface Optical Dissolved Oxygen (mg/L)" = "dissolved_oxygen_(mg/L)",
                     "Surface Chlorophyll (ug/L)" = "chlorophyll_(ug/L)",
                     "Surface Chlorophyll (RFU)" = "chlorophyll_(RFU)",
                     "Surface Phycocyanin (ug/L)" = "phycocyanin_(ug/L)",
                     "Surface Pycocyanin (RFU)" = "phycocyanin_(RFU)"),
      selected = "temperature_(C)",
      multiple = FALSE
    ),
    dateRangeInput(
      ns('dateRange'),
      label = 'Date range input: yyyy-mm-dd',
      start = Sys.Date() - 4,
      end = Sys.Date(),
      min = Sys.Date() - 30,
      max = Sys.Date()
    ),
    plotlyOutput(ns("temp_plot")),
    tags$p(
      "Clear Lake is a natural freshwater lake in Lake County in
                  the U.S. state of California, north of Napa County and
                  San Francisco. It is the largest natural freshwater lake
                  wholly within the state, with 68 square miles (180 km2) of
                  surface area. At 480,000 years, it is the oldest lake in
                  North America.[2] It is the latest lake to occupy a site
                  with a history of lakes stretching back at
                  least 2,500,000 years.[3] The dashboard above is built with WQdata Live API.
                  It shows the hourly data of interest for the current month. 
      Use pull down menu to visualize the data, hover the mouse over the graph to find value of specific hour, and 
      drag the mouse over the graph to zoom in on the graph.
      "
    )
  )
  
}

wq_data_server <- function(input, output, session) {
  ns <- session$ns
  #Declarations
  devices_full_url  = paste0(base_url, "?", apikey)
  #Access API
  api_call = GET(url = devices_full_url)
  #Could check status_code or content
  #Content is in unicode - need to convert it to text and parse the JSON
  devices <- fromJSON(rawToChar(api_call$content))
  
  devices_df <- do.call(what = "rbind",
                        args = lapply(devices, as.data.frame))
  riviera_id <- devices_df['id'][1, ]
  clearlake_oaks_id <- devices_df['id'][2, ]
  riviera_device_url = paste0(base_url, "/", riviera_id, "/", "parameters?", apikey)
  riviera_device = GET(url = riviera_device_url)
  riviera_parameters <- fromJSON(rawToChar(riviera_device$content))
  
  
  
  #Access device data parameters
  #Query function"Surface Temperature" = "temperature_(C)",
  # "Surface Conductivity" = "conductivity_(uS/cm)",
  # "Surface Optical Dissolved Oxygen (Percent Air Saturation)" = "dissolved_oxygen_(%)",
  # "Surface Optical Dissolved Oxygen (mg/L)" = "dissolved_oxygen_(mg/L)",
  # "Surface Chlorophyll (ug/L)" = "chlorophyll_(ug/L)",
  # "Surface Chlorophyll (RFU)" = "chlorophyll_(RFU)",
  # "Surface Phycocyanin (ug/L)" = "phycocyanin_(ug/L)",
  # "Surface Pycocyanin (RFU)" = "phycocyanin_(RFU)"),
  get_data <-
    function (query_start_date,
              query_end_date,
              water_variable) {
      # if station == "riviera"{
        # riviera_id <- devices_df['id'][1, ]
      # }
      if (water_variable == "temperature_(C)") {
        parameter_id <- 53286
      } else if (water_variable == "conductivity_(uS/cm)"){
        parameter_id <- 53287
      } else if (water_variable == "dissolved_oxygen_(%)"){
        parameter_id <- 53288
      } else if (water_variable == "dissolved_oxygen_(mg/L)"){
        parameter_id <- 53289
      } else if (water_variable == "chlorophyll_(ug/L)"){
        parameter_id <- 53290
      } else if (water_variable == "chlorophyll_(RFU)"){
        parameter_id <- 53291
      } else if (water_variable == "phycocyanin_(ug/L)"){
        parameter_id <- 53292
      } else if (water_variable == "phycocyanin_(RFU)"){
        parameter_id <- 53293
      }
      riviera_surface_data_url <-
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
          '%2010:00:00&to=',
          query_end_date,
          "%2010:00:00"
        )
      riviera_surface_data <- GET(url = riviera_surface_data_url)
      riviera_surface <-
        fromJSON(rawToChar(riviera_surface_data$content))
      raw_data <- riviera_surface$data
    }
  dataInput <- reactive({
    get_data(input$dateRange[1], input$dateRange[2], input$water_variable) %>%
      mutate_at('value', as.numeric) %>%
      mutate("timestamp" = ymd_hms(timestamp, tz = Sys.timezone()))
  }) %>% 
    bindCache(input$dateRange[1], input$dateRange[2], input$water_variable)

    #Visualization
  output$temp_plot <- renderPlotly({
    
    req(input$water_variable)
    
    format_data <- function(
      data_input){
      str_to_title(gsub("_", " ", data_input))
    }
    
    hover_label <- format_data(paste("'<br>" ,input$water_variable, ": "))
    formatted_title <- format_data(paste(input$water_variable, "from Riviera West Sensor at Clearlake"))
    y_label <- format_data(input$water_variable )
    dataInput() %>%
      plot_ly(
        x = ~ timestamp,
        y = ~ value,
        type = 'scatter',
        mode = 'lines',
        hoverinfo = 'text',
        text = ~ paste(
          "<br> Time Stamp: ",
          timestamp,
          hover_label,
          value
        )
      ) %>%
      layout(
        title = formatted_title,
        xaxis = list(title = 'Date'),
        yaxis = list(title = y_label)
      )
  })
}

