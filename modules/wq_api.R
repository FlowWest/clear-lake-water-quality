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
          }")
    ),
    tags$h3("Clear Lake Live Data "),
    div(
      style = "display: inline-block;vertical-align:top; width: 250px;",
      radioButtons(
        ns("monitoring_station"),
        label = "Monitoring Stations",
        choices = list(
          "Riviera Monitoring Sensor" = "riviera",
          "Oaks Monitoring Sensor" = "oaks"
        ),
        selected = "riviera"
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 300px;",
      uiOutput(ns("water_variable_select_ui"))
      ),
    div(
      style = "display: inline-block;vertical-align:top; width: 300px;",
      dateRangeInput(
        ns('dateRange'),
        label = 'Date range input: YYYY-MM-DD',
        start = Sys.Date() - 4,
        end = Sys.Date(),
        min = Sys.Date() - 30,
        max = Sys.Date()
      )
    ),
    # actionButton(ns("button"), "Apply Changes!"),
    plotlyOutput(ns("wq_plot")),
    br(),
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
  #create selection that returns monitoring device id
  
  get_station_parameters <-
    function(station_id) {
      device_url = paste0(base_url, "/", station_id, "/", "parameters?", apikey)
      device = GET(url = device_url)
      device_parameters <- fromJSON(rawToChar(device$content))
      parameters_df <- do.call(rbind.data.frame, device_parameters)
    }
  ns <- session$ns
  #Declarations
  riviera_id <- 2656
  oaks_id <- 2660
  # devices_full_url  = paste0(base_url, "?", apikey)
  #Access API
  # api_call = GET(url = devices_full_url)
  #Could check status_code or content
  #Content is in unicode - need to convert it to text and parse the JSON
  # devices <- fromJSON(rawToChar(api_call$content))
  # devices_df <- do.call(what = "rbind",
                        # args = lapply(devices, as.data.frame))
  # riviera_id <- devices_df['id'][1, ]
  # oaks_id <- devices_df['id'][2, ]
  # api_riviera_wq <- get_station_parameters(riviera_id)$name[13:20]
  # api_oaks_wq <- get_station_parameters(oaks_id)$name[13:24]
  #Create wq data dropdown based on station selected
  selected_station <- reactive({
    #Need to create a condition where
    req(input$monitoring_station)
    
    if (input$monitoring_station == "riviera") {
      #access api based on station code to find the monitoring variables
      api_riviera_wq <- get_station_parameters(riviera_id)$name[13:20]
      #map new wq variable to the variables from api 
      #apply function recode to the variables from api using the mapped variables
      #Display the recoded variables as the dropdown menu
      do.call(recode,
              c(
                list(api_riviera_wq),
                setNames(edited_riviera_wq, api_riviera_wq)
              ))
    } else if (input$monitoring_station == "oaks") {
      api_oaks_wq <- get_station_parameters(oaks_id)$name[13:24]
      do.call(recode,
              c(
                list(api_oaks_wq),
                setNames(edited_oaks_wq, api_oaks_wq)
              ))
    }
  })%>% bindCache(input$monitoring_station) 
  
  
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
      label = "Water Quality",
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
      if (monitoring_station == "riviera") {
        #create a key value pair where key is the wq variables and values are the api associated id
        riviera_vals <- get_station_parameters(riviera_id)$id[13:20]
        names(riviera_vals) <- edited_riviera_wq
        #call associated parameter id based wq variabe
        parameter_id <- riviera_vals[water_variable]
        data_url <- paste0(base_url,"/",riviera_id,"/","parameters/",parameter_id,
                 "/data?",apikey,"&from=",query_start_date,'%2010:00:00&to=',
                 query_end_date,"%2010:00:00")
      } else if (monitoring_station == 'oaks'){
        oaks_vals <- get_station_parameters(oaks_id)$id[13:24]
        names(oaks_vals) <- edited_oaks_wq
        parameter_id <- oaks_vals[water_variable]
        data_url <-paste0(base_url,"/",oaks_id,"/","parameters/",
            parameter_id,"/data?",apikey,"&from=",query_start_date,'%2010:00:00&to=',
            query_end_date,"%2010:00:00")
        }
        variable_json_data <- GET(url = data_url)
        variable_info <- fromJSON(rawToChar(variable_json_data$content))
        raw_data <- variable_info$data
        
    }
  dataInput <- reactive({
    req(input$water_variable)
    
    get_data(
      input$dateRange[1],
      input$dateRange[2],
      input$monitoring_station,
      input$water_variable
    ) %>%
      mutate_at('value', as.numeric) %>%
      mutate("timestamp" = ymd_hms(timestamp, tz = Sys.timezone()))
  }) 
    # bindCache(
    #   input$dateRange[1],
    #   input$dateRange[2],
    #   input$monitoring_station,
    #   input$water_variable
    # )
  
  #Visualization
  output$wq_plot <- renderPlotly({
    # req(input$water_variable, input$monitoring_station)
    
    #Check whether string starts with an opening bracket and closing bracket
    unit <- stringr::str_extract(string =input$water_variable,
                                 pattern ="(?<=\\().*(?=\\))")
    hover_label <-
      paste0("'<br>" , stringr::str_extract(string = input$water_variable, pattern = "^[^\\(]*"), "", 
    ": ")
    formatted_title <-
      paste(
        input$water_variable,
        "from",
        str_to_title(input$monitoring_station),
        "Monitoring Sensor at Clearlake"
      )
    dataInput() %>%
      plot_ly(
        x = ~ timestamp,
        y = ~ value,
        type = 'scatter',
        mode = 'lines',
        hoverinfo = 'text',
        text = ~ paste("<br> Time Stamp: ",
                       timestamp,
                       hover_label,
                       paste(value, unit))
      ) %>%
      layout(
        title = formatted_title,
        xaxis = list(title = 'Date'),
        yaxis = list(title = input$water_variable)
      ) %>% 
      plotly::config(displayModeBar = FALSE) %>% 
      plotly::config(showLink = FALSE)
  })
}
