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
#you can use any variable provided in teh function anywhere in teh server section
#we could call the outputs that we set up in teh module server (ns) append the output id to the input id 
wqdata_ui <- function(id){

  ns <- NS(id)

  tagList(      tags$head(
    tags$style(
      "body{
      height: 584px;
      width: 1005px;
      margin: auto;
          }"
    )
  ),
  column(width = 3,
         tags$h3("Clear Lake Live Data"),
         tags$p("Clear Lake is a natural freshwater lake in Lake County in
                  the U.S. state of California, north of Napa County and
                  San Francisco. It is the largest natural freshwater lake
                  wholly within the state, with 68 square miles (180 km2) of
                  surface area. At 480,000 years, it is the oldest lake in
                  North America.[2] It is the latest lake to occupy a site
                  with a history of lakes stretching back at
                  least 2,500,000 years.[3] The dashboard on the right shows the
                live data of interest for the current month"),
         checkboxGroupInput(ns("water_variable"), 
                            label = h4("Water Quality Data"),
                            choices = list("Temperature" = "temperature",
                                           "Surface Chlorophyll" = "chlorophyll"),
                            selected = "temperature"
         ),
         dateRangeInput(ns('dateRange'),
                        label = 'Date range input: yyyy-mm-dd',
                        start = Sys.Date() - 4, end = Sys.Date(),
                        min = Sys.Date() - 30, 
                        max = Sys.Date()
         )
         # textOutput(ns("dateRangeText"))
  ),
  column(width = 9,
         plotlyOutput(ns("temp_plot")),
         plotlyOutput(ns("chlorophyll_plot")),
         verbatimTextOutput(ns("dateRangeText"))
         # verbatimTextOutput(ns("variableText"))
         # plotlyOutput(ns("lake_boxplot"))
  )
  )

}

wq_data_server <- function(input, output, session) {
  ns <- session$ns
  #Declarations
  devices_full_url  = paste0(base_url, "?",apikey)
  #Access API
  api_call = GET(url = devices_full_url)
  #Could check status_code or content
  #Content is in unicode - need to convert it to text and parse the JSON
  devices <- fromJSON(rawToChar(api_call$content))
  
  devices_df <- do.call(what = "rbind",
                        args = lapply(devices, as.data.frame))
  riviera_id <- devices_df['id'][1,]
  clearlake_oaks_id <- devices_df['id'][2,]
  riviera_device_url = paste0(base_url, "/", riviera_id,"/","parameters?", apikey)
  riviera_device = GET(url = riviera_device_url)
  riviera_parameters <- fromJSON(rawToChar(riviera_device$content))

 
 
  #Access device data parameters
  #Query function
  get_data <- function (query_start_date, query_end_date, water_variable){
    if (water_variable == "temperature"){
      parameter_id <- 53286
    }else if(water_variable == "chlorophyll"){
      parameter_id <- 53290
    }
    riviera_surface_data_url <- paste0(base_url, "/", riviera_id,"/","parameters/",parameter_id,"/data?", apikey, "&from=",
                                      query_start_date, '%2010:00:00&to=',query_end_date, "%2010:00:00")
    riviera_surface_data <- GET(url = riviera_surface_data_url)
    riviera_surface <- fromJSON(rawToChar(riviera_surface_data$content))
    raw_data <- riviera_surface$data
    # return(raw_data)
  }
  # clean_temp_data <- function(raw_data){
  #   raw_data %>%
  #     rename("water_temperature_c" = value) %>%
  #     mutate_at("water_temperature_c", as.numeric) %>%
  #     mutate("timestamp" = ymd_hms(raw_data$timestamp,tz=Sys.timezone()))
  # }
  
  # clean_chloro_data <- function(raw_data){
  #   raw_data %>% 
  #     rename("chloro_ug_L" = value) %>%
  #     mutate_at("chloro_ug_L", as.numeric) %>%
  #     mutate("timestamp" = ymd_hms(raw_data$timestamp,tz=Sys.timezone()))
  # }
  temp_dataInput <- reactive({
    
    get_data(input$dateRange[1], input$dateRange[2], input$water_variable) %>% 
      rename("water_temperature_c" = value) %>%
      mutate_at("water_temperature_c", as.numeric) %>%
      mutate("timestamp" = ymd_hms(timestamp,tz=Sys.timezone()))
  })
  # chloro_dataInput <- reactive({
  #   clean_chloro_data(get_data(input$dateRange[1], input$dateRange[2], input$water_variable))
  # })
  # dataInput <- reactive({
  #   raw_riviera_temp_data <- riviera_surface_temp_url = paste0(base_url, "/", riviera_id,"/","parameters/53286/data?", apikey, "&from=",
  #                                                              input$dateRange[1], '%2010:00:00&to=',input$dateRange[2], "%2010:00:00")
  #   riviera_surface_temp = GET(url = riviera_surface_temp_url)
  #   riviera_surface <- fromJSON(rawToChar(riviera_surface_temp$content))
  #   raw_riviera_temp_data <- riviera_surface$data
  # #Data Transformation
  # clean_riviera_temp_data <- raw_riviera_temp_data %>%
  #   rename("water_temperature_c" = value) %>%
  #   mutate_at("water_temperature_c", as.numeric) %>%
  #   mutate("timestamp" = ymd_hms(raw_riviera_temp_data$timestamp,tz=Sys.timezone()))
  # })
  #Visualization
  output$temp_plot <- renderPlotly({
    temp_dataInput()%>% 
      plot_ly(
        x = ~timestamp, 
        y = ~water_temperature_c, 
        type = 'scatter', 
        mode = 'lines',
        hoverinfo = 'text',
        text = ~paste('<br> Time Stamp: ', timestamp,
                      '<br> Temperature: ', water_temperature_c)) %>% 
    layout(title = "Water Surface Temperature from Riviera West Sensor at Clearlake",
           xaxis = list(title = 'Date'),
           yaxis = list(title = 'Water Temperature (Celsius)'))
  })
  # output$chlorophyll_plot <- renderPlotly({
  #   chloro_dataInput()%>% 
  #     plot_ly(
  #       x = ~timestamp, 
  #       y = ~chloro_ug_L, 
  #       type = 'scatter', 
  #       mode = 'lines',
  #       hoverinfo = 'text',
  #       text = ~paste('<br> Time Stamp: ', timestamp,
  #                     '<br> Chlorophyll Concentration: ', chloro_ug_L)) %>% 
  #     layout(title = "Surface Chlorophyll Concentration from Riviera West Sensor at Clearlake",
  #            xaxis = list(title = 'Date'),
  #            yaxis = list(title = 'Chlorophyll Concentration (ug/L)'))
  # })
  output$dateRangeText  <- renderText({
    paste("input$dateRange is",
          paste(as.character(input$dateRange), collapse = " to ")
    )
  })
  # output$variableText <- renderText({
  #   variable <- paste(input$water_variable, collapse = ", ")
  #   paste("You chose", variable)
  # })
}
