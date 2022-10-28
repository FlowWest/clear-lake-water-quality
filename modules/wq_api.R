
wqdata_ui <- function(id) {
  ns <- NS(id)
  
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    ),
    tags$h3("Realtime Monitoring"),
    tags$div(class = "well",
             fluidRow(
               column(width  = 12, 
               "Select a monitoring location using the map, water quality feature
              from the drop down, and date range below to update the chart"),
               tags$br(),
               column(width = 4,
                      tags$h5('Sensor Locations'),
                      leafletOutput(ns(
                        "sensor_selection_map"
                      ), height = 200)),
               column(width = 4,
                      uiOutput(ns(
                        "water_variable_select_ui"
                      ))),
               column(
                 class = "date_range",
                 width = 4,
                 dateRangeInput(
                   ns('dateRange'),
                   label = h5('Date Range Input: YYYY-MM-DD'),
                   start = Sys.Date() - 7,
                   end = Sys.Date(),
                   min = Sys.Date() - 90,
                   max = Sys.Date()
                 )
               )
             )),
    
    fluidRow(column(
      width = 12,
      column(
        width = 8,
        dataTableOutput(ns('data_input')),
        plotlyOutput(ns("wq_plot")),
        tags$div(
          class = "wq_graph_label",
          "Data Source: hourly water quality data from",
          tags$a("WQData Live API",
                 href = "https://wqdatalive.com/",
                 target = "_blank")
        )
      ),
      column(
        width = 4,
        fluidRow(valueBoxOutput(ns("gage_height"), width = 12)),
        fluidRow(
          plotlyOutput(ns("gage_plot"),
                       height = "200px"),
          tags$div(
            class = "usgs_graph_label",
            "Data Source: lake elevation data from",
            tags$a(
              "USGS Clear Lake Gage at Lakeport.",
              href = "https://waterdata.usgs.gov/monitoring-location/11450000/",
              target = "_blank"
            )
            
          )
        )
      )
    )),
    
    br(),
    tags$p(
      class = "description",
      "Clear Lake is a natural freshwater lake in Lake County in
    the U.S. state of California, north of Napa County and
    San Francisco. It is the largest natural freshwater lake
    wholly within the state, with 68 square miles (180 square kilometers) of
    surface area. At 480,000 years, it is the oldest lake in
    North America. It is the latest lake to occupy a site
    with a history of lakes stretching back at
    least 2,500,000 years. The data for Clear Lake is collected from two sensors,
    one on the west side of Clear Lake (Riviera West), and another on the east side of
    Clear Lake (Clearlake Oaks)."
    ),
    tags$p(
      class = "description",
      "Visualize the hourly data of interest from WQData Live
    for the past 90 days. Use the drop down menu to select the water quality data of interest,
    hover the mouse over the graph to find the value of a specific hour, and
    drag the mouse over the chart to zoom in on the graph.
    The lake elevation graph displays the daily value from the USGS gage when the
    date range interval is bigger than two days. The graph displays quarter-hour
    value when the date range interval is smaller than two days."
      
    )
  )
  
}

wq_data_server <- function(input, output, session) {
  #cache the parameters and save it as a vector
  ns <- session$ns

  default_sensor <- monitoring_stations %>%
    filter(station_name == "riviera")
  api_sensors <- monitoring_stations %>%
    filter(station_name != "usgs_gage")
  
  output$sensor_selection_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addCircleMarkers(
        data = api_sensors,
        layerId = api_sensors$station_name,
        radius = 6,
        fillOpacity = .4,
        weight = 2,
        color = "#2e2e2e",
        fillColor = "#555555",
        opacity = .4,
        popup = paste(str_to_title(api_sensors$station_name), "Sensor"),
        label = paste(str_to_title(api_sensors$station_name), "Sensor")
      ) %>%
      setView(lng = -122.708927,
              lat = 39.012078,
              zoom = 11) %>%
      addCircleMarkers(
        data = default_sensor,
        fillOpacity = .8,
        weight = 2,
        color = "#2e2e2e",
        fillColor = "#28b62c",
        opacity = 1,
        group = "default_sensor",
        popup = "Riviera Sensor",
        label = "Riviera Sensor"
      )
  })
  selected_sensor <- reactive({
    if (length(input$sensor_selection_map_marker_click['lat']) > 0) {
      monitoring_stations %>%
        filter(latitude == input$sensor_selection_map_marker_click['lat'])
    } else
      (monitoring_stations %>%
         filter(station_name == "riviera"))
  }) %>% bindCache(input$sensor_selection_map_marker_click['lat'])
  
  
  observeEvent(input$sensor_selection_map_marker_click, {
    leafletProxy("sensor_selection_map") %>%
      clearGroup("default_sensor") %>%
      clearGroup('selected_sensor') %>%
      addCircleMarkers(
        data = selected_sensor(),
        fillOpacity = .8,
        weight = 2,
        color = "#2e2e2e",
        fillColor = "#28b62c",
        opacity = 1,
        group = "selected_sensor"
      )
  })
  
  selected_sensor_choices <- reactive({
    if (selected_sensor()['station_name'] == "riviera") {
      #   #access api based on station code to find the monitoring variables
      api_riviera_wq <-
        wq_parameters$name[1:8]
      #   #map new wq variable to the variables from api
      #   #apply function recode to the variables from api using the mapped variables
      #   #Display the recoded variables as the dropdown menu
      do.call(recode,
              c(
                list(api_riviera_wq),
                setNames(edited_riviera_wq, api_riviera_wq)
              ))
    } else if (selected_sensor()['station_name'] == "oaks") {
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
    water_variable_choices <-
      selected_sensor_choices()
    #function returns UI
    selectInput(
      ns("water_variable"),
      label = h5("Water Quality Features"),
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
      # print(monitoring_stations['station_name'])
      query_end_date <- query_end_date + 1
      
      if (monitoring_station == "riviera") {
        #create a key value pair where key is the wq variables and values are the api associated id
        riviera_vals <- wq_parameters$id[1:8]
        names(riviera_vals) <- edited_riviera_wq
        #call associated parameter id based wq variabe
        parameter_id <- riviera_vals[water_variable]
        data_url <-
          paste0(
            base_url,
            "/",
            riviera_id,
            "/parameters/",
            parameter_id,
            "/data?apiKey=",
            apikey,
            "&from=",
            query_start_date,
            '%2008:00:00&to=',
            query_end_date,
            "%2008:00:00"
          )
      } else if (monitoring_station == "oaks") {
        oaks_vals <- wq_parameters$id[9:20]
        names(oaks_vals) <- edited_oaks_wq
        parameter_id <- oaks_vals[water_variable]
        data_url <- paste0(
          base_url,
          "/",
          oaks_id,
          "/parameters/",
          parameter_id,
          "/data?apiKey=",
          apikey,
          "&from=",
          query_start_date,
          '%2008:00:00&to=',
          query_end_date,
          "%2008:00:00"
        )
      }
      variable_json_data <- GET(url = data_url)
      variable_info <-
        fromJSON(rawToChar(variable_json_data$content))
      raw_data <- variable_info$data
      if (input$water_variable == "Surface Temperature (F)" |
          input$water_variable == "Lake Bed Temperature (F)") {
        raw_data <- raw_data %>%
          mutate_at('value', as.numeric) %>%
          mutate(
            # "timestamp" = ymd_hms(timestamp) - hours(8),
            "date" = as.Date(timestamp),
            "value" = round(value * (9 / 5) + 32, 1)
          ) %>%
          return(raw_data)
      }
      else{
        raw_data <- raw_data %>%
          mutate_at('value', as.numeric) %>%
          return(raw_data)
      }
    }
  dataInput <- reactive({
    req(input$water_variable, input$dateRange[1], input$dateRange[2])

    get_data(input$dateRange[1],
             input$dateRange[2],
             selected_sensor()['station_name'],
             input$water_variable)%>%
  mutate("timestamp" = ymd_hms(timestamp) - hours(8),
  "date" = as.Date(timestamp)) %>%
  filter(!(value == -179968), !(value == -100000))
  })
    #Visualization
  output$wq_plot <- renderPlotly({
    
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
        "Water",
        input$water_variable,
        "from",
        str_to_title(selected_sensor()['station_name']),
        "Monitoring Sensor"
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
          paste(format(timestamp, format = "%H:%M"), "PST"),
          "<br>Date: ",
          date,
          hover_label,
          paste(value, unit)
        )
      ) %>%
      layout(
        title = (list(text= formatted_title, y = 0.97)),
        xaxis = list(title = 'Date'),
        yaxis = list(title = paste("Water", input$water_variable)),
        hovermode = "x"
      ) %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::config(showLink = FALSE)
  }) %>% bindCache(input$dateRange[1], input$dateRange[2], input$water_variable)
  
  data_gage_Input <- reactive({
    req(input$dateRange[1], input$dateRange[2])
    
    date_dif <- date(input$dateRange[2]) - date(input$dateRange[1])
    # print(date_dif)
    if (date_dif > 2) {
      readNWISdata(
        sites = "11450000",
        parameterCd = "00065",
        service = "dv",
        startDate = input$dateRange[1],
        endDate = input$dateRange[2],
        tz = "America/Los_Angeles"
      ) %>%
        mutate(dateTime = as.Date(dateTime)) %>%
        rename("gage_height" = "X_00065_00003")
    } else{
      readNWISdata(
        sites = "11450000",
        parameterCd = "00065",
        service = "iv",
        startDate = input$dateRange[1],
        endDate = input$dateRange[2],
        tz = "America/Los_Angeles"
      ) %>%
        mutate(dateTime = ymd_hms(dateTime)) %>%
        rename("gage_height" = "X_00065_00000")
    }
  })
  
  output$gage_height <- renderValueBox({
    most_recent_value <- data_gage_Input() %>%
      pull(gage_height) %>%
      last()
    valueBox(value = tags$p(
      paste("Current Water Level:", most_recent_value, " ft."),
      style = "font-size: 65%"
    ),
    subtitle = NULL)
  })
  
  output$gage_plot <- renderPlotly({
    data_gage_Input() %>%
      plot_ly(
        x = ~ dateTime,
        y = ~ gage_height,
        type = 'scatter',
        mode = 'lines',
        hoverinfo = 'text',
        color = "orange",
        text = ~ paste(
          "<br>Time: ",
          paste(format(dateTime, format = "%H:%M"), "PST"),
          "<br>Date: ",
          as.Date(dateTime),
          paste("<br>Water Level:", gage_height, "ft.")
        )
      ) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Gage Height"),
        hovermode = "x"
      ) %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::config(showLink = FALSE)
    
  })
}
