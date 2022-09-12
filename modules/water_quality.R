historical_data_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style("body{
      height: 584px;
      width: 1005px;
      margin: auto;
          }")
    ),
    tags$h3("Historic Water Quality"),
    tags$div(class = "well",
               fluidRow(
                 column(width  = 12, 
                        "Select a monitoring station using the map and a water quality feature
              from the drop down to update the chart."),
                 tags$br(),
                 column(width = 6,
                        tags$h5("Station Locations"),
                        leafletOutput(ns(
                          "station_selection_map"
                        ), height = 200)),
                 column(width = 5,
                        uiOutput(ns("clear_lake_wq_select_ui"))),
               )),
    
    column(width = 12,
           align = "left",
           fluidRow(
             plotlyOutput(ns("historic_gage_plot"),
                          height = 500),
             tags$br(),
             tags$p(
               "Source: The Big Valley Band of Pomo Indians obtained Clear Lake water quality data from stakeholders and public agences.
                    The initial data incoporated into the master dataset included California Department of Food and Agriculture (CDFA)
                    and California Department of Water Resources (CDWR)."
             )
           ))
  )
}

water_quality_server <- function(input, output, session) {
  
  ns <- session$ns
  
 default_station <- 
    clear_lake_wq %>%
    filter(station_name == "Soda Bay 100 Meters")

  output$station_selection_map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap) %>% 
      addCircleMarkers(
      data = clear_lake_wq, 
      layerId = clear_lake_wq$station_code,
      radius = 6,
      fillOpacity = .4,
      weight = 2,
      color = "#2e2e2e", 
      fillColor = "#555555",
      opacity = .4,
        label = paste(str_to_title(clear_lake_wq$station_name), "Sensor")
      )%>%
      setView(lng = -122.708927,
              lat = 39.012078,
              zoom = 10)%>%
      addCircleMarkers(
        data = default_station,
        ~default_station$longitude,
        ~default_station$latitude,
        fillOpacity = .8,
        weight = 2,
        color = "#2e2e2e",
        fillColor = "#28b62c",
        opacity = 1, 
        group = "default_station"
	)
  })
  
 selected_station <- reactive({
    if (length(input$station_selection_map_marker_click['lat']) > 0) {
      clear_lake_wq %>%
        filter(latitude == input$station_selection_map_marker_click['lat'])
    } 
    else
      (
        clear_lake_wq %>%
          filter(station_name == "Soda Bay 100 Meters")
      )
  }) %>% bindCache(input$station_selection_map_marker_click['lat']) 
  observeEvent(input$station_selection_map_marker_click, {
    leafletProxy("station_selection_map") %>% 
     clearGroup("default_station") %>%
     clearGroup('selected_station') %>% 
     addCircleMarkers(
      data=selected_station(), 
      fillOpacity = .8,
      weight = 2,
      color = "#2e2e2e", 
      fillColor = "#28b62c",
      opacity = 1, 
      
      group = "selected_station"
      )
  })
  
  selected_station <- reactive({
    if (length(input$station_selection_map_marker_click['lat']) > 0) {
      clear_lake_wq %>%
        filter(station_code == input$station_selection_map_marker_click['id'])
    }else(
      clear_lake_wq %>% 
        filter(station_name == "Soda Bay 100 Meters")) 
  })
  
 output$clear_lake_wq_select_ui<- renderUI({
   
   historic_water_variable_choices <-
     unique(selected_station()$analyte_name)
   #function returns UI
   selectInput(
     ns("historic_water_variable"),
     label = h5("Water Quality Features"),
     selected = NULL,
     choices = historic_water_variable_choices,
     width = '250px',
     multiple = FALSE
   )
  })
 
 selected_wq_data_in_station <- reactive(
   selected_station() %>% 
     filter(analyte_name == input$historic_water_variable)
 )

#  
  output$historic_gage_plot <- renderPlotly({
    # req(input$station_selection_map_marker_click)
    
    shiny::validate(
      need(nrow(selected_wq_data_in_station()) > 0, "Selection yielded no results"))
    
    
    unit <- selected_wq_data_in_station() %>%
      pull(unit)
    
    analyte_plot <- selected_wq_data_in_station() %>%
      plot_ly(
        x =  ~ sample_datetime,
        y =  ~ numeric_result,
        type = 'bar',
        hoverinfo = 'text',
        text = ~ paste(
          "<br>Date:",
          as.Date(sample_datetime),
          paste0("<br>", input$historic_water_variable, ": ", numeric_result, na.omit(unit))
          )
      ) %>%
      layout(xaxis = list(title = ~ sample_datetime),
             yaxis = list(title = selected_wq_data_in_station()$unit[1], showgrid = TRUE),
             height = 500,
             width = 1000,
             hovermode = "x",
             showlegend = FALSE
             ) %>%
      add_annotations(
        text = paste(input$historic_water_variable, "at", selected_wq_data_in_station()$station_name[1]),
        x = 0,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "left",
        yanchor = "top",
        yshift = 30,
        showarrow = FALSE,
        font = list(size = 18)
      ) %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::config(showLink = FALSE)
  })
}



