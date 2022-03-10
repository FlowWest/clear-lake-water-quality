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
                        "Select a  water quality feature
              from the drop down, and a monitoring station to update the chart"),
                 tags$br(),
                 column(width = 7,
                        leafletOutput(ns(
                          "station_selection_map"
                        ), height = 150)),
                 column(width = 5,
                        uiOutput(ns("clear_lake_wq_select_ui"))),
               )),
    
    column(width = 12,
           align = "center",
           plotlyOutput(ns("historic_gage_plot"))
           # plotlyOutput(ns("gage_plot2"))
           )
  )
}
# analyte <- c('Dissolved Oxygen', 'Fluridone', 'pH', 'Specific Conductance',
             # 'Specific Conductivity', 'Temperature', 'Turbidity')
# condition <- c(5.0, )

water_quality_server <- function(input, output, session) {
  ns <- session$ns
  
  # selected_wq_data <- reactive({
  #   req(input$analyte)
  #   clear_lake_wq %>%
  #     filter(analyte_name == input$analyte,!is.na(numeric_result))
  # }) %>% bindCache(input$analyte)
  
 
  default_station <- 
    clear_lake_wq %>%
      filter(station_name == "Soda Bay 100 Meters")

  output$station_selection_map <-renderLeaflet({
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
        # popup = paste(str_to_title(selected_wq_data()$station_name), "Sensor"),
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
  
  # output$station_selection_map <-renderLeaflet({
  #   leaflet(data = selected_wq_data()) %>%
  #     addProviderTiles(providers$OpenStreetMap) %>%
  #     addCircleMarkers(
  #       ~selected_wq_data()$longitude,
  #       ~selected_wq_data()$latitude,
  #       layerId = selected_wq_data()$station_code,
  #       radius = 6,
  #       fillOpacity = .4,
  #       weight = 2,
  #       color = "#2e2e2e",
  #       fillColor = "#555555",
  #       opacity = .4,
  #       # popup = paste(str_to_title(selected_wq_data()$station_name), "Sensor"),
  #       label = paste(str_to_title(selected_wq_data()$station_name), "Sensor")
  #     )%>%
  #   setView(lng = -122.708927,
  #           lat = 39.012078,
  #           zoom = 10)%>%
  #   addCircleMarkers(
  #   data = default_station,
  #   ~default_station$longitude,
  #   ~default_station$latitude,
  #   fillOpacity = .8,
  #   weight = 2,
  #   color = "#2e2e2e",
  #   fillColor = "#28b62c",
  #   opacity = 1,
  #   group = "default_station"
  #   )
  # })
  
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
        data = selected_station(),
        fillOpacity = .8,
        weight = 2,
        color = "#2e2e2e",
        fillColor = "#28b62c",
        opacity = 1,
        group = "selected_station"
      )
  # print(input$station_selection_map_marker_click)
  })
  
  selected_station <- reactive({
    # req(input$analyte, input$station_selection_map_marker_click)
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
    # print(nrow(selected_wq_data_in_station()))
    
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
          paste0("<br>", input$historic_water_variable, ":", numeric_result, na.omit(unit))
          )
        # tooltip = "text"
      ) %>%
      layout(xaxis = list(title = ~ sample_datetime),
             yaxis = list(title = selected_wq_data_in_station()$unit[1], showgrid = TRUE),
             height = 500,
             width = 800,
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
    # gage_plot <-
    #   gage_analyte_data() %>%
    #   plot_ly(
    #     x = ~ dateTime,
    #     y = ~ gage_height,
    #     type = 'scatter',
    #     mode = 'lines',
    #     hoverinfo = 'text',
    #     color = "orange",
    #     text = ~ paste0(
    #       "<br>Date: ",
    #       as.Date(dateTime),
    #       paste0("<br>Water Level:",gage_height, "ft.")
    #     )
    #   ) %>%
    #   layout(
    #     xaxis = list(title = "Date"),
    #     yaxis = list(title = "Feet", showgrid = TRUE),
    #     hovermode = "x",
    #     height = 800,
    #     width = 800,
    #     showlegend = FALSE
    #   )%>%
    #   add_annotations(
    #     text = "Lake Elevation",
    #     x = 0,
    #     y = 1,
    #     yref = "paper",
    #     xref = "paper",
    #     xanchor = "left",
    #     yanchor = "top",
    #     yshift = 20,
    #     showarrow = FALSE,
    #     font = list(size = 18)
    #   ) %>%
    #   plotly::config(displayModeBar = FALSE) %>%
    #   plotly::config(showLink = FALSE)


    # subplot(list(analyte_plot, gage_plot),nrows = 2, shareX = TRUE, margin = 0.06, titleY = TRUE, titleX = TRUE)
#   })
# }
#   
#   
# 
# output$about_selected_analyte <- renderUI({
#   description <- analyte_descriptions %>%
#     filter(analyte == input$analyte) %>%
#     pull(description)
#   
#   description_img <- analyte_descriptions %>%
#     filter(analyte == input$analyte) %>%
#     pull(img_url)
# 
#   description_cite <- analyte_descriptions %>%
#     filter(analyte == input$analyte) %>%
#     pull(source_citation)
#   
#   tagList(tags$h4(input$analyte),
#           tags$p(paste(description)))
#           actionLink(ns("analyte_image_link"),
#                      href = "#", label = tags$img(src = paste(description_img), width = "400px"))
#           helpText(paste("source:", description_cite))
#           
# })
#   
  # observeEvent(input$analyte_image_link, {
  #   description_img <- analyte_descriptions %>%
  #     filter(analyte == input$analyte) %>%
  #     pull(img_url)
  # 
  #   showModal(modalDialog(
  #     title = input$analyte,
  #     tagList(tags$img(src = paste(description_img))),
  #     size = "l"
  #   ))
  # })
# #   
#   selected_station_map_marker <- reactive({
#     clear_lake_wq %>%
#       filter(station_code == input$station)
#   })
# # 
#   output$station_map <- renderLeaflet({
#     leaflet() %>%
#       addProviderTiles(providers$Esri.WorldTopoMap) %>%
#       addCircleMarkers(
#         data = clear_lake_wq,
#         radius = 4,
#         fillOpacity = .4,
#         weight = 2,
#         color = "#2e2e2e",
#         fillColor = "#555555",
#         opacity = .4,
#         label = ~ station_code
#       )
#   })
# # #   # 
#   observeEvent(input$station, {
#     leafletProxy("station_map") %>%
#       clearGroup("selected_station") %>%
#       addCircleMarkers(
#         data = selected_station_map_marker(),
#         fillOpacity = .8,
#         weight = 2,
#         color = "#2e2e2e",
#         fillColor = "#28b62c",
#         opacity = 1,
#         label = ~ station_code,
#         group = "selected_station"
#       )
  # })

  
  
