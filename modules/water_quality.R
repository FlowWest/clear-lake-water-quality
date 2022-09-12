water_quality_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    column(width = 3, 
           tags$h3("Water Quality"),
           tags$p("Use this dashboard to visualize different analytes 
                  of interest located in or around the Big Valley Rancheria."),
           selectInput(ns("analyte"), "Select analyte", 
                       choices = bvr_analytes), 
           uiOutput(ns("station_select_ui")), 
           tags$hr(), 
           tabsetPanel(
             type = "pills",
             tabPanel(
               "Analyte Details",
               uiOutput(ns("about_selected_analyte"))
             ),
             tabPanel(
               "Station Map", 
               leafletOutput(ns("station_map"))
             )
           )), 
    column(width = 9, 
           plotlyOutput(ns("analyte_plot")), 
           plotlyOutput(ns("analyte_boxplot")))
  )
}

water_quality_server <- function(input, output, session) {
  
  ns <- session$ns
  
  selected_wq_data <- reactive({
    bvr_water_quality %>% 
      filter(analyte == input$analyte, 
             !is.na(value_numeric)) 
  })
  
  abundance_by_station <- reactive({
    selected_wq_data() %>% 
      group_by(station_id) %>% 
      summarise(
        total = n()
      ) %>% ungroup()
  })
  
  output$station_select_ui <- renderUI({
    station_choices <- 
      abundance_by_station() %>% 
      arrange(desc(total))
    
    pickerInput(
      inputId = ns("station"),
      label = "Select station(s)", 
      choices = station_choices$station_id,
      choicesOpt = list(
        subtext = paste(" observations", 
                        station_choices$total,
                        sep = ": "))
    )
  })
  
  selected_wq_data_in_station <- reactive({
    selected_wq_data() %>% 
      filter(station_id == input$station)
  })
  
  output$analyte_plot <- renderPlotly({
    
    req(input$station)
    selected_wq_data_in_station() %>% 
      plot_ly(x=~datetime, 
              y=~value_numeric, 
              type = 'scatter', mode='markers') %>% 
      layout(xaxis = list(title = ""), yaxis = list(title = input$analyte))
  })
  
  output$analyte_boxplot <- renderPlotly({
    selected_wq_data_in_station() %>% 
      mutate(month = factor(month.abb[month(datetime)], 
                            levels = month.abb)) %>% 
      plot_ly(x=~month, y=~value_numeric) %>% 
      add_boxplot(boxpoints = "outliers")
  })
  
  
  output$about_selected_analyte <- renderUI({
    description <- analyte_descriptions %>% 
      filter(analyte == input$analyte) %>% 
      pull(description)
    
    description_img <- analyte_descriptions %>% 
      filter(analyte == input$analyte) %>% 
      pull(img_url)
    
    description_cite <- analyte_descriptions %>% 
      filter(analyte == input$analyte) %>% 
      pull(source_citation)
    
    tagList(
      tags$h4(input$analyte), 
      tags$p(paste(description)), 
      actionLink(ns("analyte_image_link"), 
                 href = "#", label = tags$img(src = paste(description_img), width = "400px")),
      helpText(paste("source:", description_cite))
    )
    
  })
  
  observeEvent(input$analyte_image_link, {
    
    description_img <- analyte_descriptions %>% 
      filter(analyte == input$analyte) %>% 
      pull(img_url)
    
    showModal(
      modalDialog(title = input$analyte, 
                  tagList(
                    tags$img(src = paste(description_img))
                  ), size = "l")
    )
  })
  
  selected_station_map_marker <- reactive({
    bvr_stations %>% 
      filter(station_id == input$station)
  })
  
  output$station_map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>% 
      addCircleMarkers(data=bvr_stations, 
                       fillOpacity = .8,
                       weight = 2,
                       color = "#2e2e2e", 
                       fillColor = "#555555",
                       opacity = 1, 
                       label = ~station_id)
  })
  
  observeEvent(input$station, {
    leafletProxy("station_map") %>% 
      clearGroup("selected_station") %>% 
      addCircleMarkers(data=selected_station_map_marker(), 
                       fillOpacity = .8,
                       weight = 2,
                       color = "#2e2e2e", 
                       fillColor = "#28b62c",
                       opacity = 1, 
                       label = ~station_id, 
                       group = "selected_station")
  })
  
  
}
























