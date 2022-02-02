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
    column(
      width = 3,
      tags$h3("Historic Water Quality"),
      tags$p(
        "Use this dashboard to visualize different analytes
                  of interest located in or around the Big Valley Rancheria."
      ),
      selectInput(ns("analyte"), "Select analyte",
                  choices = clear_lake_wq_choices),
      uiOutput(ns("station_select_ui")),
      #server insert ui for us
      tags$hr(),
      tabsetPanel(
        type = "pills",
        tabPanel("Sampling Location Map",
                 leafletOutput(ns("station_map")),),
        tabPanel("Analyte Details",
                 uiOutput(ns(
                   "about_selected_analyte"
                 )))
      )
    ),
    column(width = 9,
           plotlyOutput(ns("analyte_plot"))
           # plotlyOutput(ns("gage_plot2"))
           )
  )
}
analyte <- c('Dissolved Oxygen', 'Fluridone', 'pH', 'Specific Conductance',
             'Specific Conductivity', 'Temperature', 'Turbidity')
# condition <- c(5.0, )

water_quality_server <- function(input, output, session) {
  ns <- session$ns
  
  selected_wq_data <- reactive({
    clear_lake_wq %>%
      filter(analyte_name == input$analyte,!is.na(numeric_result))
  })
  
  #group station by code, calculate total obs for selected analyte
  abundance_by_station <- reactive({
    selected_wq_data() %>%
      group_by(station_code) %>%
      summarise(total = n()) %>% ungroup()
  })

  
  output$station_select_ui <- renderUI({
    # wait for this selection explicitly
    req(input$analyte)
    
    station_choices <-
      abundance_by_station() %>%
      arrange(desc(total))
    #function returns UI
    pickerInput(
      inputId = ns("station"),
      label = "Select Sampling Location",
      choices = station_choices$station_code,
      multiple = FALSE,
      choicesOpt = list(subtext = paste(
        " observations",
        station_choices$total,
        sep = ": "
      ))
    )
  })
  
  selected_wq_data_in_station <- reactive({
    selected_wq_data() %>%
      filter(station_code == input$station)
  })
  
  gage_analyte_data <- reactive({
    req(input$station)
    
    readNWISdata(
    sites = "11450000",
    parameterCd = "00065",
    service = "dv",
    startDate = min(as.Date(selected_wq_data_in_station()$sample_datetime)),
    endDate = max(as.Date(selected_wq_data_in_station()$sample_datetime)),
    tz = "America/Los_Angeles"
  ) %>%
    mutate(dateTime = as.Date(dateTime)) %>%
    rename("gage_height" = "X_00065_00003") %>% 
      glimpse()
  })
 
   # output$gage_plot2 <- renderPlotly({
     # gage_plot <- 
     #   gage_analyte_data() %>%
     #   plot_ly(
     #    x = ~ dateTime,
     #    y = ~ gage_height,
     #    type = 'scatter',
     #    mode = 'lines',
     #    hoverinfo = 'text',
     #    color = "orange",
     #    text = ~ paste(
     #      "<br>Date: ",
     #      as.Date(dateTime),
     #      paste("<br>Water Level:",gage_height, "ft.")
     #    )
     #  ) %>% 
     #   layout(
     #     xaxis = list(title = "Date"),  
     #     yaxis = list(title = "Gage Height"),
     #     hovermode = "closest"
     #   )%>%      
     #   plotly::config(displayModeBar = FALSE) %>%
     #   plotly::config(showLink = FALSE)
  # })
  output$analyte_plot <- renderPlotly({
     req(input$station)
    analyte <- selected_wq_data_in_station() %>%
      plot_ly(
        x =  ~ sample_datetime,
        y =  ~ numeric_result,
        type = 'bar'
      ) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = input$analyte),
             height = 800,
             width = 800) %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::config(showLink = FALSE)
    
    gage_plot <- 
      gage_analyte_data() %>%
      plot_ly(
        x = ~ dateTime,
        y = ~ gage_height,
        type = 'scatter',
        mode = 'lines',
        hoverinfo = 'text',
        color = "orange",
        text = ~ paste(
          "<br>Date: ",
          as.Date(dateTime),
          paste("<br>Water Level:",gage_height, "ft.")
        )
      ) %>% 
      layout(
        xaxis = list(title = "Date"),  
        yaxis = list(title = "Gage Height"),
        hovermode = "closest",
        height = 800,
        width = 800
      )%>%      
      plotly::config(displayModeBar = FALSE) %>%
      plotly::config(showLink = FALSE)
    
    subplot(list(analyte, gage_plot),nrows = 2, shareX = TRUE, margin = 0.06)
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
  
  tagList(tags$h4(input$analyte),
          tags$p(paste(description)))
          actionLink(ns("analyte_image_link"),
                     href = "#", label = tags$img(src = paste(description_img), width = "400px"))
          helpText(paste("source:", description_cite))
          
})
  
  observeEvent(input$analyte_image_link, {
    description_img <- analyte_descriptions %>%
      filter(analyte == input$analyte) %>%
      pull(img_url)
    
    showModal(modalDialog(
      title = input$analyte,
      tagList(tags$img(src = paste(description_img))),
      size = "l"
    ))
  })
  
  selected_station_map_marker <- reactive({
    clear_lake_stations %>%
      filter(station_code == input$station)
  })
  
  output$station_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addCircleMarkers(
        data = clear_lake_stations,
        radius = 4,
        fillOpacity = .4,
        weight = 2,
        color = "#2e2e2e",
        fillColor = "#555555",
        opacity = .4,
        label = ~ station_code
      )
  })
  
  observeEvent(input$station, {
    leafletProxy("station_map") %>%
      clearGroup("selected_station") %>%
      addCircleMarkers(
        data = selected_station_map_marker(),
        fillOpacity = .8,
        weight = 2,
        color = "#2e2e2e",
        fillColor = "#28b62c",
        opacity = 1,
        label = ~ station_code,
        group = "selected_station"
      )
  })
}
  
  
