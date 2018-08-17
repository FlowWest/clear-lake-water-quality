home_UI <- function(id) {
  ns <- NS(id) 
  
  tagList(
    fluidRow(
      column(width = 12,
             tags$header(class = 'title',
                         tags$img(src = 'big_valley_rancheria.png'),
                         tags$h1('Clear Lake Water Quality', style="display:inline-block"))
      )
    ),
    fluidRow(
      column(width = 12, class = 'col-md-6',
             tags$section(class = 'app-controls',
                          tags$div(style="display:inline-block",
                                   selectInput(ns("analyte_selected"), label="Select Analyte", 
                                               choices = analyte_choices, selectize = TRUE)),
                          tags$div(style="display:inline-block",
                                   uiOutput(ns('select_sampling_ui'))),
                          tags$div(style="display:inline-block",
                                   uiOutput(ns('select_property_ui'))),
                          uiOutput(ns('select_depth'))
             ),
             plotlyOutput(ns('analyte_plot')),
             uiOutput(ns('analyte_description'))),
      column(width = 12, class = 'col-md-6',
             leafletOutput(ns('sites_map')),
             tags$h2('Clear Lake Elevation'), 
             dygraphOutput(ns('lake_elev_plot'), height = '200px'))
    ),
    fluidRow(
      column(width = 12,
             tags$img(src = 'TransLogoTreb.png', width = '200px'),
             tags$p('App developed and maintained by', 
                    tags$a('Emanuel Rodriguez', href = 'mailto:erodriguez@flowwest.com', target = '_blank')))
    )
  )
  
}

home_server <- function(input, output, session) {
  ns <- session$ns
  
  output$select_property_ui <- renderUI({
    d <- wq_data %>% 
      filter(analyte_name == input$analyte_selected) %>% 
      distinct(analyte_quality) %>% 
      pull()
    
    if (length(d) == 1 && is.na(d)) return(NULL)
    
    selectInput(ns("analyte_property_selected"), label = "Sample Fraction", 
                choices = d[!is.na(d)])
  })
  
  output$select_sampling_ui <- renderUI({
    d <- wq_data %>% filter(analyte_name == input$analyte_selected) %>% 
      distinct(sample_method) %>% pull()
    selectInput(ns("sampling_method_selected"), label = "Sample Method", 
                choices = d[!is.na(d)])
  })
  
  
  output$select_depth <- renderUI({
    selectInput('selected_depth', label = 'Select Depth', choices = 1:4)
  })
  
  
  selected_analyte_data <- reactive({
    d1 <- wq_data %>% 
      filter(analyte_name == input$analyte_selected)
    
    if (all(is.na(d1$analyte_quality))) return(d1 %>% filter(sample_method == input$sampling_method_selected)) 
    
    d1 %>% 
      filter(analyte_quality == input$analyte_property_selected, 
             sample_method == input$sampling_method_selected)
    
  })
  
  output$analyte_plot <- renderPlotly({
    req(input$analyte_property_selected)
    validate(need(
      nrow(selected_analyte_data()) > 0, 
      paste0("No data found for ", input$analyte_selected)
    ))
    
    validate(need(
      !all(is.na(selected_analyte_data()$numeric_result)), 
      paste0("No data found for ", input$analyte_selected)
    ))

    selected_analyte_data() %>% 
      plot_ly(x=~sample_datetime, y=~numeric_result, type='scatter', mode='markers', 
              hoverinfo="text", 
              text = ~paste0(format(sample_datetime, "%b %d, %Y %H:%M:%S"), "<br>",
                            "<b>", numeric_result, "</b> ", unit)) %>% 
      layout(xaxis = list(title = "Sample Time"), 
             yaxis = list(title = "Result"))
  })
  
  output$analyte_description <- renderUI({
    tags$p('A description of the analyte')
  })
  
  output$sites_map <- renderLeaflet(
    leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(data=wq_stations, label=~station_name, 
                       popup=~paste0("<b>", station_name, "</b><br>", 
                                     "<em>Station Code:&emsp;", station_code, "</em><br>", 
                                     "<em>Parent Project:&emsp;", parent_project, "</em><br>",
                                     "<button>Plot Series</button>")) %>% 
      setView(lng=-122.767084, lat=39.028363, zoom = 11)
  )
  
  output$lake_elev_plot <- renderDygraph(
      dygraph(xts(x=clear_lake_wse$wse_ft, order.by = clear_lake_wse$date)) %>%
        dySeries("V1", label = "WSE (ft)") %>% 
        dyRangeSelector(dateWindow = c(as.Date("1990-01-01"), Sys.Date())) 
  )
}