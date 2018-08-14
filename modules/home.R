home_UI <- function(id) {
  ns <- NS(id) 

  tagList(
    fluidRow(
      column(width = 12,
             tags$header(class = 'title',
               tags$img(src = 'big_valley_rancheria.png'),
               tags$h1('Clear Lake Water Quality'))
             )
    ),
    fluidRow(
      column(width = 12, class = 'col-md-6',
             tags$section(class = 'app-controls',
                          tags$div(style="display:inline-block",
                                   selectInput(ns("analyte_selected"), label="Select Analyte", 
                                               choices = analyte_choices, selectize = TRUE)),
                          tags$div(style="display:inline-block",
                                   uiOutput(ns('select_property_ui'))),
               uiOutput(ns('select_depth'))
             ),
             plotlyOutput(ns('analyte_plot')),
             uiOutput(ns('analyte_description'))),
      column(width = 12, class = 'col-md-6',
             leafletOutput(ns('sites_map')),
             tags$h2('Clear Lake Elevation'), 
             plotlyOutput(ns('lake_elev_plot'), height = '200px'))
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
  
  analyte_has_property <- reactive({
    wq_data %>% 
      filter(analyte_name == input$analyte_selected) %>% 
      pull(analyte_quality)
  })
  
  output$select_property_ui <- renderUI({
    req(input$analyte_selected)
    if (is.na(analyte_has_property())) return(NULL)
    
    selectInput(ns("analyte_property_selected"), label = "Select Property", 
                choices = analyte_has_property(), selected = analyte_has_property()[1])
  })
  
  output$select_depth <- renderUI({
    selectInput('selected_depth', label = 'Select Depth', choices = 1:4)
  })
  
  output$analyte_plot <- renderPlotly(
    mtcars %>% 
      plot_ly(x = ~mpg, y = ~cyl)
  )
  
  output$analyte_description <- renderUI({
    tags$p('A description of the analyte')
  })
  
  output$sites_map <- renderLeaflet(
    leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(data=ceden_stations, label=~station_name, 
                       popup=~paste0("<b>", station_name, "</b><br>", 
                                     "<em>Station Code:&emsp;", station_code, "</em><br>", 
                                     "<em>Parent Project:&emsp;", parent_project, "</em><br>",
                                     "<button>Plot Series</button>")) %>% 
      setView(lng=-122.767084, lat=39.028363, zoom = 11)
  )
  
  output$lake_elev_plot <- renderPlotly(
    mtcars %>% 
      plot_ly(x = ~disp, y = ~qsec, type = 'scatter', mode = 'lines')
  )
}