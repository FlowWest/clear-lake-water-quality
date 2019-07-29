home_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    column(width = 3, 
           selectInput(ns("analyte"), "Select analyte", 
                       choices = bvr_analytes), 
           uiOutput(ns("station_select_ui"))), 
    column(width = 9, 
           plotlyOutput(ns("analyte_plot")))
  )
}

home_server <- function(input, output, session) {
  
  ns <- session$ns
  
  selected_wq_data <- reactive({
    bvr_wq %>% 
      filter(characteristic_name == input$analyte)
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
  
  output$analyte_plot <- renderPlotly({
    
    req(input$station)
    
    selected_wq_data() %>% 
      filter(station_id == input$station) %>% 
      plot_ly(x=~activity_start_date, 
              y=~result_value_numeric, 
              type = 'scatter', mode='markers')
  })
}