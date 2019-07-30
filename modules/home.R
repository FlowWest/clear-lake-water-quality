home_ui <- function(id) {
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
           uiOutput(ns("about_selected_analyte"))), 
    column(width = 9, 
           plotlyOutput(ns("analyte_plot")), 
           plotlyOutput(ns("analyte_boxplot")))
  )
}

home_server <- function(input, output, session) {
  
  ns <- session$ns
  
  selected_wq_data <- reactive({
    bvr_wq %>% 
      filter(characteristic_name == input$analyte, 
             !is.na(result_value_numeric)) 
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
      plot_ly(x=~activity_start_date, 
              y=~result_value_numeric, 
              type = 'scatter', mode='markers') %>% 
      layout(xaxis = list(title = ""), yaxis = list(title = input$analyte))
  })
  
  output$analyte_boxplot <- renderPlotly({
    selected_wq_data_in_station() %>% 
      mutate(month = factor(month.abb[month(activity_start_date)], 
                            levels = month.abb)) %>% 
      plot_ly(x=~month, y=~result_value_numeric) %>% 
      add_boxplot(boxpoints = "outliers")
  })
  
  
  output$about_selected_analyte <- renderUI({
    tagList(
      tags$h4(input$analyte), 
      tags$p("This is just what this section can look like, this text is not real."),
      tags$p('A fecal coliform (British: faecal coliform) is a facultatively anaerobic, rod-shaped, gram-negative, non-sporulating bacterium. Coliform bacteria generally originate in the intestines of warm-blooded animals. Fecal coliforms are capable of growth in the presence of bile salts or similar surface agents, are oxidase negative, and produce acid and gas from lactose within 48 hours at 44 ± 0.5°C.[1] The term "thermotolerant coliform" is more correct and is gaining acceptance over "fecal coliform".[2]'
      )
    )
  })
  
  
}
























