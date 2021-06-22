fishkill_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    column(width = 3, 
           tags$h3("Fish Kills"),
           tags$h4("Context"),
           tags$p("This page shows fish kill data near in and around the Big Valley Rancheria. 
                  This data comes from a citizen science inatrualist project ... whatever context we want to include."),
           tags$h4("How to Use"), 
           tags$p("Use this dashboard to visualize fish kill sites 
                  in or around the Big Valley Rancheria. Hover over a 
                  marker on the map to see information about the fish 
                  kill recorded at that point."),
           "Use", 
           tags$a(href = "https://www.inaturalist.org/projects/clear-lake-fish-kill-monitoring-project?tab=observations", "inaturalist"), 
           "to record and view Fish Kills", 
           tags$h4("Relevent Events Nearby"),
           tabsetPanel(
             type = "pills",
             tabPanel(
               "Flow at Rumsey Gage",
               plotlyOutput(ns("rumsey_plot"))),
             tabPanel(
               "Real Time Monitoring",
               plotlyOutput(ns("monitoring_plot")))
           )),
    column(width = 9,
           leafletOutput(ns("fish_kills_map"), width = "100%", height = 700),
           tags$h4("Data Summary"),
           formattableOutput(ns("summary_table")))
  )
}

fishkill_server <- function(input, output, session) {
  
  make_popup <- function(name, common_name, date_observed, desc, inat_link) {
    sprintf("<strong>Taxon:</strong> <strong>%s</strong> (%s)<br/> <strong>Date:</strong> %s <br/> <strong>Description:</strong> %s <br/> <a href='%s' target='_blank'>View at iNaturalist</a>",
            name, 
            common_name,
            date_observed, 
            desc, 
            inat_link
    ) %>% lapply(htmltools::HTML)
  }
  
  ns <- session$ns
  labels <- sprintf("<strong>Taxon:</strong> <strong>%s</strong> (%s) <br/> <strong>User:</strong> %s <br/> <strong>Date:</strong> %s <br/> <strong>Description:</strong> %s",
                    fish_kill_data$taxon_name, 
                    fish_kill_data$taxon_common_name,
                    fish_kill_data$user,
                    fish_kill_data$date_observed,
                    fish_kill_data$description
  ) %>% lapply(htmltools::HTML)
  
  
  output$fish_kills_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Map") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addCircleMarkers(data = fish_kill_data,  
                       label = labels,
                       popup = ~make_popup(taxon_name, taxon_common_name, date_observed, description, link),
                       color = "#972D15", 
                       weight = 1.5,
                       opacity =  1, fillOpacity = 1, 
                       labelOptions = labelOptions(style = list("font-size" = "14px")))
  })
  output$summary_table <- renderFormattable({formattable(summary_table,
                                                 align = c("l", "c", "c", "c", "c", "c", "c", "c"),
                                                 list(taxon_name = 
                                                        formatter("span", 
                                                                  style = ~style(color = "grey", 
                                                                                 font.weight = "bold"))))})
  output$rumsey_plot <- renderPlotly({
  rumsey_flows %>% 
    plot_ly(x = ~Date, y = ~ `Daily Flow Rumsey`) %>%
    add_lines(alpha = 0.6)  %>% 
    layout(title = 'Flow at Rumsey Gage (last month)',
           yaxis = list(title = 'Flow Rumsey Gage',
                        zeroline = TRUE),
           xaxis = list(title = 'Date'))
  })
  
  output$monitoring_plot <- renderPlotly({
    
    temps <- rep(c(70, 78, 80, 75, 67, 70, 79, 70, 70, 71), 3)
    rumsey_flows %>% 
      plot_ly(x = ~Date, y = ~ temps) %>%
      add_lines(alpha = 0.6)  %>% 
      layout(title = 'Temperature °F at Real Time Monitoring Station',
             yaxis = list(title = 'Temperature °F',
                          zeroline = TRUE),
             xaxis = list(title = 'Date'))
  })
}
























