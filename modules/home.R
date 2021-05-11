home_ui <- function(id) {
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
           tags$h4("Relevent Events Nearby")
           ),
    column(width = 9,
           leafletOutput(ns("fish_kills_map"), width = "100%", height = 400))
  )
}

home_server <- function(input, output, session) {
  
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
    addCircleMarkers(data = fish_kill_data,  label = labels,
                      color = "#972D15", 
                      weight = 1.5,
                      opacity =  1, fillOpacity = 1, 
                      labelOptions = labelOptions(style = list("font-size" = "14px")))
  })
  
}
























