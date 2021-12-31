fishkill_ui <- function(id) {
  ns <- NS(id)
  tagList(
      tags$head(
        tags$style(
          "body{
      height: 584px;
      width: 1005px;
      margin: auto;
          }"
        )
      ),
    column(width = 4, 
           tags$h3("Fish Kills"),
           tags$h4("Context"),
           tags$p("This page shows fish kill data in Clear Lake California.  
                  This data comes from a citizen science iNaturalist project. Elevation data
                  used is obtained from the USGS water data service using", 
                  tags$a("Clear Lake at Lakeport (11450000)", href="https://waterdata.usgs.gov/ca/nwis/uv?11450000", 
                         target = "_blank")),
           tags$h4("How to Use"), 
           tags$p("Use this dashboard to visualize fish kill sites 
                  in or around the Big Valley Rancheria. Hover over a 
                  marker on the map to see information about the fish 
                  kill recorded at that point. Click on a marker to get
                  additional information and click the View at iNaturalist link to view origional record."),
           "Use", 
           tags$a(href = "https://www.inaturalist.org/projects/clear-lake-fish-kill-monitoring-project?tab=observations", "iNaturalist"), 
           "to record and view Fish Kills.",
           tags$p("Step by step instruction on reporting Fish Kills on Clear Lake with iNaturalist, can be found", 
                  tags$a("here", href="Reporting fish kills with iNaturalist 2021 V1B.pdf", 
                         target = "_blank")),
           # tags$h4("Relevent Events Nearby"),
           # tags$hr()),
           # tabsetPanel(
             # type = "pills",
             # tabPanel(
             #   "Elevation at Clear Lake, Rumsey Gage",
             #   plotlyOutput(ns("clear_lake_plot"))),
             
             # tabPanel(
             #   "Real Time Monitoring",
             #   plotlyOutput(ns("monitoring_plot")))
           # ),
           tags$hr()),
    column(width = 8,
           leafletOutput(ns("fish_kills_map"), width = "100%", height = 700),
           tags$h4("Data Summary"),
           DT::dataTableOutput(ns("summary_table")), 
           tags$br(),
           tags$br())
  
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
  
  
  pal <- colorFactor(palette = c("#64f588", "#5cc8e0"), 
                     domain = c("recent observation (15 days)", 
                                "archived observation (16+ days)"), 
                     reverse = TRUE)
  
  output$fish_kills_map <- renderLeaflet({
    leaflet(fish_kill_data) %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Map") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addCircleMarkers(label = labels,
                       popup = ~make_popup(taxon_name, taxon_common_name, date_observed, description, link),
                       color = ~pal(is_recent),
                       fillColor = ~pal(is_recent),
                       weight = 4.5,
                       opacity =  1, 
                       fillOpacity = ~ifelse(is_recent == "recent observation (15 days)", 3, .5), 
                       labelOptions = labelOptions(style = list("font-size" = "20px"))) %>% 
      leaflet::addLegend("bottomright", pal = pal, values = ~is_recent,
                title = "Observation type",
                opacity = 1
      )
  })
  summary_table<- summary_table %>% 
    rename("Taxon Name" = taxon_name,
         "Common Name" = taxon_common_name,
         "Date Observed" = date_observed,
         'Description' = description) %>% 
    select(-longitude, -latitude)
  
  output$summary_table <- DT::renderDataTable(arrange(summary_table, desc('Date Observed')), 
                                              extensions = "Buttons", 
                                              options = list(dom = 'Btp', 
                                                             buttons = 
                                                               list(list(
                                                                 extend = 'collection',
                                                                 buttons = c('copy','csv'),
                                                                 text = 'Download'
                                                               ))))
  

  
  # output$clear_lake_plot <- renderPlotly({
  # clear_lake_elevation %>%
  #     filter(Date > Sys.Date() - 30) %>% 
  #   plot_ly(x = ~Date, y = ~ `Elevation (ft)`) %>%
  #   add_lines(alpha = 0.6)  %>% 
  #   layout(title = "Elevation at Lakeport, Clear Lake", 
  #          yaxis = list(title = 'Elevation (ft.)',
  #                       zeroline = TRUE),
  #          xaxis = list(title = 'Date'),
  #          margin = list(l = 20, 
  #                        r = 15, 
  #                        b = 15, 
  #                        t = 60, 
  #                        pad = 20))
  # })
  # output$monitoring_plot <- renderPlotly({
  # 
  #   temps <- rep(c(70, 78, 80, 75, 67, 70, 79, 70, 70, 71), 3)
  #   rumsey_flows %>%
  #     plot_ly(x = ~Date, y = ~ temps) %>%
  #     add_lines(alpha = 0.6)  %>%
  #     layout(title = 'Temperature F at Real Time Monitoring Station',
  #            yaxis = list(title = 'Temperature degrees F',
  #                         zeroline = TRUE),
  #            xaxis = list(title = 'Date'),
  #            margin = list(l = 20,
  #                          r = 15,
  #                          b = 15,
  #                          t = 60,
  #                          pad = 20))
  # })
}
























