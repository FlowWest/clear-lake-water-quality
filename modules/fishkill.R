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
           tags$h3("Fish Mortality"),
           tags$p("This map shows fish kill data in Clear Lake, California.  
                  The data comes from a citizen science iNaturalist project."),
           tags$h4("How to Use"), 
           tags$p("Visualize fish kill sites 
                  in or around the Big Valley Rancheria. Click on a marker to get
                  additional information and click the View at iNaturalist link to view origional record."),
           "Use", 
           tags$a(href = "https://www.inaturalist.org/projects/clear-lake-fish-kill-monitoring-project?tab=observations", "iNaturalist"), 
           "to record and view fish kills. Step by step instruction on reporting fish kills on Clear Lake with iNaturalist, can be found", 
                  tags$a("here", href="Reporting fish kills with iNaturalist 2021 V1B.pdf", 
                         target = "_blank"),
           tags$br(),
           tags$br(),
           downloadButton(ns('download'), "Download CSV")
           ),
    column(width = 8,
           leafletOutput(ns("fish_kills_map"), width = "100%", height = 700)
           )
  
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
      addCircleMarkers(popup = ~make_popup(taxon_name, taxon_common_name, date_observed, description, link),
                       color = ~pal(is_recent),
                       fillColor = ~pal(is_recent),
                       weight = 4.5,
                       opacity =  1, 
                       fillOpacity = ~ifelse(is_recent == "recent observation (15 days)", 3, .5) 
                       ) %>% 
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
  
  output$download <- downloadHandler(
    filename = "fish_kill_summary_table.csv",
    content = function(file) {
      write.csv(summary_table, file, row.names = FALSE)
    }
  )
}
























