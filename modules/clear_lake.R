clear_lake_ui <- function(id) {
  ns <- NS(id) 
  
  tagList(
    column(width = 3, 
           tags$h3("Clear Lake"), 
           tags$p("A little something about clear lake here....."), 
           tags$p("Clear Lake is a natural freshwater lake in Lake County in the U.S. state of California, north of Napa County and San Francisco. It is the largest natural freshwater lake wholly within the state, with 68 square miles (180 km2) of surface area. At 480,000 years, it is the oldest lake in North America.[2] It is the latest lake to occupy a site with a history of lakes stretching back at least 2,500,000 years.[3]")), 
    column(width = 9, 
           plotlyOutput(ns("lake_plot")), 
           plotlyOutput(ns("lake_boxplot")))
  )
}

clear_lake_server <- function(input, output, session) {
  
  output$lake_plot <- renderPlotly({
    clear_lake_wq %>% 
      plot_ly(x=~date, y=~gage_height_ft, type='scatter', mode='lines')
  })
  
  output$lake_boxplot <- renderPlotly({
    clear_lake_wq %>% 
      mutate(month = factor(month.abb[month(date)], levels = month.abb)) %>% 
      plot_ly(x=~month, y=~gage_height_ft, type='box')
  })
  
}