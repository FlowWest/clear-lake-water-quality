clear_lake_ui <- function(id) {
  ns <- NS(id) 
  
  tagList(      tags$head(
    tags$style(
      "body{
      height: 584px;
      width: 1005px;
      margin: auto;
          }"
    )
  ),
    column(width = 3, 
           tags$h3("Clear Lake"),
           tags$p("Clear Lake is a natural freshwater lake in Lake County in 
                  the U.S. state of California, north of Napa County and 
                  San Francisco. It is the largest natural freshwater lake
                  wholly within the state, with 68 square miles (180 km2) of
                  surface area. At 480,000 years, it is the oldest lake in
                  North America.[2] It is the latest lake to occupy a site
                  with a history of lakes stretching back at
                  least 2,500,000 years.[3]"),
           dateRangeInput('dateRange',label = "Input date range of interest
                          (yyyy-mm-dd):",start = "2010-01-01", end=Sys.Date(),
                             separator = " - "),
           # textOutput("SliderText"),
           ), 
    column(width = 9,
           plotlyOutput(ns("lake_plot")), 
           plotlyOutput(ns("lake_boxplot"))
           )
  )

}

#look into add date range selector
#add drop down to select years/multiple years
#plot should be a time series
#compare water conditions between years

clear_lake_server <- function(input, output, session) {
  ns <- session$ns
  # Dates <- reactiveValues()
  # observe({
  #   Dates$SelectedDates <- c(as.character(input$dateRange[1]),as.character(input$dateRange[2]))
  # })
  # output$SliderText <- renderText({Dates$SelectedDates})
# 
#   selected_elevation_over_time <- reactive({
#     clear_lake_elevation %>% 
#       filter(Date %in% (input$dateRange[1]:input$dateRange[2]))
  # })
  
  output$lake_plot <- renderPlotly({
    # selected_elevation_over_time %>%
    clear_lake_elevation %>%   
    plot_ly(x = ~Date, y = ~ `Elevation (ft)`) %>%
      add_lines(alpha = 0.6)  %>%
      layout(title = "Elevation at Lakeport, Clear Lake",
             yaxis = list(title = 'Elevation (ft.)',
                          zeroline = TRUE),
             xaxis = list(title = 'Date'))
  })
  # 
  output$lake_boxplot <- renderPlotly({
    clear_lake_elevation %>%
      mutate(month = factor(month.abb[month(Date)],
                            levels = month.abb)) %>%
      plot_ly(x= ~month, y = ~ `Elevation (ft)`) %>%
      add_boxplot(boxpoints = "outliers")
  })

}