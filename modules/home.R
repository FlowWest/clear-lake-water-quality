home_UI <- function(id) {
  ns <- NS(id) 
  
  # htmlTemplate("templates/home.html", 
  #              analyte_selector_ui = selectInput(ns("analyte_input"), "Analyte", 
  #                                                choices = 1:10), 
  #              analyte_qualities_ui_output = uiOutput(ns("analyte_qualities_ui")))
  tagList(
    fluidRow(
      column(width = 12,
             tags$div(
               tags$img(src = 'big_valley_rancheria.png'),
               tags$h1('Clear Lake Water Quality'))
             )
    ),
    fluidRow(
      column(width = 12, class = 'col-md-6',
             uiOutput(ns('select_analyte')),
             uiOutput(ns('select_depth')),
             plotlyOutput(ns('analyte_plot'))),
      column(width = 12, class = 'col-md-6',
             leafletOutput(ns('sites_map')),
             tags$h2('Clear Lake Elevation'), 
             plotlyOutput(ns('lake_elev_plot'), height = '200px'))
    ),
    fluidRow(
      column(width = 12,
             tags$img(src = 'TransLogoTreb.png', width = '20%'),
             tags$p('App developed and maintained by', 
                    tags$a('Emanuel Rodriguez', href = 'mailto:erodriguez@flowwest.com', target = '_blank')))
    )
  )
  
}

home_server <- function(input, output, session) {
  
  ns <- session$ns
  
  # output$analyte_qualities_ui <- renderUI({
  #   tagList(
  #     selectInput(ns("in1"), "fda", choices = letters),
  #     selectInput(ns("in2"), "fda", choices = LETTERS)
  #   )
  # })
  output$select_analyte <- renderUI({
    selectInput('selected_analyte', label = 'Select Analyte',
                choices = c("pH", "Dissolved Oxygen"))
  })
  
  output$select_depth <- renderUI({
    selectInput('selected_depth', label = 'Select Depth', choices = 1:4)
  })
  
  output$analyte_plot <- renderPlotly(
    mtcars %>% 
      plot_ly(x = ~mpg, y = ~cyl)
  )
  
  output$sites_map <- renderLeaflet(
    leaflet() %>% 
      addTiles() %>% 
      setView(lng=-122.767084, lat=39.028363, zoom = 11)
  )
  
  output$lake_elev_plot <- renderPlotly(
    mtcars %>% 
      plot_ly(x = ~disp, y = ~qsec, type = 'scatter', mode = 'lines')
  )
}