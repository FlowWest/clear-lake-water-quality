home_UI <- function(id) {
  ns <- NS(id) 
  
  htmlTemplate("templates/home.html", 
               analyte_selector_ui = selectInput(ns("analyte_input"), "Analyte", 
                                                 choices = 1:10), 
               analyte_qualities_ui_output = uiOutput(ns("analyte_qualities_ui")))
}

home_server <- function(input, output, session) {
  
  ns <- session$ns
  
  output$analyte_qualities_ui <- renderUI({
    tagList(
      selectInput(ns("in1"), "fda", choices = letters),
      selectInput(ns("in2"), "fda", choices = LETTERS)
    )
  })
}