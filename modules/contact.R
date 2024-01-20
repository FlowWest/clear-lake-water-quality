contact_ui <- function(id) {
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
           tags$h3("Big Valley Rancheria Contact"),
           tags$p(
             "Address: 2726 Mission Road, Lakeport CA 95453" 
           ),
           tags$p(
             "Email: receptionist@big-valley.net"
           ),
           tags$p(
             "Tel:(707)263-3924"
           ),
           tags$p(
             "Fax:(707)264-6613"
           )
  )
  )
}