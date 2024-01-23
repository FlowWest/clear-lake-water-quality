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
    column(width = 12, 
           tags$h3("Big Valley Rancheria Contact"),
           tags$p(
             "Address: 2726 Mission Rancheria Road, Lakeport CA 95453"),
           tags$p(
             "Telephone: (707)263-3924"
           ),
           tags$p(
             "Environmental Director: Sarah Ryan, sryan@big-valley.net"),
           tags$p(
             "Environmental Specialist: Alicia Castellanos, acastellanos@big-valley.net")
  )
  )
}