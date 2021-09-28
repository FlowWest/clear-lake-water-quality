shinyUI(  
  navbarPage(
    title = div(tags$img(src="big_valley_rancheria.png", 
                         width = "30px", `margin-top`="-4px;"), 
                "Big Valley Rancheria",style = "font-size:20px;" ), 
    windowTitle = "BVR Dashboard",
    theme = shinythemes::shinytheme("yeti"),
    
    header = includeCSS("www/styles.css"), 
    
    tabPanel(title = tags$strong("Fish Kill Data",style = "font-size:20px;"), 
             fishkill_ui('app')),
    tabPanel(title = tags$strong("Water Quality", style = "font-size:20px;"), 
             water_quality_ui('app')), 
    tabPanel(title = tags$strong("Clear Lake", style = "font-size:20px;"), 
             clear_lake_ui('app'))
  ))











