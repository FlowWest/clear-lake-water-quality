shinyUI(  
  navbarPage(
    title = div(tags$img(src="big_valley_rancheria.png", 
                         width = "30px", `margin-top`="-4px;"), 
                "Big Valley Rancheria"), 
    windowTitle = "BVR Dashboard",
    # shinythemes::themeSelector(),
    theme = shinythemes::shinytheme("lumen"),
    header = includeCSS("www/styles.css"), 
    tabPanel(title = "Fish Kill Data", 
             home_ui('app')),
    tabPanel(title = "Water Quality", 
             water_quality_ui('app')), 
    tabPanel(title = "Clear Lake", 
             clear_lake_ui('app'))
  ))











