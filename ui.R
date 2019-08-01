# fluidPage(
#   title = "Clear Lake Water Quality Dashboard", 
#   theme = shinytheme('flatly'),
#   header = includeCSS('www/styles.css'),
#   home_UI('app')
# )
shinyUI(  
  navbarPage(
    title = div(tags$img(src="big_valley_rancheria.png", 
                         width = "30px", `margin-top`="-4px;"), 
                "Big Valley Rancheria"), 
    windowTitle = "BVR Dashboard",
    # shinythemes::themeSelector(),
    theme = shinythemes::shinytheme("yeti"),
    header = includeCSS("www/styles.css"), 
    tabPanel(title = "Water Quality", 
             home_ui('app')), 
    tabPanel(title = "Clear Lake", 
             clear_lake_ui('app'))
  ))











