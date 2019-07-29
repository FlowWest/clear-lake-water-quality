# fluidPage(
#   title = "Clear Lake Water Quality Dashboard", 
#   theme = shinytheme('flatly'),
#   header = includeCSS('www/styles.css'),
#   home_UI('app')
# )

navbarPage(
  title = div(tags$img(src="big_valley_rancheria.png", 
                       width = "30px", `margin-top`="-4px")), 
  # shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("sandstone"),
  header = includeCSS("www/styles.css"), 
  tabPanel(title = "Big Valley Rancheria Water Quality", 
           home_ui('app'))
)