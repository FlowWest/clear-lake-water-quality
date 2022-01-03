shinyUI( 
  navbarPage(
    title = div(tags$img(src="big_valley_rancheria.png", 
                         width = "30px", `margin-top`="-4px;"), 
                "Big Valley Rancheria",style = "font-size:20px;" ), 
    windowTitle = "BVR Dashboard",

    theme = shinythemes::shinytheme("yeti"),
    
    header = includeCSS("www/styles.css"), 
    #Calling the UI modules 
    tabPanel(title = tags$strong("Community Science",style = "font-size:20px;"), 
             fishkill_ui('app')),
    tabPanel(title = tags$strong("Historical Data", style = "font-size:20px;"), 
             water_quality_ui('app')), 
    tabPanel(title = tags$strong("Live Data", style = "font-size:20px;"), 
             wqdata_ui('app'))
  ))











