shinyUI( 
  navbarPage(
    title = div(tags$img(src="big_valley_rancheria.png", 
                         width = "30px", `margin-top`="-4px;"),
                "Clear Lake Water Quality Monitoring",style = "font-size:20px;" ),
    windowTitle = "BVR Dashboard",

    theme = shinythemes::shinytheme("yeti"),
    
    header = includeCSS("www/styles.css"), 
    tabPanel(title = tags$strong("Live Data", style = "font-size:20px;"), 
             wqdata_ui('app')),
    # tabPanel(title = tags$strong("Historic Data", style = "font-size:20px;"), 
    #          historical_data_ui('app')),
    tabPanel(title = tags$strong("Community Science",style = "font-size:20px;"), 
             fishkill_ui('app')),
    tabPanel(title = tags$strong("Contact",style = "font-size:20px;"), 
             contact_ui('app'))
    
  ))











