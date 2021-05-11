function(input, output, session) {
  callModule(home_server, 'app')
  callModule(water_quality_server, 'app')
  callModule(clear_lake_server, 'app')
}