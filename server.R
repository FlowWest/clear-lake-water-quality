function(input, output, session) {
  callModule(home_server, 'app')
  callModule(clear_lake_server, 'app')
}