shinyServer(function(input, output, session) {
  # Data Base Connection-------------------------------------------------------
  con <- psql_con()
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
  
})