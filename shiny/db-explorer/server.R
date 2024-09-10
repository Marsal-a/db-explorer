shinyServer(function(input, output, session) {
    
  enc <- getOption("db-explorer.encoding", "UTF-8")
  # source("init.R", encoding = enc, local = TRUE)
  
  for (file in list.files(c("server_tools"), pattern = "\\.(r|R)$", full.names = TRUE)) {
    source(file, encoding = enc, local = TRUE)
  }
  
  onStop(function(){
    cat("Log onStop")
    stopApp()
  })
  
  session$onSessionEnded(function() {
    logger(path_out_log)
    cat("Log onSessionEnded")
    stopApp()
  })
  
  current_time<-reactiveTimer()

  observeEvent(input$changed_value,{
    logg_full <<- c(logg_full,input$changed_value)
  })
  
  observe({
    if(current_time()-start_time>max_session_time){
      logger(path_out_log)
      stopApp()
    }
  })
  
  logins <- reactiveValues()
  
  viewTabServer("Onglet_1",parent_session=session,logins=logins)
  
  viewSqlServer("Onglet_1",parent_session=session,logins=logins)
  
  rv <- reactiveValues(
    logg_full = c()
  )

})
