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

  observe({
    if(current_time()-start_time>max_session_time){
      logger(path_out_log)
      stopApp()
    }
  })
  
  session$options$FilterClick    <- FALSE
  session$options$CumulateFilter <- FALSE
  
  logins <- reactiveValues()
  
  viewTabServer("NAVIG_TAB1",parent_session=session,logins=logins)

  viewSqlServer("SQL_TAB1",parent_session=session,logins=logins)

  
  rv <- reactiveValues(
    logg_full = c(),
    window_height = NULL,
    window_width = NULL  
  )
  
  observeEvent(input$changed_value,{
    logg_full <<- c(logg_full,input$changed_value)
  })
  
  observeEvent(input$windowHeight,{
    window_height <<- input$windowHeight
  })
  observeEvent(input$windowWidth,{
    window_width <<- input$windowWidth
  })
  

})
