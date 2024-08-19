shinyServer(function(input, output, session) {
    
  enc <- getOption("db-explorer.encoding", "UTF-8")
  # source("init.R", encoding = enc, local = TRUE)
  
  for (file in list.files(c("tools"), pattern = "\\.(r|R)$", full.names = TRUE)) {
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
    # browser()
    # print(input$changed_value)
    logg_full <<- c(logg_full,input$changed_value)
  })
  
  observe({
    if(current_time()-start_time>max_session_time){
      logger(path_out_log)
      stopApp()
    }
  })

  rv <- reactiveValues(
    tab_count = 1,
    logg_full = c()
  )
  
  viewTabServer("Onglet_1")
 
  observeEvent(input$TABSETPANEL,{
    
    if(input$TABSETPANEL=="+"){
      
      rv$tab_count <- rv$tab_count+1
      tab_title=paste0("Table_",rv$tab_count)
      viewTabServer(paste0("Onglet_",rv$tab_count))
      insertTab("TABSETPANEL",
                target="+",
                tab=tabPanel(value=tab_title,title = tab_title_removable(tab_title),viewTabUi(paste0("Onglet_",rv$tab_count))),
                select=TRUE
                )
    }
  })
  
  observeEvent(input$remove_data_tab, {
    removeTab(inputId = "TABSETPANEL", target = input$remove_data_tab)
  })
  

})
