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
    logg_full <<- c(logg_full,input$changed_value)
  })
  
  observe({
    if(current_time()-start_time>max_session_time){
      logger(path_out_log)
      stopApp()
    }
  })

  rv <- reactiveValues(
    tab_created = 1,
    tab_set_panel=c("Table_1"),
    logg_full = c()
  )
  
  logins <- reactiveValues()
  
  viewTabServer("Onglet_1",parent_session=session,logins=logins)
  
  
  ### Input trigger depuis le script js CreateTabButton.
  observeEvent(input$create_tab,{
    
    rv$tab_created <- rv$tab_created+1
    tab_id         <- paste0("Onglet_",rv$tab_created)
    tab_title      <- paste0("Table_",rv$tab_created)
    viewTabServer(tab_id,parent_session=session,logins=logins)
    
    insertTab("TABSETPANEL",
              target=tail(rv$tab_set_panel,n=1),
              position = 'after',
              tab=tabPanel(value=tab_title,
                           title = tab_title_removable(tab_title),
                           viewTabUi(tab_id)),
              select=TRUE
    )
    rv$tab_set_panel <- c(rv$tab_set_panel,tab_title)

  })
  
  
  observeEvent(input$remove_tab, {
    
    if(length(rv$tab_set_panel)>1){
      active_tab=input$TABSETPANEL
      removeTab(inputId = "TABSETPANEL", target = input$remove_tab)
      updateTabsetPanel(inputId = "TABSETPANEL",selected = active_tab)
      rv$tab_set_panel <- rv$tab_set_panel[!rv$tab_set_panel == input$remove_tab]
    }
    
  })
  

})
