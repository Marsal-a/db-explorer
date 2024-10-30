rv_navig_tab <- reactiveValues(
  tab_count      = 1,
  tab_list_name  = c("Table_1"),
  tab_list_id    = c("NAVIG_TAB1")
)

rv_sql_tab <- reactiveValues(
  tab_count      = 1,
  tab_list_name  = c("SQL_1"),
  tab_list_id    = c("SQL_TAB1")
)

### Input trigger depuis le script js CreateTabButton.
observeEvent(input$navig_create_tab,{
  
  active_tab_id <- rv_navig_tab$tab_list_id[which(rv_navig_tab$tab_list_name==input$Navig_tabset_panel)]
  active_tab_db <- input[[NS(active_tab_id,"navig_db")]]

  rv_navig_tab$tab_count <- rv_navig_tab$tab_count+1
  tab_id         <- paste0("NAVIG_TAB",rv_navig_tab$tab_count)
  tab_title      <- paste0("Table_",rv_navig_tab$tab_count)
  viewTabServer(tab_id,parent_session=session,logins=logins)

  insertTab("Navig_tabset_panel",
            target=tail(rv_navig_tab$tab_list_name,n=1),
            position = 'after',
            tab=tabPanel(value=tab_title,
                         title = tab_title_removable(tab_title,removeInputName="remove_navig_tab"),
                         viewTabUi(tab_id)),
            select=TRUE
  )
  
  updateSelectInput(inputId = NS(tab_id,"navig_db"),selected=active_tab_db)
  
  rv_navig_tab$tab_list_name <- c(rv_navig_tab$tab_list_name,tab_title)
  rv_navig_tab$tab_list_id <- c(rv_navig_tab$tab_list_id,tab_id)

})

observeEvent(input$sql_create_tab,{
  
  rv_sql_tab$tab_count <- rv_sql_tab$tab_count+1
  tab_id         <- paste0("SQL_TAB",rv_sql_tab$tab_count)
  tab_title      <- paste0("SQL_",rv_sql_tab$tab_count)
  viewSqlServer(tab_id,parent_session=session,logins=logins)
  
  insertTab("ConsoleSQL_tabset_panel",
            target=tail(rv_sql_tab$tab_list,n=1),
            position = 'after',
            tab=tabPanel(value=tab_title,
                         title = tab_title_removable(tab_title,removeInputName="remove_sql_tab"),
                         viewSqlUi(tab_id)),
            select=TRUE
  )
  rv_sql_tab$tab_list <- c(rv_sql_tab$tab_list,tab_title)
  
})



observeEvent(input$remove_navig_tab, {
  
  if(length(rv_navig_tab$tab_list_id)>1){
    tab_name_to_remove <- input$remove_navig_tab
    tab_id_to_remove   <- rv_navig_tab$tab_list_id[which(rv_navig_tab$tab_list_name==tab_name_to_remove)]
    active_tab=input$Navig_tabset_panel
    removeTab(inputId = "Navig_tabset_panel", target =tab_name_to_remove)
    updateTabsetPanel(inputId = "Navig_tabset_panel",selected = active_tab)
    
    rv_navig_tab$tab_list_name <- rv_navig_tab$tab_list_name[!rv_navig_tab$tab_list_name == tab_name_to_remove]
    rv_navig_tab$tab_list_id   <- rv_navig_tab$tab_list_id[!rv_navig_tab$tab_list_id == tab_id_to_remove]
  }
})

observeEvent(input$remove_sql_tab, {
  if(length(rv_sql_tab$tab_list)>1){
    active_tab=input$ConsoleSQL_tabset_panel
    removeTab(inputId = "ConsoleSQL_tabset_panel", target = input$remove_sql_tab)
    updateTabsetPanel(inputId = "ConsoleSQL_tabset_panel",selected = active_tab)
    rv_sql_tab$tab_list <- rv_sql_tab$tab_list[!rv_sql_tab$tab_list == input$remove_sql_tab]
  }
})