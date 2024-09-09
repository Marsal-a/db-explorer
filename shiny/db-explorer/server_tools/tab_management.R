rv_navig_tab <- reactiveValues(
  tab_count   = 1,
  tab_list       = c("Table_1")
)

rv_sql_tab <- reactiveValues(
  tab_count   = 1,
  tab_list       = c("SQL_1")
)

### Input trigger depuis le script js CreateTabButton.
observeEvent(input$navig_create_tab,{

  rv_navig_tab$tab_count <- rv_navig_tab$tab_count+1
  tab_id         <- paste0("Onglet_",rv_navig_tab$tab_count)
  tab_title      <- paste0("Table_",rv_navig_tab$tab_count)
  viewTabServer(tab_id,parent_session=session,logins=logins)

  insertTab("Navig_tabset_panel",
            target=tail(rv_navig_tab$tab_list,n=1),
            position = 'after',
            tab=tabPanel(value=tab_title,
                         title = tab_title_removable(tab_title,removeInputName="remove_navig_tab"),
                         viewTabUi(tab_id)),
            select=TRUE
  )
  rv_navig_tab$tab_list <- c(rv_navig_tab$tab_list,tab_title)

})

observeEvent(input$sql_create_tab,{
  
  rv_sql_tab$tab_count <- rv_sql_tab$tab_count+1
  tab_id         <- paste0("Onglet_",rv_sql_tab$tab_count)
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
  if(length(rv_navig_tab$tab_list)>1){
    active_tab=input$Navig_tabset_panel
    removeTab(inputId = "Navig_tabset_panel", target = input$remove_navig_tab)
    updateTabsetPanel(inputId = "Navig_tabset_panel",selected = active_tab)
    rv_navig_tab$tab_list <- rv_navig_tab$tab_list[!rv_navig_tab$tab_list == input$remove_navig_tab]
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