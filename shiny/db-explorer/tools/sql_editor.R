
### PANNEAU DE CONNEXION POUR LA CONSOLE SQL. 
### CODE EN DOUBLE AVEC LA CONNEXION du tableau principales, 
### c'est trés mal, essayer de factoriser !


connexion_sql_panel <- eventReactive(c(input$db_sql),{
  if(input$db_sql=="SQLite"){
    source(paste0(getOption("path_db_explorer"),"/R/create_sqlite_db.R"))
    cona <- con_sqlite
  }else if(input$db_sql=="Netezza"){
    cona <- connectNzSDSE()
    print("connec nz")
  }else if(input$db_sql=="PostgreSQL - Prod"){
    # if(!is.null(pg_password_ok[["ok"]])){
    if(!is.null(pg_password_ok())){
      cona <- connectPostgreSDSE(user = input$username_pg, password=input$password_pg)
      updateTextInput(session = session,inputId = "password_pg",value = "")
    }
  }else if(input$db_sql=="Oracle - Prod"){
    cona <- connectOraSDSE()
  }
  return(cona)
})
schema_list_sql_panel <- eventReactive(c(input$db_sql,input$submit_pg_login),{
  
  req(input$db_sql)
  ts_print("schema_list")
  
  if(input$db_sql=="SQLite"){
    schema <- 'default'
  }else if(input$db_sql=="Netezza"){
    schema_table <- nzsdse:::nzSDSEListDb(connexion_sql_panel())
    schema <- schema_table$name
  }else if(input$db_sql=="PostgreSQL - Prod"){
    # browser()
    req(input$password_pg)
    req(input$submit_pg_login)
    schema_table <- pgsdse:::pgsdseListSchemas(connexion_sql_panel(),"prod")
    schema <- schema_table$name
  }else if(input$db_sql=="Oracle - Prod"){
    schema <- orasdse:::listSchemas(connexion_sql_panel())
  }
  schema
}) 

all_nz_tables_sql_panel <- reactive({
  
  ts_print("all_nz_table_start")
  
  # query_nz_tables <- "SELECT * FROM _V_OBJECT_DATA"
  query_nz_tables <- "
      SELECT
        _V_OBJECT_DATA.OBJTYPE,
        _V_OBJECT_DATA.OBJNAME,
        _V_OBJECT_DATA.DBNAME,
        _V_TABLE_ONLY_STORAGE_STAT.RELTUPLES AS nrows
      FROM _V_OBJECT_DATA
      LEFT JOIN  _V_TABLE_ONLY_STORAGE_STAT
      ON _V_OBJECT_DATA.OBJNAME=_V_TABLE_ONLY_STORAGE_STAT.TABLENAME
      WHERE LOWER(_V_OBJECT_DATA.OBJTYPE) IN ('table','view')
      ORDER BY _V_OBJECT_DATA.OBJNAME
      "
  
  all_tables <- dplyr::as_tibble(DBI::dbGetQuery(connexion_sql_panel(), query_nz_tables))
  
  all_tables <- all_tables %>% filter(OBJTYPE %in% c("TABLE","VIEW"))
  
  ts_print("all_nz_table_end")
  return(all_tables)
  
}) %>% bindCache(input$db_sql) 

table_list_sql_panel=reactive({

  
  req(input$selected_schema_sql_panel)
  
  if(input$db_sql=="SQLite"){
    tables <- dbGetQuery(connexion_sql_panel(), "SELECT name FROM sqlite_master WHERE type='table'")
  }else if(input$db_sql=="Netezza"){

    tables <- all_nz_tables_sql_panel() %>% filter(DBNAME==input$selected_schema_sql_panel) %>% pull(OBJNAME)
    
  }else if(input$db_sql=="PostgreSQL - Prod"){
    tables_table <- pgsdse:::pgsdseListTablesAndViews(connexion_sql_panel(),"prod",input$selected_schema_sql_panel)
    tables <- tables_table$name
  }else if(input$db_sql=="Oracle - Prod"){
    tables_table <- orasdse:::oraSDSEListObjects(connexion_sql_panel(),input$selected_schema_sql_panel)
    tables <- tables_table$name
  }
  
  return(tables)
})  

output$ui_schemas_sql_panel <- renderUI({
  schema <- schema_list_sql_panel()
  
  selectInput("selected_schema_sql_panel",
              "Sélection du schéma :",
              choices = c('Please select the schema to explore'="",schema),
              selectize=T,selected = default_schema)
  
})

output$ui_tables_sql_panel  <- renderUI({
  
  tables <- table_list_sql_panel()
  selectInput("selected_table_sql_panel",
              "Sélection de la table :",
              multiple = F,
              choices = c(tables),
              selected = default_table,
              selectize = FALSE, 
              size = min(40, length(tables))+1)
})

observeEvent(input$db_sql,{
  # browser()
  if(input$db_sql=="PostgreSQL - Prod"){
    showModal(pg_connexion_modal())
  }
})

sql_table <- eventReactive(input$run_sql,{
  
  # browser()
  ts_print("sql_table") 
  req(input$sql_code)
  
  print(input$sql_code)
  rs=dbSendQuery(connexion_sql_panel(),input$sql_code)
  dt=dbFetch(rs,1000)
  ts_print("sql_table-2")
  
  dbClearResult(rs)
  ts_print("sql_table-3")
  return(dt)
  
})

output$sql_dt <- DT::renderDataTable({
  
  ts_print("output$sql_dt")
  
  dat=sql_table()
  ts_print("output$sql_dt-2")
  
  d=withProgress(
    message = "Generating view table", value = 1,
    DT::datatable(dat) 
  )
  ts_print("output$sql_dt-end")
  return(d)
})


observeEvent(input$selected_schema_sql_panel,{
  
  newval=paste0("SELECT * FROM ",input$selected_schema_sql_panel,".")
  updateAceEditor(session=session,
                  editorId = "sql_code",
                  newval
                  )
})

observeEvent(input$selected_table_sql_panel,{
  
  if(input$db_sql=="Netezza"){
    newval=paste0("SELECT * FROM ",input$selected_schema_sql_panel,".ADMIN.",input$selected_table_sql_panel)  
  }else{
    newval=paste0("SELECT * FROM ",input$selected_schema_sql_panel,".",input$selected_table_sql_panel)
  }
  
  
  
  updateAceEditor(session=session,
                  editorId = "sql_code",
                  newval
  )
  
})