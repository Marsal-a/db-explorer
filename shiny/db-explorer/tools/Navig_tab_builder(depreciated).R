build_1_viewing_tab <- function(tab){
  
  output[[paste0("ui_navig_db","_",tab)]] <- renderUI({
    selectInput(
      inputId = paste0("navig_db","_",tab),
      label = "Sélection de la base :",
      choices = c("Select database to explore"="",db_choices),
      selected=default_db
    )
  })
  
  assign(
    x=paste0("NAVIG_connector","_",tab),
    value = eventReactive(
      eventExpr = input$navig_db_tab1,{ 
        con <- connectors[[input[[paste0("navig_db","_",tab)]]]]$connect_function()
      }
    ),envir = parent.frame()
  )
  
  # browser()
  assign(
    x=paste0("NAVIG_all_nz_tables_function","_",tab),
    value = reactive({
      
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
      all_tables <- dplyr::as_tibble(DBI::dbGetQuery(get(paste0("NAVIG_connector","_",tab))(), query_nz_tables))
      
      all_tables <- all_tables %>% filter(OBJTYPE %in% c("TABLE","VIEW"))
      
      
      return(all_tables)
      
    }) %>% bindCache(input[[paste0("navig_db","_",tab)]]),
    envir = parent.frame()
  )
  
  assign(
    x=paste0("NAVIG_schemas","_",tab),
    value = eventReactive(c(input[[paste0("navig_db","_",tab)]],input$submit_pg_login),{
      req(input[[paste0("navig_db","_",tab)]])
      schemas <- connectors[[input[[paste0("navig_db","_",tab)]]]]$list_schemas_function(get(paste0("NAVIG_connector","_",tab))())
      return(schemas)
    }),
    envir = parent.frame()
  )
  
  output[[paste0("ui_navig_schemas","_",tab)]] <- renderUI({
    
    schema <- get(paste0("NAVIG_schemas","_",tab))()
    selectInput(paste0("navig_schema","_",tab),
                "Sélection du schéma :",
                choices = c('Please select the schema to explore'="",schema),
                selectize=T,selected = default_schema)
    
  })
  
  assign(
    x=paste0("NAVIG_tables","_",tab),
    value = reactive({
      req(input[[paste0("navig_schema","_",tab)]])
      tables <- connectors[[input[[paste0("navig_db","_",tab)]]]]$list_tables_function(
        con = get(paste0("NAVIG_connector","_",tab))(),
        dbname=input[[paste0("navig_schema","_",tab)]]
      )
      return(tables)
    }),
    envir = parent.frame()
  )
  
  output[[paste0("ui_navig_tables","_",tab)]] <- renderUI({
    
    tables <- get(paste0("NAVIG_tables","_",tab))()
    selectInput(paste0("navig_table","_",tab),
                "Sélection de la table :",
                choices = c('Select the table to explore'="",tables),selected = default_table,
                selectize=T)
  })
  
  
  assign(
    x=paste0("NAVIG_raw_data_lz","_",tab),
    value = eventReactive(c(input[[paste0("navig_table","_",tab)]]),{
      
      req(input[[paste0("navig_table","_",tab)]])
      connectors[[input[[paste0("navig_db","_",tab)]]]]$remote_table_function(
        con=get(paste0("NAVIG_connector","_",tab))(),
        dbname=input[[paste0("navig_schema","_",tab)]],
        table=input[[paste0("navig_table","_",tab)]])
      
    }),
    envir = parent.frame()
  )
  
  output[[paste0("navig_dataviewer","_",tab)]] <- DT::renderDataTable({
    dat <- get(paste0("NAVIG_raw_data_lz","_",tab))() %>% head(1000) %>% collect()
    DT::datatable(dat)
    
  })
  
}
build_1_viewing_tab("tab1")


