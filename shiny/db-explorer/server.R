shinyServer(function(input, output, session) {
    
  enc <- getOption("db-explorer.encoding", "UTF-8")
  # source("init.R", encoding = enc, local = TRUE)
  
  for (file in list.files(c("tools"), pattern = "\\.(r|R)$", full.names = TRUE)) {
    source(file, encoding = enc, local = TRUE)
  }
  
  # NAVIG_connecteur_fonctions <- reactiveValues(
  #   list_schemas=NULL,
  #   list_tables=NULL
  # )
  # 
  # NAVIG_INIT_CONNECT <- observeEvent(input$db,{
  #   
  #   if(input$db=="SQLite"){
  #     source(paste0(getOption("path_db_explorer"),"/R/create_sqlite_db.R"))
  #     cona <- con_sqlite
  #   
  #     
  #   }else if(input$db=="Netezza"){
  #     
  #     list_schemas <- function(con){
  #       schemas_df <- nzsdse:::nzSDSEListDb(con)
  #       schemas <- schemas_df$name
  #       return(schemas)
  #     }
  #     list_tables <- function(con,dbname){
  #       tables <- all_nz_tables() %>% filter(DBNAME==dbname) %>% pull(OBJNAME)
  #       return(tables)
  #     }
  #     
  #     NAVIG_connecteur_fonctions[['list_schemas']] <- list_schemas
  #     NAVIG_connecteur_fonctions[['list_tables']] <- list_tables
  #     
  #   }
  # })
  
  
  connexion <- eventReactive(c(input$db,input$submit_pg_login),{
    ts_print("connexion_start")
    if(input$db=="SQLite"){
      source(paste0(getOption("path_db_explorer"),"/R/create_sqlite_db.R"))
      cona <- con_sqlite
    }else if(input$db=="Netezza"){
      cona <- connectNzSDSE()
      print("connec nz")
    }else if(input$db=="PostgreSQL - Prod"){
      # if(!is.null(pg_password_ok[["ok"]])){
      if(!is.null(pg_password_ok())){
        cona <- connectPostgreSDSE(user = input$username_pg, password=input$password_pg)
        updateTextInput(session = session,inputId = "password_pg",value = "")
      }else{
        cona <- NULL
      }
    }else if(input$db=="Oracle - Prod"){
      cona <- connectOraSDSE()
    }
    ts_print("connexion_end")
    return(cona)
  })
  
  schema_list <- eventReactive(c(input$db,input$submit_pg_login),{
    
    req(input$db)
    ts_print("schema_list")
    # schema<-NULL
    if(input$db=="SQLite"){
      schema <- 'default'
    }else if(input$db=="Netezza"){
      schema_table <- nzsdse:::nzSDSEListDb(connexion())
      schema <- schema_table$name
    }else if(input$db=="PostgreSQL - Prod"){
      # browser()
      req(input$password_pg)
      req(input$submit_pg_login)
      schema_table <- pgsdse:::pgsdseListSchemas(connexion(),"prod")
      schema <- schema_table$name
    }else if(input$db=="Oracle - Prod"){
      schema <- orasdse:::listSchemas(connexion())
    }
    schema
  }) 
  
  ### Gestion des fonctions de connexions aux base de données
  ## Fonctions pour la connexion à Netezza :
  
  all_nz_tables <- reactive({
    
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
    
    all_tables <- dplyr::as_tibble(DBI::dbGetQuery(connexion(), query_nz_tables))
    
    all_tables <- all_tables %>% filter(OBJTYPE %in% c("TABLE","VIEW"))
    
    ts_print("all_nz_table_end")
    return(all_tables)
    
  }) %>% bindCache(input$db) 
  # %>% 
  # bindEvent(c(input$db,input$password_pg,input$selected_schema))
  # Le bind event fait planter l'application, surement lié à un problème du package rJava référencé ici : 
  # https://forum.posit.co/t/error-in-jcheck-java-exception-no-description-because-tostring-failed/161960/11
  
  table_list=reactive({
  # table_list=eventReactive(input$selected_schema,{
      
    req(input$selected_schema)
    ts_print("table_list")
    
    if(input$db=="SQLite"){
      tables <- dbGetQuery(connexion(), "SELECT name FROM sqlite_master WHERE type='table'")
    }else if(input$db=="Netezza"){
      # tables_table <- nzsdse:::nzSDSEListTablesAndViews(connexion(),input$selected_schema)
      # tables <- tables_table$name
      # tables <- stringr::str_extract(tables, "^[^ ]*")
      # browser()
      tables <- all_nz_tables() %>% filter(DBNAME==input$selected_schema) %>% pull(OBJNAME)
      
    }else if(input$db=="PostgreSQL - Prod"){
      # req(input$password_pg)
      req(input$submit_pg_login)
      # browser()
      tables_table <- pgsdse:::pgsdseListTablesAndViews(connexion(),"prod",input$selected_schema)
      tables <- tables_table$name
    }else if(input$db=="Oracle - Prod"){
      tables_table <- orasdse:::oraSDSEListObjects(connexion(),input$selected_schema)
      tables <- tables_table$name
    }
    
    return(tables)
  })  
  # %>% bindCache(input$selected_schema) %>% bindEvent(input$selected_schema) 
  
  output$ui_schemas <- renderUI({
    ts_print("ui_schema")
    schema <- schema_list()
    
    selectInput("selected_schema",
                "Sélection du schéma :",
                choices = c('Please select the schema to explore'="",schema),
                selectize=T,selected = default_schema)
      
  })
  
  output$ui_tables  <- renderUI({
    ts_print("ui_tables")
    tables <- table_list()
    selectInput("selected_table",
                "Sélection de la table :",
                choices = c('Select the table to explore'="",tables),selected = default_table,
                selectize=T)
  })
  
  raw_data_lz <- eventReactive(c(input$selected_table),{

    req(input$selected_table)
    ts_print("raw_data_lz")  
    
    if(input$db=="SQLite"){
      lz <- dplyr::tbl(connexion(), input$selected_table)
    }else if(input$db=="Netezza"){
      table_name <- stringr::str_extract(input$selected_table, "^[^ ]*")
      if(packageVersion("dbplyr")>"2.2.0"){
        lz <- dplyr::tbl(connexion(), dbplyr::in_catalog(dbplyr::sql(isolate(input$selected_schema)),".", table_name))  
      }else{
        lz <- dplyr::tbl(connexion(), dbplyr::in_schema(dbplyr::sql(paste0(isolate(input$selected_schema),".")), table_name))  
      }
    }else if(input$db=="PostgreSQL - Prod"){
      # browser()
      lz <- dplyr::tbl(connexion(), dbplyr::in_schema(dbplyr::sql(isolate(input$selected_schema)), input$selected_table))
      
    }else if(input$db=="Oracle - Prod"){
      lz <- dplyr::tbl(connexion(), dbplyr::in_schema(dbplyr::sql(isolate(input$selected_schema)), input$selected_table))
    }
    
    return(lz)

  })
  
  varnames <- reactive({
    # browser()
    req(input$selected_table)
    
    var_class = get_class(raw_data_lz() %>% head(10) %>% collect())
    names(var_class) %>%
      set_names(., paste0(., " {", var_class, "}"))
  })
  
  output$ui_view_vars <- renderUI({
    
    req(input$selected_table)
    vars <- varnames()
    # browser()
    wellPanel(selectInput(
      "view_vars", "Select variables to show:",
      choices = vars,
      selected=vars,
      multiple = TRUE,
      selectize = FALSE, size = min(20, length(vars))+1
    ))
  })
    
  prepared_data_lz <- eventReactive(
    eventExpr=c(input$view_vars,input$data_filter),
    ignoreNULL = T,{
    # browser()
    req(input$selected_table)
    ts_print("prepared_data_lz1_init")
    
    prepared_data <- raw_data_lz()
    ts_print("prepared_data_lz2_raw_data_loaded") 
    
    
    if(is.empty(input$data_filter) & is.empty(input$data_arrange)){
      isolate(filter_error[["val"]] <- "")
    }else{
      if(!is.empty(input$data_filter)){
        if (grepl("([^=!<>])=([^=])", input$data_filter)){
          isolate(filter_error[["val"]] <- "Filtre invalide : Ne pas utiliser le signe '=' dans un filtre, mais utiliser '==' à la place (ex : I_ELST==\"123456\"")
        }else{
          filter_cmd <- isolate(input$data_filter) %>%
            gsub("\\n", "", .) %>%
            gsub("'", "\\\\'", .) %>%
            gsub("\"", "\'", .) %>%
            fix_smart()
          
          safefilter=purrr::safely(function(tbl,filter){
            tbl %>%
              (function(x) if (!is.empty(filter)) x %>% filter(!!rlang::parse_expr(filter)) else x)
          })
          filtered_data <- safefilter(prepared_data,filter_cmd)
          
          if(is.null(filtered_data$error)){
            isolate(filter_error[["val"]] <- "")
            prepared_data <- filtered_data$result
          }else{
            isolate(filter_error[["val"]] <- paste0("E1 ","Erreur dans la commande de filtre :\n\n",filtered_data$error$message,"\n",filtered_data$error$parent$message))
          }
          
        }
      }
    }
  

    
    ts_print("prepared_data_lz7") 
    return(prepared_data)
  })
    
  filter_error<-reactiveValues()
  
  displayTable <- eventReactive(
    eventExpr = c(input$view_vars,input$data_filter),
    ignoreNULL=T,ignoreInit = T,{
    
    # browser()
    req(input$selected_table)
    prepared_data <- prepared_data_lz()
    
    # browser()
    ### Capture des erreurs renvoyées par la base de données
    ### Ex : XXX=1 au lieu de ==
    
    if(!is.null(input$data_filter) & input$data_filter!=""){
      
      collect_tb <- function(lazy_tb,n){
        collected_tb <- lazy_tb %>% head(n) %>% collect()
        return(collected_tb)
      }
      
      ### La capture des erreurs prend un temps anormalement long pour certaines erreurs sur la condition de filtre
      ### Capture des erreurs tentative 1 avec purrr::safely : 
      safe_collect <- purrr::safely(collect_tb)
      collected_tb <- safe_collect(prepared_data,1000)

      if(is.null(collected_tb$error)){
        # isolate(filter_error[["val"]] <- "")
        tb <- collected_tb$result
      }else{
        # browser()
        isolate(filter_error[["val"]] <- paste0("E2 ","Erreur dans la commande de filtre :\n\n",collected_tb$error$parent$message))
        tb <- raw_data_lz() %>% head(1000) %>% collect()
      }
      
      ### Capture des erreurs tentative 2 avec try :
      # tb_try <- try({
      #   collect_tb(prepared_data,1000)
      # },silent=T)
      # 
      # if (inherits(tb_try, "try-error")) {
      #   isolate(filter_error[["val"]] <- paste0("E2 ","Erreur dans la commande de filtre :\n",attr(tb_try,"condition")$parent$message))
      #   tb <- raw_data_lz() %>% head(1000) %>% collect()
      # } else {
      #   tb <- tb_try
      # }
      
    }else{
      tb <- prepared_data %>% head(1000) %>% collect()
    }
    
    if(!is.null(input$view_vars)){
      tb <- select_at(tb, .vars = input$view_vars)
    }

    return(tb)
        
  })
  
  
  output$ui_summary <- renderUI({
    ansi2html(tableSummary())
  })
  
  
  tableSummary <- eventReactive(
    eventExpr = c(input$selected_table,input$data_filter,prepared_data_lz()),
    # eventExpr = displayTable(),
    ignoreInit = F,
    ignoreNULL=F,{
      
      req(input$selected_table)
      
      preview_information <- function(connexion,prepared_data,raw_data_lz,selected_table){
        current_query <- dbplyr::remote_query(prepared_data)
        rowcount_query <- glue::glue("SELECT COUNT(*) AS COUNT FROM (\n{current_query}\n) SUB")
        rowcount <- DBI::dbGetQuery(connexion, rowcount_query)
        rowcountPretty <- trimws(prettyNum(rowcount, big.mark = ","))
        
        colcount=dim(raw_data_lz)[2]
        
        glue1 <- glue::glue("
        Prévisualisation de la table {crayon::bold$blue(input$selected_table)} ({min(1000,as.integer(rowcount))} premières lignes)
      
        Nombre de lignes   : {crayon::bold$blue(rowcountPretty)}
        Nombre de colonnes : {crayon::bold$blue(colcount)}
  
        ")
        return(glue1)
      }
      
      safe_preview_information<-purrr::safely(preview_information)
      
      safe_results <- safe_preview_information(connexion(),prepared_data_lz(),raw_data_lz(),input$selected_table)
      
      if(is.null(safe_results$error)){
        res <- safe_results$result
      }else{
        res <- safe_preview_information(connexion(),raw_data_lz(),raw_data_lz(),input$selected_table)
      }
      
      
    
      return(res)
      
    }
  )  
  
  output$ui_filter_error <- renderUI({
    if (is.empty(filter_error[["val"]])) {
      return()
    }
    helpText(filter_error[["val"]],class = "shiny-output-error")
  })
  
  output$dataviewer <- DT::renderDataTable({
    # browser()
    # req(input$selected_table)
    ts_print("output$dataviewer")
    dat <- displayTable()
    ts_print("output$dataviewer")
    
    style <- if (exists("bslib_current_version") && "4" %in% bslib_current_version()) "bootstrap4" else "bootstrap"
          
    search <- ""
    fbox <- if (nrow(dat) > 5e6) "none" else list(position = "top")
    fbox <- "none"
    
    isBigFct <- sapply(dat, function(x) is.factor(x) && length(levels(x)) > 1000)
    if (sum(isBigFct) > 0) {
        dat[, isBigFct] <- select(dat, which(isBigFct)) %>% mutate_all(as.character)
    }
    
    ## for rounding
    isInt <- sapply(dat, function(x) is.integer(x))
    isDbl <- sapply(dat, is_double)
    dec <- input$view_dec %>%
        (function(x) ifelse(is.empty(x) || x < 0, 3, round(x, 0)))
    
    caption <- if (is.empty(input$view_tab_slice)) NULL else htmltools::tags$caption(glue("Table slice {input$view_tab_slice} will be applied on Download, Store, or Report"))


    withProgress(
        message = "Generating view table", value = 1,
        DT::datatable(
            dat,
            filter = fbox,
            rownames = FALSE,
            fillContainer = TRUE,
            escape = FALSE,
            style = style,
            options = list(
                dom="tpil",
                # dom="Bfrtip",
                stateSave = TRUE, ## maintains state
                search = list(search = search, regex = TRUE),
                columnDefs = list(
                    list(orderSequence = c("desc", "asc"), targets = "_all"),
                    list(className = "dt-center", targets = "_all")
                ),
                autoWidth = TRUE,
                processing = isTRUE(fbox == "none"),
                pageLength = 15,
                lengthMenu = list(c(15, 25, 50, 100, -1), c("15", "25", "50", "100", "All"))
            ),
            caption = caption,
            ## https://github.com/rstudio/DT/issues/146#issuecomment-534319155
            # callback = DT::JS('$(window).on("unload", function() { table.state.clear(); }); '),
            callback = DT::JS(c(
              "table.on( 'search.dt', function () {",
              "Shiny.setInputValue( 'search', table.search() );",
              "} );",'$(window).on("unload", function() { table.state.clear(); }); ')
            ),
            selection = list(target = 'cell')
        )

    )
  })

  observeEvent(input$selected_schema,{

      # updateSelectizeInput(session=session,inputId = "table",selected = "")

      updateSelectizeInput(session=session,inputId = "selected_table",selected = "")
      updateTextAreaInput(session,inputId = "data_filter",value ="")
      updateSelectInput(session,inputId = "view_vars",selected ="")
      isolate(filter_error[["val"]] <- "")

  })
  
  observeEvent(input$selected_table,{
      updateTextAreaInput(session,inputId = "data_filter",value ="")

      ### Le reset du select input permet de combler le cas d'un changement de table qui aurait les memes colonnes exactemetn.
      ### En effet dans ce cas l'inputId view_vars ne changerait pas et la table affiché ne serait pas raffraichie
      updateSelectInput(session,inputId = "view_vars",selected ="")
      updateSelectInput(session,inputId = "view_vars",selected =varnames())

      ## Reset du message d'erreur de filtre :
      isolate(filter_error[["val"]] <- "")

  })
  
  observeEvent(input$clearFilters, {
    updateTextAreaInput(session,inputId = "data_filter",value ="")
    isolate(filter_error[["val"]] <- "")
  })
  
  observeEvent(input$dataviewer_cells_selected,{

    req(input$dataviewer_cells_selected)
    req(input$filterByClick)

    string_filter <- c()
    for (row in seq_len(nrow(input$dataviewer_cells_selected))){
      col_name <- names(displayTable())[input$dataviewer_cells_selected[row,2]+1]
      value <- displayTable()[input$dataviewer_cells_selected[row,1],input$dataviewer_cells_selected[row,2]+1] %>% pull(col_name)
      string <- paste0(col_name,"==\"",value,"\"")
      string_filter[row]<-string
    }

    current_filter=strsplit(input$data_filter," &\n")[[1]]
    if(input$cumulateFilters){
      string_filter=unique(c(current_filter,string_filter))
    }

    str <- paste0(string_filter,collapse = " &\n")
    updateTextAreaInput(session,inputId = "data_filter",value =str)

    session$sendCustomMessage(type="refocus",message=list("data_filter"))

  })
  
  observeEvent(input$db,{
    # browser()
    if(input$db=="PostgreSQL - Prod"){
      showModal(pg_connexion_modal())

    }
    # showModal(modalDialog(
    #   tags$h2('Login for PostgreSQL (Production)'),
    #   textInput('username_pg', 'Username',value = system("whoami", intern = TRUE)),
    #   passwordInput('password_pg', 'Password'),
    #   footer=tagList(
    #     actionButton('submit_pg_login', 'Submit'),
    #     modalButton('cancel')
    #   )
    # ))
  })
  
  observeEvent(input$db,{
    # browser()
    updateSelectizeInput(session=session,inputId = "selected_schema",selected = "")
    updateSelectizeInput(session=session,inputId = "selected_table",selected = "")
    updateTextAreaInput(session,inputId = "data_filter",value ="")
    updateSelectInput(session,inputId = "view_vars",selected ="")
    isolate(filter_error[["val"]] <- "")
  })
  
  # pg_password_ok <- reactiveValues()
  pg_password_ok <- reactiveVal(NULL)
  
  observeEvent(input$submit_pg_login,{
    # browser()
    cona <- try(connectPostgreSDSE(user = input$username_pg, password=input$password_pg),silent=TRUE)
    if(inherits(cona, "try-error")){
      showModal(pg_connexion_modal(failed=T))
      updateTextInput(session,inputId = "password_pg",value =NULL)
    }else{
      # pg_password_ok[["ok"]]<-TRUE
      pg_password_ok(TRUE)
      removeModal()
    }
  })
  
  # observeEvent(input$cancel,{
  #   # browser()
  # })
  # observeEvent(input$search,{
  #   # browser()
  # })
  

  observeEvent(input$help_filter,{
  # observeEvent(input$`shinyhelper-modal_params`,{
  # observeEvent(c(input$help_filter,input$`shinyhelper-modal_params`),{
    showModal(modalDialog(
      tags$iframe(src="help/help_filters.html", width="800", height="800", scrolling="no", seamless="seamless", frameBorder="0"),
      # includeHTML("tools/help/help_filters.html"),
      size="l"
    ))
  })
  
  onStop(function(){
    logger(session,path_out_log)
    cat("Log onStop")
    stopApp()
  })
  
  session$onSessionEnded(function() {
    logger(session,path_out_log)
    cat("Log onSessionEnded")
    stopApp()
  })
  
  observeEvent(input$trigtest,{
    print_session()
    browser()
    # updateSelectizeInput(session=session,inputId = "selected_table",selected = "")
    # updateTextAreaInput(session,inputId = "data_filter",value ="")

  })
  
  current_time<-reactiveTimer()

  # observe({
  #   if(current_time()-start_time>max_session_time){
  #     logger(session,path_out_log)
  #     stopApp()
  #   }
  # })

})
