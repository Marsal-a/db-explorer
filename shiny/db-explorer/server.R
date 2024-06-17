
library(shiny)
library(dplyr)
library(dbplyr)
library(purrr)
library(rlang)
# library(shinyAce)

shinyServer(function(input, output, session) {
    
    connexion <- reactive({
        if(input$db=="SQLite"){
            source(paste0(getOption("path_db_explorer"),"/R/create_sqlite_db.R"))
            cona <- con_sqlite
        }else if(input$db=="Netezza"){
            cona <- connectNzSDSE()
            print("connec nz")
        }else if(input$db=="PostgreSQL - Prod"){
          # browser()
          cona <- connectPostgreSDSE(user = input$username_pg, password=input$password_pg)
        }else if(input$db=="Oracle - Prod"){
            cona <- connectOraSDSE()
        }
        return(cona)
    })
    
    schema_list <- reactive({
        
        req(input$db)
        
        if(input$db=="SQLite"){
            schema <- 'default'
        }else if(input$db=="Netezza"){
            schema_table <- nzsdse:::nzSDSEListDb(connexion())
            schema <- schema_table$name
            # browser()
        }else if(input$db=="PostgreSQL - Prod"){
            req(input$password_pg)
            req(input$submit_pg_login)
            # browser()
            schema_table <- pgsdse:::pgsdseListSchemas(connexion(),"prod")
            schema <- schema_table$name
        }else if(input$db=="Oracle - Prod"){
            schema <- orasdse:::listSchemas(connexion())
        }
        schema
    })
    
    table_list=eventReactive(c(input$db,input$selected_schema),{
        
        req(input$db)
        req(input$selected_schema)
        
        if(input$db=="SQLite"){
            tables <- dbGetQuery(connexion(), "SELECT name FROM sqlite_master WHERE type='table'")
        }else if(input$db=="Netezza"){
          # browser()
            tables_table <- nzsdse:::nzSDSEListTablesAndViews(connexion(),input$selected_schema)
            tables <- tables_table$name
            tables <- stringr::str_extract(tables, "^[^ ]*")
        }else if(input$db=="PostgreSQL - Prod"){
            tables_table <- pgsdse:::pgsdseListTablesAndViews(connexion(),"prod",input$selected_schema)
            tables <- tables_table$name
        }else if(input$db=="Oracle - Prod"){
          tables_table <- orasdse:::oraSDSEListObjects(connexion(),input$selected_schema)
          tables <- tables_table$name
        }
        tables
    })
    
    output$ui_schemas <- renderUI({
        
        
        schema <- schema_list()
        
        selectInput("selected_schema",
                    "Sélection du schéma :",
                    choices = c('Please select the schema to explore'="",schema),
                    selectize=T,selected = default_schema)
        
    })
    
    output$ui_tables  <- renderUI({
      tables <- table_list()
      req(tables)
      
      selectInput("selected_table",
                  "Sélection de la table :",
                  choices = c('Select the table to explore'="",tables),selected = default_table,
                  selectize=T)
    })
    
    raw_data_lz <- reactive({
        
        req(input$selected_table)
        
        # browser()
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
    
    prepared_data_lz <- reactive({
      
      # browser()
      req(input$selected_table)
      prepared_data <- raw_data_lz()
      
      if(!is.null(input$data_filter) & input$data_filter!=""){
        # browser()
        filter_cmd <- isolate(input$data_filter) %>%
          gsub("\\n", "", .) %>%
          gsub("'", "\\\\'", .) %>%
          gsub("\"", "\'", .) %>%
          fix_smart()
        
       prepared_data <- try(
          prepared_data %>%
            (function(x) if (!is.empty(filter_cmd)) x %>% filter(!!rlang::parse_expr(filter_cmd)) else x),
          silent = TRUE
        )
      }
      return(prepared_data)
    })

    displayTable <- reactive({
      req(input$selected_table)
      req(input$view_vars)
      # browser()
      
      tb <- prepared_data_lz() %>% 
          head(1000) %>% 
          collect()
      if(!is.null(input$view_vars)){
          tb <- select_at(tb, .vars = input$view_vars)
      }
      
      return(tb)
        
    })
    
    output$dataviewer <- DT::renderDataTable({
            
            
            dat <- displayTable()
            
            style <- if (exists("bslib_current_version") && "4" %in% bslib_current_version()) "bootstrap4" else "bootstrap"
            
            
            search <- ""
            fbox <- if (nrow(dat) > 5e6) "none" else list(position = "top")
            
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
                    fillContainer = FALSE,
                    escape = FALSE,
                    style = style,
                    options = list(
                        stateSave = TRUE, ## maintains state
                        search = list(search = search, regex = TRUE),
                        columnDefs = list(
                            list(orderSequence = c("desc", "asc"), targets = "_all"),
                            list(className = "dt-center", targets = "_all")
                        ),
                        autoWidth = TRUE,
                        processing = isTRUE(fbox == "none"),
                        pageLength = {
                            # if (is.null(r_state$dataviewer_state$length)) 10 else r_state$dataviewer_state$length
                            10
                        },
                        lengthMenu = list(c(5, 10, 25, 50, -1), c("5", "10", "25", "50", "All"))
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
                # %>%
                #     (function(x) if (sum(isDbl) > 0) DT::formatRound(x, names(isDbl)[isDbl], dec) else x) %>%
                #     (function(x) if (sum(isInt) > 0) DT::formatRound(x, names(isInt)[isInt], 0) else x)
            )
        })
    
    output$table <- renderTable({
        
        req(input$selected_table)
        displayTable()
        
    })
    
    varnames <- reactive({
        req(input$selected_table)
      # browser()
      
        var_class = get_class(raw_data_lz() %>% head(10) %>% collect())
        names(var_class) %>%
            set_names(., paste0(., " {", var_class, "}"))
    })
    
    output$ui_filters <- renderUI({
      req(input$selected_table)
      wellPanel(
        checkboxInput("filterByClick", "Cliquer pour filtrer?", value = F),
        checkboxInput("cumulateFilters", "Accumuler filtres?", value = F),
        br(),
        actionLink("clearFilters", "Clear filters", icon = icon("sync", verify_fa = FALSE), style = "color:black"),
        returnTextAreaInput("data_filter",
                            label = "Data filter:",
                            value = "",
                            rows=2,
                            placeholder = "Ecrire une condition de filtre et appuyer sur Entrée"
                            # placeholder = "Ecrire une condition de filtre et appuyer sur Entrée. Exemple :\nPER_ID==\"123\"\nou\nPER_ID %in% c(\"1\",\"2\",\"3\")\n"
                            
        )
      )
    })
    
    output$ui_view_vars <- renderUI({
        
        req(input$selected_table)
        vars <- varnames()
        # browser()
        wellPanel(selectInput(
            "view_vars", "Select variables to show:",
            choices = vars,
            multiple = TRUE,
            selectize = FALSE, size = min(20, length(vars))+1
        ))
    })

    observeEvent(input$selected_schema,{

        # updateSelectizeInput(session=session,inputId = "table",selected = "")

        updateSelectizeInput(session=session,inputId = "selected_table",selected = "")
        updateTextAreaInput(session,inputId = "data_filter",value ="")

    })
    
    observeEvent(input$selected_table,{
        
        updateTextAreaInput(session,inputId = "data_filter",value ="")
        updateTextAreaInput(session,inputId = "data_filter",value ="")
        
    },priority = 1)
    
    observeEvent(input$clearFilters, {
        updateTextAreaInput(session,inputId = "data_filter",value ="")
    })
    
    observeEvent(input$dataviewer_cells_selected,{
      
      req(input$dataviewer_cells_selected)
      req(input$filterByClick)
      
      # browser()
      
      
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
      
    })
    
    observeEvent(input$db,{
      # browser()
      if(input$db=="PostgreSQL - Prod")
      showModal(modalDialog(
        tags$h2('Login for PostgreSQL (Production)'),
        textInput('username_pg', 'Username',value = system("whoami", intern = TRUE)),
        passwordInput('password_pg', 'Password'),
        footer=tagList(
          actionButton('submit_pg_login', 'Submit'),
          modalButton('cancel')
        )
      ))
    })
    
    observeEvent(input$submit_pg_login,{
      removeModal()
    })
    
    observeEvent(input$search,{
      browser()
    })
    
    session$onSessionEnded(function() {
      cat("Log onSessionEnded")
      stopApp()
    })
    
    observeEvent(input$trigtest,{
      

      # updateSelectizeInput(session=session,inputId = "selected_table",selected = "")
      updateTextAreaInput(session,inputId = "data_filter",value ="")
      browser()
      
    })

})
