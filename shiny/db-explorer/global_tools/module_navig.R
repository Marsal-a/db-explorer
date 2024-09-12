viewTabUi <- function(id,label="Tab"){
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        selectInput(NS(id,"navig_db"),"Sélection de la base :", choices = c("Select database to explore"="",db_choices)),
        uiOutput(NS(id,"ui_navig_schemas")),
        uiOutput(NS(id,"ui_navig_tables")),
      ),
      uiOutput(NS(id,"ui_navig_filters")),
      uiOutput(NS(id,"ui_navig_arrange")),
      uiOutput(NS(id,"ui_navig_filter_error")),
      uiOutput(NS(id,"ui_navig_view_vars")),
      actionButton(NS(id,"trigtest_DEV"), "button_test", icon = icon("sync", verify_fa = FALSE), style = "color:black"),
      width = 3
    ),
    mainPanel(
      fluidRow(
        column(12,htmlOutput(NS(id,"ui_navig_summary"))),
        # column(4,htmlOutput(NS(id,"ui_current_query")))
        ),
      fluidRow(
        column(10),
        column(1,uiOutput(NS(id,"ui_clip_current_query_button"))),
        column(1,uiOutput(NS(id,"ui_navig_dl_view_tab")))
      ),
      # uiOutput(NS(id,"ui_button_sql_query")),
      
      DT::dataTableOutput(NS(id,"ui_navig_dataviewer"),height = NULL), # le height = NULL permet de laisser la taille ajusté par CSS 
      width = 9
    )
  )
}

viewTabServer <- function(id,parent_session,logins){
  moduleServer(
    id = id,
    module = function(input,output,session){
      
      ### WORKS : 
      shinyjs::runjs(sprintf("
        $(document).keyup(function(event) {
            if ($('#%s').is(':focus') && (event.key == 'Enter')) {
                $('#%s').click();
            }
        });
    ", NS(id,'modal_pw'), NS(id,'modal_submit_login')))
      
      
      
      connexion_modal <- function(failed = FALSE,title='Login for PostgreSQL (Production)') {
        modalDialog(
          tags$h2(title),
          textInput(NS(id,'modal_username'), 'Username',value = system("whoami", intern = TRUE)),
          passwordInput(NS(id,'modal_pw'), 'Password'),
          if (failed)
            div(tags$b("Login incorrect", style = "color: red;")),
          footer=tagList(
            actionButton(NS(id,'modal_submit_login'), 'Submit'),
            modalButton('cancel')
          )
        )
      }
    
      observeEvent(input$navig_db,{
        if(isTruthy(connectors[[input$navig_db]]$req_login)){
          if(is.null(logins[[input$navig_db]])){
            showModal(connexion_modal(title = input$navig_db ))  
          }
        }
      })
      
      password_ok <- reactiveVal(FALSE)
      
      observeEvent(input$modal_submit_login,{
        test_con <- try(connectors[[input$navig_db]]$connect_function(user=input$modal_username,pw=input$modal_pw),silent=TRUE)
        if(inherits(test_con, "try-error")){
          showModal(connexion_modal(failed=T))
          updateTextInput(session,inputId = NS(id,"modal_pw"),value =NULL)
        }else{
          password_ok(TRUE)
          removeModal()
        }
      })
      
      NAVIG_connector <- eventReactive(c(input$navig_db,input$modal_submit_login),{ 
        
        if(is.null(logins[[input$navig_db]])){
          if(connectors[[input$navig_db]]$req_login){
            con <- connectors[[input$navig_db]]$connect_function(user=input$modal_username,pw=input$modal_pw)
          }else{
            con <- connectors[[input$navig_db]]$connect_function()
          }
          logins[[input$navig_db]] <- con
        }else{
          con <-  logins[[input$navig_db]]
        }
        return(con)
      })
      
      NAVIG_schemas <- eventReactive(c(input$navig_db,input$modal_submit_login,password_ok()),{

        req(input$navig_db)
        
        if(connectors[[input$navig_db]]$req_login & is.null(logins[[input$navig_db]])){
          req(input$modal_username)
          req(input$modal_pw)
          req(input$modal_submit_login)
          req(password_ok())
        }
        schemas <- connectors[[input$navig_db]]$list_schemas_function(NAVIG_connector())  
        
        return(schemas)
      })
      
      NAVIG_tables <- reactive({
        req(input$navig_schema)
        if(connectors[[input$navig_db]]$req_login & is.null(logins[[input$navig_db]])){
          req(input$modal_username)
          req(input$modal_pw)
          req(input$modal_submit_login)
          req(password_ok())
        }
        tables <- connectors[[input$navig_db]]$list_tables_function(NAVIG_connector(),input$navig_schema)
      })
      
      output$ui_navig_schemas <- renderUI({
        
        schema <- NAVIG_schemas()
        selectInput(NS(id,"navig_schema"),
                    "Sélection du schéma :",
                    choices = c('Please select the schema to explore'="",schema),
                    selectize=T,selected = default_schema)
        
      })
      
      output$ui_navig_tables  <- renderUI({
        
        tables <- NAVIG_tables()
        selectInput(NS(id,"navig_table"),
                    "Sélection de la table :",
                    choices = c('Select the table to explore'="",tables),selected = default_table,
                    selectize=T)
      })
      
      NAVIG_raw_tbl_lazy <- eventReactive(c(input$navig_table),{
        
        req(input$navig_table)
        raw_tbl_lazy <- connectors[[input$navig_db]]$remote_table_function(NAVIG_connector(),input$navig_schema,input$navig_table)
      
        return(raw_tbl_lazy)
      })

      NAVIG_varnames <- eventReactive(
        eventExpr = c(NAVIG_raw_tbl_lazy()),{
        
        req(input$navig_table)
        
        var_class = get_class(NAVIG_raw_tbl_lazy() %>% head(10) %>% collect())
        res <-names(var_class) %>%
          set_names(., paste0(., " {", var_class, "}"))
        
        return(res)
      })
      
      output$ui_navig_filters <- renderUI({
        req(input$navig_table)
        wellPanel(
          
          returnTextAreaInput(NS(id,"navig_data_filter"),
                              # label = inputLabelWithHelper("Filtrer la table :","helper"),
                              label = "Filtrer la table :",
                              value = "",
                              rows=2,
                              placeholder = "Ecrire une condition de filtre et appuyer sur Entrée"
          ),
          fluidRow(
            column(width = 10,actionLink(NS(id,"navig_clearFilters"), "Clear filters", icon = icon("sync", verify_fa = FALSE), style = "color:black")),
            column(width = 2,actionLink(NS(id,"navig_help_filter"), "", icon = icon("question-circle", verify_fa = FALSE), style = "color:#4b8a8c"))
          ),
          fluidRow(
            column(width = 10,checkboxInput(NS(id,"navig_filterByClick"),"Cliquer pour filtrer?", value = F)),
            # column(width = 2,actionLink(NS(id,"navig_help_cliquerFiltrer"),"",icon = icon("question-circle", verify_fa = FALSE)))
          ),
          # checkboxInput(NS(id,"navig_filterByClick"), "Cliquer pour filtrer?", value = F),
          checkboxInput(NS(id,"navig_cumulateFilters"), "Accumuler filtres?", value = F)
        )
      })
      
      observeEvent(input$navig_help_cliquerFiltrer,{
        showModal(modalDialog(
          tags$iframe(src="help/cliquerPourFiltrer.html", width="800", height="800", scrolling="no", seamless="seamless", frameBorder="0"),
          # includeHTML("tools/help/help_filters.html"),
          size="l",
          easyClose = TRUE
        ))
      })
      
      observeEvent(input$navig_help_filter,{
        browser()
        showModal(modalDialog(
          tags$iframe(src="help/help_filters.html", width="800", height="800", scrolling="no", seamless="seamless", frameBorder="0"),
          # includeHTML("tools/help/help_filters.html"),
          size="l"
        ))
      })
      
      output$ui_navig_arrange <- renderUI({
        req(input$navig_table)
        wellPanel(
          returnTextAreaInput(NS(id,"navig_data_arrange"),
                              label = "Trier la table :",
                              rows=2,
                              value = "",
                              placeholder = "Ex : desc(DATE) ou cliquer sur le nom d'une colonne"
          )
        )
      })
      
      output$ui_navig_view_vars <- renderUI({
        
        req(input$navig_table)
        
        vars <- NAVIG_varnames()
        wellPanel(
          selectInput(
            inputId   = NS(id,"navig_view_vars"), "Sélectionner les colonnes :",
            choices   = vars,
            selected  = vars,
            multiple  = TRUE,
            selectize = FALSE, 
            size = min(20, length(vars))+1
        ))
      })
      
      NAVIG_filter_error<-reactiveValues()
      
      NAVIG_prepared_tbl_lazy <- eventReactive(
        eventExpr=c(NAVIG_raw_tbl_lazy(),input$navig_data_filter,input$navig_data_arrange),
        ignoreInit = T,
        ignoreNULL = T,{
          
          req(input$navig_table)
          
          prepared_data <- NAVIG_raw_tbl_lazy()
          
          if(is.empty(input$navig_data_filter) & is.empty(input$navig_data_arrange)){
            
            isolate(NAVIG_filter_error[["value_filter_error"]] <- "")
            
          }else{
            
            if(!is.empty(input$navig_data_filter)){
              
              if (grepl("([^=!<>])=([^=])", input$navig_data_filter)){
                isolate(NAVIG_filter_error[["value_filter_error"]] <- "Filtre invalide : Ne pas utiliser le signe '=' dans un filtre, mais utiliser '==' à la place (ex : I_ELST==\"123456\"")
              }else{
                filter_cmd <- isolate(input$navig_data_filter) %>%
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
                  isolate(NAVIG_filter_error[["value_filter_error"]] <- "")
                  prepared_data <- filtered_data$result
                }else{
                  isolate(NAVIG_filter_error[["value_filter_error"]] <- paste0("Erreur dans la commande de filtre :\n",filtered_data$error$message,"\n",filtered_data$error$parent$message))
                }
              }
            }
            
            if(!is.empty(input$navig_data_arrange)){
              arrange_cmd <- input$navig_data_arrange
              if (!is.empty(arrange_cmd)) {
                arrange_cmd <- arrange_cmd %>%
                  strsplit(., split = "(&|,|\\s+)") %>%
                  unlist() %>%
                  .[!. == ""] %>%
                  paste0(collapse = ", ") %>%
                  (function(x) glue("arrange(x, {x})"))
              }
              
              safearrange=purrr::safely(function(tbl,arrange){
                tbl %>%
                  (function(x) if (!is.empty(arrange)) eval(parse(text = arrange)) else x)
              })
              arranged_data <- safearrange(prepared_data,arrange_cmd)
              # browser()
              if(is.null(arranged_data$error)){
                isolate(NAVIG_filter_error[["value_arrange_error"]] <- "")
                prepared_data <- arranged_data$result
              }else{
                isolate(NAVIG_filter_error[["value_arrange_error"]] <- paste0("Erreur dans la commande de tri :\n",arranged_data$error$message,"\n",arranged_data$error$parent$message))
              }
              
            }
          }
          return(prepared_data)
        }
      )
      
      NAVIG_collected_data <- eventReactive(
        eventExpr = c(NAVIG_prepared_tbl_lazy()),{
          
          prepared_tbl_lazy <- NAVIG_prepared_tbl_lazy()
          collect_tb <- function(lazy_tb,n){
            collected_tb <- lazy_tb %>% head(n) %>% collect()
            return(collected_tb)
          }
          
          ### La capture des erreurs prend un temps anormalement long pour certaines erreurs sur la condition de filtre
          ### Capture des erreurs tentative 1 avec purrr::safely : 
          safe_collect <- purrr::safely(collect_tb)
          collected_tb <- safe_collect(prepared_tbl_lazy,n_rows_collected)
          
          if(is.null(collected_tb$error)){
            collected_data <- collected_tb$result
          }else{
            
            isolate(NAVIG_filter_error[["value_filter_error"]] <- paste0("E2 ","Erreur dans la commande de filtre :\n\n",collected_tb$error$parent$message))
            collected_data <- NAVIG_raw_tbl_lazy() %>% head(n_rows_collected) %>% collect()
          }
          
          return(collected_data)
          
        }
      )
      
      NAVIG_displayTable <- eventReactive(
        eventExpr = c(NAVIG_collected_data(),input$navig_view_vars),
        ignoreNULL=T,ignoreInit = F,{
          
          req(input$navig_view_vars)
          req(all(input$navig_view_vars %in% NAVIG_varnames()))
          display_data <- NAVIG_collected_data()
          
          if(!is.null(input$navig_view_vars)){
            display_data <- select_at(display_data, .vars = input$navig_view_vars)
          }
          
          return(display_data)
          
        })
      
      NAVIG_tableSummary <- eventReactive(
        eventExpr = c(input$navig_table,input$navig_data_filter),
        ignoreInit = T,
        ignoreNULL=F,{
          
          req(input$navig_table)
          req(NAVIG_prepared_tbl_lazy())
          
          preview_information <- function(connexion,prepared_data,raw_data_lz,selected_table){
            current_query <- dbplyr::remote_query(prepared_data)
            rowcount_query <- glue::glue("SELECT COUNT(*) AS COUNT FROM (\n{current_query}\n) SUB")
            rowcount <- DBI::dbGetQuery(connexion, rowcount_query)
            rowcountPretty <- trimws(prettyNum(rowcount, big.mark = ","))
            
            colcount=dim(raw_data_lz)[2]
            # length(C_FAIT$lazy_query$vars)
            
            glue1 <- glue::glue("
            Prévisualisation de la table {crayon::bold$blue(input$navig_table)} ({min(n_rows_collected,as.integer(rowcount))} premières lignes)
          
            Nombre de lignes   : {crayon::bold$blue(rowcountPretty)}
            Nombre de colonnes : {crayon::bold$blue(colcount)}
            ")
            return(glue1)
          }
          
          safe_preview_information<-purrr::safely(preview_information)
          
          safe_results <- safe_preview_information(
            connexion      = NAVIG_connector(),
            prepared_data  = NAVIG_prepared_tbl_lazy(),
            raw_data_lz    = NAVIG_raw_tbl_lazy(),
            selected_table = input$navig_table)
          
          if(is.null(safe_results$error)){
            res <- safe_results$result
          }else{
            res <- safe_preview_information(NAVIG_connector(),NAVIG_raw_tbl_lazy(),NAVIG_raw_tbl_lazy(),input$navig_table)
          }
          
          return(res)
          
        }
      )  
      
      output$ui_navig_summary <- renderUI({
        ansi2html(NAVIG_tableSummary())
      })
      
      # NAVIG_dplyr_query <- eventReactive(
      #   eventExpr = c(input$navig_table,input$navig_view_vars,input$navig_data_filter,input$navig_data_arrange),
      #   ignoreNULL=T,ignoreInit = T,{
      #     
      #      # connexion_string <- glue::glue("con <- {trimws(deparse(body(connectors[[input$navig_db]]$connect_function))[2])}")
      #      # tbl_string <- glue::glue()
      #      # str<- glue::glue("tbl <- dplyr::tbl(con, dbplyr::in_catalog(dbplyr::sql(isolate(dbname)),".", tablename))")
      #     
      #   }
      # )
      
      # NAVIG_sql_query <- eventReactive(
      #   eventExpr = c(NAVIG_prepared_tbl_lazy()),
      #   ignoreNULL=T,ignoreInit = F,{
      #     # browser()
      #     lazy_tbl <- NAVIG_prepared_tbl_lazy()
      # 
      #     uncolored_query <- as.character(lazy_tbl %>% dbplyr::remote_query() %>% as.character())
      #     withr::local_options(list(dbplyr_use_colour = TRUE))
      #     colored_query <- lazy_tbl %>% dbplyr::remote_query()
      # 
      #     return(list(uncolored=uncolored_query,colored=colored_query))
      # })
      
      # output$ui_clip_current_query_button <- renderUI({
      #   req(input$navig_table)
      #   req(NAVIG_displayTable())
      #   # browser()
      #   query <- NAVIG_sql_query()$uncolored
      #   rclipButton(
      #     inputId = "clipbtn", 
      #     label = "Copy query", 
      #     clipText = query, 
      #     icon = icon("clipboard"),
      #     tooltip = "Click me to copy the content of the text field to the clipboard!",
      #     options = list(delay = list(show = 800, hide = 100), trigger = "hover")
      #   )
      # })
      
      # output$ui_button_sql_query <- renderUI({
      #   req(input$navig_table)
      #   req(NAVIG_displayTable())
      #   actionButton(NS(id,"sql_query_button"),"la")
      # })
      
      # observeEvent(input$sql_query_button, {
      #   query <- NAVIG_sql_query()
      #   
      #   showModal(modalDialog(
      #     title = "Requête SQL générée",
      #     rclipButton(
      #       inputId = "clipbtn", 
      #       label = "rclipButton Copy", 
      #       clipText = NAVIG_sql_query()$uncolored, 
      #       icon = icon("clipboard"),
      #       tooltip = "Click me to copy the content of the text field to the clipboard!",
      #       options = list(delay = list(show = 800, hide = 100), trigger = "hover")
      #     )
      #   ))
      # })
      
      
      output$ui_current_query <- renderUI({
        ansi2html(NAVIG_sql_query()$colored)
      })
          
      output$navig_dl_data <- downloadHandler(
        filename = function() {
          paste0("Extract_",n_rows_collected,"_rows_",input$navig_db,"_",input$navig_schema,"_",input$navig_table,"_",format(Sys.time(),"%Y%m%d_%H%M%S"),".csv")
        },
        content = function(file) {
          NAVIG_displayTable()  %>%
            data.table::fwrite(file=file,row.names = FALSE,bom=TRUE,sep=";")
        }
      )
      
      output$ui_navig_dl_view_tab <- renderUI({
        if(!is.null(NAVIG_displayTable())){
          downloadButton(NS(id,"navig_dl_data"),label="Ctrl + S",ic = "download",class = "alignright")
        }else{
          NULL
        }
      })
      
      output$ui_navig_filter_error <- renderUI({
        # browser()
        if (is.empty(NAVIG_filter_error[["value_filter_error"]]) & is.empty(NAVIG_filter_error[["value_arrange_error"]])) {
          return()
        }
        concat_error=paste(NAVIG_filter_error[["value_filter_error"]],NAVIG_filter_error[["value_arrange_error"]],sep = "\n")
        helpText(concat_error,class = "shiny-output-error")
        
      })
      
      output$ui_navig_dataviewer <- DT::renderDataTable({
        
        dat <- NAVIG_displayTable()
        
        style <- if (exists("bslib_current_version") && "4" %in% bslib_current_version()) "bootstrap4" else "bootstrap"
        
        search <- ""
        fbox <- if (nrow(dat) > 5e6) "none" else list(position = "top")
        fbox <- "none"
        ## for rounding

        IsDateWithoutTime <- function(col){
          if (inherits(col, "POSIXct")) {
            all(format(col[!is.na(col)], "%H:%M:%S") == "00:00:00")
          } else {
            FALSE
          }
        }
        
        dat <- dat %>% mutate(across(where(~IsDateWithoutTime(.)),~as.Date(format(., "%Y-%m-%d"))))
        
        dec <- input$view_dec %>%
          (function(x) ifelse(is.empty(x) || x < 0, 3, round(x, 0)))
        
        caption <- if (is.empty(input$view_tab_slice)) NULL else htmltools::tags$caption(glue("Table slice {input$view_tab_slice} will be applied on Download, Store, or Report"))
        
        columnclik = NS(id,"columnClicked")
        
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
              ordering=T,
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
            ## https://github.com/rstudio/DT/issues/146#issuecomment-534319155,
            callback=DT::JS(c(
              "table.on('dblclick', 'td',
                  function() {
                            var row = table.cell(this).index().row;
                            var col = table.cell(this).index().column;
                            Shiny.setInputValue('NAVIG_dt_dblclick', {dt_row: row, dt_col: col});
                }
              );",
              glue::glue("table.on('init.dt', function() {{
                table.table().header().addEventListener('click', function(event) {{
                  var target = event.target;
                  console.log($(this));
                  if (target.tagName === 'TH') {{
                    console.log('p1');
                    console.log('{columnclik}')
                    var colIdx = $(target).index();
                    var colName = table.column(colIdx).header().innerHTML;
                    Shiny.setInputValue('{columnclik}', colName);
                  }}
                }});
              });")
            )),
            selection = list(target = 'cell')
          )
          
        )
      })
      
      observeEvent(input$navig_db,{
        updateSelectizeInput(session=session,inputId = "navig_schema",selected = NULL)
        updateSelectizeInput(session=session,inputId = "navig_table",selected = NULL)
        updateTextAreaInput(session,inputId = "navig_data_filter",value ="")
        updateTextAreaInput(session,inputId = "navig_data_arrange",value ="")
        updateSelectInput(session,inputId = "navig_view_vars",selected ="")
        isolate(NAVIG_filter_error[["value_filter_error"]] <- "")
        isolate(NAVIG_filter_error[["value_arrange_error"]] <- "")
      })
      
      observeEvent(input$navig_schema,{
        updateSelectizeInput(session=session,inputId = "navig_table",selected = NULL)
        updateTextAreaInput(session,inputId = "navig_data_filter",value ="")
        updateTextAreaInput(session,inputId = "navig_data_arrange",value ="")
        updateSelectInput(session,inputId = "navig_view_vars",selected ="")
        isolate(NAVIG_filter_error[["value_filter_error"]] <- "")
        isolate(NAVIG_filter_error[["value_arrange_error"]] <- "")
        
      })
      
      observeEvent(input$navig_table,{
        updateTextAreaInput(session,inputId = "navig_data_filter",value ="")
        updateTextAreaInput(session,inputId = "navig_data_arrange",value ="")
        
        ### Le reset du select input permet de combler le cas d'un changement de table qui aurait les memes colonnes exactemetn.
        ### En effet dans ce cas l'inputId view_vars ne changerait pas et la table affiché ne serait pas raffraichie
        updateSelectInput(session,inputId = "navig_view_vars",selected ="")
        updateSelectInput(session,inputId = "navig_view_vars",selected =NAVIG_varnames())
        
        ## Reset du message d'erreur de filtre :
        isolate(NAVIG_filter_error[["value_filter_error"]]  <- "")
        isolate(NAVIG_filter_error[["value_arrange_error"]] <- "")
        
      })
      
      observeEvent(input$navig_clearFilters, {
        updateTextAreaInput(session,inputId = "navig_data_filter",value ="")
        isolate(NAVIG_filter_error[["value_filter_error"]] <- "")
      })
      
      observeEvent(input$ui_navig_dataviewer_cells_selected,{
        
        req(input$ui_navig_dataviewer_cells_selected)
        req(input$navig_filterByClick)
        
        string_filter <- c()
        for (row in seq_len(nrow(input$ui_navig_dataviewer_cells_selected))){
          col_name <- names(NAVIG_displayTable())[input$ui_navig_dataviewer_cells_selected[row,2]+1]
          value <- NAVIG_displayTable()[input$ui_navig_dataviewer_cells_selected[row,1],input$ui_navig_dataviewer_cells_selected[row,2]+1] %>% pull(col_name)
          string <- paste0(col_name,"==\"",value,"\"")
          string_filter[row]<-string
        }
        
        current_filter=strsplit(input$navig_data_filter," &\n")[[1]]
        if(input$navig_cumulateFilters){
          string_filter=unique(c(current_filter,string_filter))
        }
        
        str <- paste0(string_filter,collapse = " &\n")
        updateTextAreaInput(session,inputId = "navig_data_filter",value =str)
        
        session$sendCustomMessage(type="refocus",message=list(NS(id,"navig_data_filter")))
        
      })
      
      NAVIG_current_order_clicked <- reactiveVal("init_reserved_string")
      
      observeEvent(input$columnClicked,{
        #### Cet observeur écoute les cliques sur les entetes de colonnes du tableau principal.
        #### A chaque clique, on met à jour l'input data_arrange en récupérant le nom de la colonne cliquée.
        
        #### Fonctionement :
        # au premier clique, on assigne la valeur de la colonne dans l'input
        # si on reclique, alors la condition passe en desc() afin d'avoir un tri décroissant
        
        ### Stockage :
        # l'input qui stock la colonne cliqué s'apelle 'columnClicked' est issue d'une commande JS : Shiny.setInputValue
        # l'assignation est faite dans le callback du tableau principal.
        
        # Une fois la colonne cliqué, l'input prend la valeur de la colonne, et un second clique ne déclenche plus l'observeur.
        # On procède donc au stockage de l'input dans la reactive value current_order_clicked() et on ré-initialize l'input pour
        # qu'il soit re-triggerable au prochain clique.
        # un shiny::updateInput ne fonctionne pas alors on réinitialiser avec un session$sendCustomMessage et le script reset_colOrder.js
        # browser()
        string_order <- input$columnClicked
        
        reverse_order <- function(current_order){
          if(stringr::str_detect(current_order,"desc")){
            return(stringr::str_extract(current_order,pattern = "desc\\((.*?)\\)",group=1))
          }else{
            return(paste0("desc(",current_order,")"))
          }
        }
        
        if(string_order == NAVIG_current_order_clicked()){
          order_cmd <- reverse_order(string_order)
        }else{
          order_cmd <- string_order
        }
        
        if(string_order != "init_reserved_string"){
          NAVIG_current_order_clicked(order_cmd)
          session$sendCustomMessage(type="reset_colorder_navig", NS(id,"columnClicked"))
          updateTextAreaInput(session = session,inputId = "navig_data_arrange",value =order_cmd )
        }
        
        session$sendCustomMessage(type="refocus",message=list(NS(id,"navig_data_arrange")))
        
      })
      
      observeEvent(input$navig_table,{
        if(!is.null(input$navig_table) & input$navig_table!=""){
          new_title <- input$navig_table
          shinyjs::runjs(glue::glue("addCloseButtonToActiveTab('{new_title}', 'remove_navig_tab');"))
        }
      })
      
      observeEvent(input$trigtest_DEV,{
        browser()
      })
      
      
      
    }
  )
}