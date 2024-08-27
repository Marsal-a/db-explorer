viewSqlUi <-  function(id,label="TabSQL"){
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        selectInput(NS(id,"sql_db"),"Sélection de la base :", choices = c("Select database to explore"="",db_choices),selected=default_db),
        uiOutput(NS(id,"ui_sql_schemas")),
        uiOutput(NS(id,"ui_sql_tables")),
        # actionButton(NS(id,"sql_trigtest_SQL"), "button_test", icon = icon("sync", verify_fa = FALSE), style = "color:black"),
      ),
      width=3
    ),
    mainPanel(
      fluidRow(uiOutput(NS(id,"ui_ace_editor"))),
      fluidRow(
        column(actionButton(NS(id,"sql_run_btn"), "Run"),width = 6),
        column(uiOutput(NS(id,"ui_sql_dl_btn")),width = 6)),
      tags$br(),
      DT::dataTableOutput(NS(id,"sql_dt"),height = NULL),
      width=9
    )
  )
}

viewSqlServer <- function(id,parent_session,logins){
  moduleServer(
    id = id,
    module = function(input,output,session){
      
      connexion_modal <- function(failed = FALSE,title='') {
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
      
      observeEvent(input$sql_db,{
        if(isTruthy(connectors[[input$sql_db]]$req_login)){
          if(is.null(logins[[input$sql_db]])){
            showModal(connexion_modal(title = input$sql_db ))  
          }
        }
      })
      
      password_ok <- reactiveVal(FALSE)
      
      observeEvent(input$modal_submit_login,{
        # browser()
        test_con <- try(connectors[[input$sql_db]]$connect_function(user=input$modal_username,pw=input$modal_pw),silent=TRUE)
        if(inherits(test_con, "try-error")){
          showModal(connexion_modal(failed=T))
          updateTextInput(session,inputId = NS(id,"modal_pw"),value =NULL)
        }else{
          password_ok(TRUE)
          removeModal()
        }
      })
      
      SQL_connector <- eventReactive(c(input$sql_db,input$modal_submit_login),{ 
        
        if(is.null(logins[[input$sql_db]])){
          if(connectors[[input$sql_db]]$req_login){
            con <- connectors[[input$sql_db]]$connect_function(user=input$modal_username,pw=input$modal_pw)
          }else{
            con <- connectors[[input$sql_db]]$connect_function()
          }
          logins[[input$sql_db]] <- con
          
        }else{
          con <-  logins[[input$sql_db]]
          
        }
        return(con)
        
      })
      
      SQL_schemas <- eventReactive(c(input$sql_db,input$modal_submit_login,password_ok()),{
        # browser()
        req(input$sql_db)
        
        if(connectors[[input$sql_db]]$req_login & is.null(logins[[input$sql_db]])){
          req(input$modal_username)
          req(input$modal_pw)
          req(input$modal_submit_login)
          req(password_ok())
        }
        schemas <- connectors[[input$sql_db]]$list_schemas_function(SQL_connector())  
        
        return(schemas)
      })
      
      SQL_tables <- reactive({
        req(input$sql_schema)
        if(connectors[[input$sql_db]]$req_login & is.null(logins[[input$sql_db]])){
          req(input$modal_username)
          req(input$modal_pw)
          req(input$modal_submit_login)
          req(password_ok())
        }
        tables <- connectors[[input$sql_db]]$list_tables_function(SQL_connector(),input$sql_schema)
      })      
      
      output$ui_sql_schemas <- renderUI({
        
        schema <- SQL_schemas()
        selectInput(NS(id,"sql_schema"),
                    "Sélection du schéma :",
                    choices = c('Please select the schema to explore'="",schema),
                    selectize=T,selected = default_schema)
        
      })
      
      output$ui_sql_tables  <- renderUI({
        
        tables <- SQL_tables()
        selectInput(NS(id,"sql_table"),
                    "Sélection de la table :",
                    multiple = F,
                    choices = c(tables),
                    selected = default_table,
                    selectize = FALSE, 
                    size = min(40, length(tables))+1)
      })
      
      output$ui_ace_editor <- renderUI({
        ## initially, only show completions in 'comps' (i.e., dplyr and selected dataset)
        aceEditor(NS(id,"sql_code"),
                  mode = "sql",
                  autoComplete = "live",
                  height = "100px",
                  value = "SELECT * FROM ...")
      })
      
      output$SQL_dl_data <- downloadHandler(
        filename = function() {
          paste0("Extract_",n_rows_collected,"_rows_",input$sql_db,"_",input$sql_schema,"_",input$sql_table,"_",format(Sys.time(),"%Y%m%d_%H%M%S"),".csv")
        },
        content = function(file) {
          SQL_table()  %>%
            data.table::fwrite(file=file,row.names = FALSE,bom=TRUE,sep=";")
        }
      )
      
      output$ui_sql_dl_btn <- renderUI({
        if(!is.null(SQL_table())){
          downloadButton(NS(id,"SQL_dl_data"),label="Ctrl + S",ic = "download",class = "alignright")
        }else{
          NULL
        }
      })
      
      
      SQL_table <- eventReactive(c(input$sql_run_btn,input$sql_table),ignoreInit = F,{
        
        safe_dbSendQuery <- purrr::safely(dbSendQuery)
        safe_rs <- safe_dbSendQuery(SQL_connector(),isolate(input$sql_code))
        # browser()
        if(is.null(safe_rs$error) & isTruthy(parent_session$input$changed==NS(id,"sql_run_btn"))){
          dt=dbFetch(safe_rs$result,1000)
          dbClearResult(safe_rs$result)
        }else{
          dt=NULL
        }
        return(dt)
        
      })
      
      output$sql_dt <- DT::renderDataTable({
        
        dat=SQL_table()
        
        d=withProgress(
          message = "Generating view table", value = 1,
          DT::datatable(dat,fillContainer = TRUE,
                        options = list(
                          dom="tpil",
                          pageLength = 15,
                          lengthMenu = list(c(15, 25, 50, 100, -1), c("15", "25", "50", "100", "All"))
                        )
          )
          
        )

        return(d)
      })
      
      SQL_comps <- reactive({
        req(input$sql_table)
        comps <- list()
        comps[[input$sql_table]] <- colnames(SQL_table())
        comps <- c(comps,sql_keywords)
        
      })
      
      observe({
        shinyAce::updateAceEditor(session,
                                  "sql_code",
                                  autoCompleters = c("static", "text", "rlang"),
                                  autoCompleteList = SQL_comps()
        )
      })
      
      updateAceEditor_timelapsed <- function(session,id,text,start_char,lapse=0.015){
        
        initial_text=substr(text,1,start_char-1)
        purrr::walk(seq(nchar(text)-start_char+1),function(i){
          new_val=substr(text,start_char,i+start_char-1)
          updateAceEditor(session=session,
                          editorId = id,
                          value=paste0(initial_text,new_val)
          )
          Sys.sleep(lapse)
        })
        
      }
      
      observeEvent(input$sql_schema,{
        # browser()
        if(!is.null(input$sql_schema) & input$sql_schema!=""){
          # freezeReactiveValue(input,"sql_code")
          # freezeReactiveValue(input,"sql_table")
          updatedAceInput=paste0("SELECT * FROM ",input$sql_schema,".")  
          updateAceEditor_timelapsed(session = session,id = "sql_code",text=updatedAceInput,start_char = 1)
        }
      })
      
      observeEvent(input$sql_table,priority = 10,{
        
        req(input$sql_schema)
        init_sql_code <- paste0("SELECT * FROM ",input$sql_schema,".")
        
        shinyjs::disable("sql_run_btn")
   
        if(input$sql_db=="Netezza"){
          updatedAceInput=paste0("SELECT * FROM ",input$sql_schema,".ADMIN.",input$sql_table,"\nLIMIT 100")
        }else if(input$sql_db=="Oracle - Prod"){
          updatedAceInput=paste0("SELECT * FROM ",input$sql_schema,".",input$sql_table,"\nWHERE (ROWNUM <= 100)")
        }else if(input$sql_db=="PostgreSQL - Prod"){
          updatedAceInput=paste0("SELECT * FROM ",input$sql_schema,".",input$sql_table,"\nLIMIT 100")
        }
        
        updateAceEditor_timelapsed(session = session,id = "sql_code",text=updatedAceInput,start_char = nchar(init_sql_code))
        

        
        Sys.sleep(0.8)
        
        shinyjs::enable("sql_run_btn")
         
      })
      
      observeEvent(input$sql_db,{
        
        updateSelectizeInput(session=session,inputId = "sql_schema",selected = "")
        updateSelectizeInput(session=session,inputId = "sql_table",selected = "")
        
      })
      
      observeEvent(input$sql_trigtest_SQL,{
        browser()
      })
      
      
    }
  )
}