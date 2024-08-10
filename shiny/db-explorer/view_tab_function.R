viewTabUi <- function(id,label="Tab"){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        selectInput(ns("navig_db"),"Sélection de la base :", choices = c("Select database to explore"="",db_choices),selected=default_db),
        uiOutput(NS(id,"ui_navig_schemas")),
        uiOutput(NS(id,"ui_navig_tables")),
      ),
      actionButton(ns("trig"), "button_test", icon = icon("sync", verify_fa = FALSE), style = "color:black"),
      width=3
    ),
    
    mainPanel(
      DT::dataTableOutput(NS(id,"navig_dataviewer"),height = NULL),
      width=9,
    )
  )
  
}

viewTabServer <- function(id){
  moduleServer(
    id = id,
    module = function(input,output,session){
      
      NAVIG_connector <- eventReactive(input$navig_db,{ 
        
        con <- connectors[[input$navig_db]]$connect_function()
        
      })
      
      NAVIG_schemas <- eventReactive(c(input$navig_db,input$submit_pg_login),{
        req(input$navig_db)
        schemas <- connectors[[input$navig_db]]$list_schemas_function(NAVIG_connector())
        return(schemas)
      })
      
      NAVIG_tables <- reactive({
        req(input$navig_schema)
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
      
      NAVIG_raw_data_lz <- eventReactive(c(input$navig_table),{
        
        req(input$navig_table)
        connectors[[input$navig_db]]$remote_table_function(NAVIG_connector(),input$navig_schema,input$navig_table)
        
      })
      
      output$navig_dataviewer <- DT::renderDataTable({
        dat <- NAVIG_raw_data_lz() %>% head(1000) %>% collect()
        DT::datatable(dat)
        
      })
      
      output$navig_dataviewer2 <- DT::renderDataTable({
        dat <- NAVIG_raw_data_lz() %>% head(1000) %>% collect()
        DT::datatable(dat)
        
      })
      
      observeEvent(input$trig,{
        browser()
      })
      
    }
  )
}