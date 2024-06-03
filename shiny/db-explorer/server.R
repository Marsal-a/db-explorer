
library(shiny)


shinyServer(function(input, output) {
    
    connexion <- reactive({
        
        if(input$db=="SQLite"){
            source(paste0(getOption("path_db_explorer"),"/R/create_sqlite_db.R"))
            cona <- con_sqlite
        }else if(input$db=="Netezza"){
            cona <- connectNzSDSE()
        }
        
        return(cona)
    })
    
    schema_list=reactive({
        if(input$db=="SQLite"){
            schema <- 'default'
        }else if(input$db=="Netezza"){
            schema_table <- nzsdse:::nzSDSEListDb(connexion())
            schema <- schema_table$name
            # browser()
        }
        schema
    })
    
    table_list=eventReactive(c(input$db,input$schema),{
        if(input$db=="SQLite"){
            tables <- dbGetQuery(connexion(), "SELECT name FROM sqlite_master WHERE type='table'")
        }else if(input$db=="Netezza"){
            browser()
            tables_table <- nzsdse:::nzSDSEListTablesAndViews(connexion(),input$schema)
            tables <- tables_table$name
            
        }
        tables
    })
    
    
    output$ui_schemas <- renderUI({
        schema <- schema_list()
        req(schema)
        selectInput("schema","Selection du schéma :",choices = schema)
    })
    
    output$ui_tables <- renderUI({
        tables <- table_list()
        req(tables)
        selectInput("Table","Selection d'une table :",choices = tables)
    })
    
})
