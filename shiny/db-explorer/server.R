
library(shiny)


shinyServer(function(input, output) {
    
    connexion <- reactive({
        
        if(input$db=="sqllite"){
            source(paste0(getOption("path_db_explorer"),"/R/create_sqlite_db.R"))
            cona <- con_sqlite
            print("sqliteaa")
        }else if(input$db=="nz"){
            print("nz")
        }
        
        return(cona)
    })
    
    schema_list=reactive({
        req(input$db)
        if(input[['db']]=="sqlite"){
            
        }
    })
    
    output$ui_schemas <- renderUI({
        schema <- schema_list()
        req(schema)
        selectInput("schema","Selection du schéma :",choices = "")
    })
    
})
