
server <- function (input, output, session) {
    
    #read in data 
    data1 <- reactive({
        if(is.null(input$file1)) return (NULL)
        inFile <- input$file1
        data.table::fread(inFile$datapath)
    })
    
    observeEvent(input$file1, {
        updateSelectInput(session,"varlist",choices=c(colnames (data1())))
    })
    
    new_data<-reactive({
        raw_data <- data1()
        # browser()
        DT::datatable(raw_data[, mget(input$varlist)], filter = "top", selection = 'multiple', escape=FALSE) 
    })
    
    output$mytable1 <- DT:: renderDataTable({
        new_data() 
    })

}

#Create the shiny app

# shinyApp(ui, server)
