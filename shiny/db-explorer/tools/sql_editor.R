sql_table <- eventReactive(input$run_sql,{
  
  ts_print("sql_table") 
  req(input$sql_code)
  
  # browser()
  rs=dbSendQuery(connexion(),input$sql_code)
  dt=dbFetch(rs,1000)
  ts_print("sql_table-2")
  
  # dbClearResult(rs)
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