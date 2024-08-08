download_handler <- function(id, label = "", fun = id, fn, type = "csv", caption = "Save to csv", class = "", ic = "download", btn = "link",...) {
  output[[id]] <- downloadHandler(
    filename = function() {
      if (is.function(fn)) fn <- fn()
      if (is.function(type)) type <- type()
      paste0(fn, ".", type)
    },
    content = function(path) {
      fun(path, ...)
    }
  )
}

dl_sql_tab <- function(file) {
  sql_table()  %>%
    write.csv2(file, row.names = FALSE)
}

dl_view_tab <- function(file) {
  displayTable()  %>%
    write.csv2(file, row.names = FALSE,fileEncoding = "utf-8")
}


download_handler(
  id = "dl_view_tab",
  fun = dl_view_tab,
  fn = function() {
    paste0("Extract_",n_rows_collected,"_rows_",input$db,"_",input$selected_schema,"_",input$selected_table,"_",format(Sys.time(),"%Y%m%d_%H%M%S"))
  }
)

download_handler(
  id = "dl_sql_tab",
  fun = dl_sql_tab,
  fn = function() {
    paste0("Extract_",n_rows_collected,"_rows_",input$db_sql,"_",input$selected_schema_sql_panel,"_",input$selected_table_sql_panel,"_",format(Sys.time(),"%Y%m%d_%H%M%S"))
  }
)


