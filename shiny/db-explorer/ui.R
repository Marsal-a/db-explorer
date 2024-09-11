ui <- navbarPage(
  id="id_navbarpage",
  
  title = "Exploration des bases de données",
  
  ## icone favicon
  # tags$head(
  #   tags$link(rel = "shortcut icon", href = "11bb5ddc-170b-45fb-9526-91922a980df2.webp")
  # ),
            
  tabPanel("Navigation",
           shinyjs::useShinyjs(),
           # rclipboardSetup(),
           tags$head(
             tags$link(rel = "stylesheet", type = "text/css", href = "dbexplorer-style.css")
             # tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
           ),
           tags$head(
             tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")
           ),
           
           includeScript("www/js/CreateTabButton.js"),
           includeScript("www/js/changeActiveTabTitle.js"),
           includeScript("www/js/enter_password.js"),
           includeScript("www/js/event_log.js"),
           includeScript("www/js/refocus_cursor.js"),
           includeScript("www/js/reset_colOrder.js"),
           includeScript("www/js/returnTextAreaBinding.js"),
           includeScript("www/js/run_return.js"),
           

           tabsetPanel(id="Navig_tabset_panel",
                       type = "pills",
                       tabPanel(title = "Table_1",viewTabUi("NAVIG_TAB_1"))
           )
  ),
  tabPanel("Console SQL",
           tabsetPanel(id="ConsoleSQL_tabset_panel",
                       type = "pills",
                       tabPanel(title="SQL_1",viewSqlUi("SQL_TAB_1"))
           )
  )
)