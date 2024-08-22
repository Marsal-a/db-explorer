.libPaths(c( "~/R_Commun/Adam/custom_lib/db-explorer/",.libPaths()))
# .libPaths(.libPaths()[1])
unloadNamespace("nzsdse")
unloadNamespace("dbplyr")
unloadNamespace("radiant.data")
unloadNamespace("plotly")
unloadNamespace("broom")
unloadNamespace("tidyr")
unloadNamespace("dplyr")
unloadNamespace("readr")
unloadNamespace("shinyFiles")
unloadNamespace("car")
unloadNamespace("rio")
unloadNamespace("haven")
unloadNamespace("rio")
unloadNamespace("patchwork")
unloadNamespace("ggplot2")
unloadNamespace("tibble")
unloadNamespace("pillar")
# unloadNamespace("arrow")
unloadNamespace("tidyselect")
unloadNamespace("pgsdse")
unloadNamespace("orasdse")
unloadNamespace("RJDBC")
unloadNamespace("crayon")
unloadNamespace("RPostgres")
unloadNamespace("ROracle")
unloadNamespace("DBI")
unloadNamespace("stringr")
unloadNamespace("usethis")
unloadNamespace("forcats")
unloadNamespace("scales")
unloadNamespace("gtable")
unloadNamespace("glue")

library(crayon)
library(shinyAce)
library(shiny)
library(shinyhelper)
library(glue)
library(bslib)
library(dplyr)
library(DBI)
library(dbplyr)
library(purrr)
library(rlang)
library(orasdse)
library(pgsdse)
library(nzsdse)
library(fontawesome)
library(stringi)
library(stringr)
library(sodium)

ui <- navbarPage(
  id="id_navbarpage",
  
  title = "Exploration des bases de données",
  
  tabPanel("Navigation",
           shinyjs::useShinyjs(),
           tags$head(
             tags$link(rel = "stylesheet", type = "text/css", href = "dbexplorer-style.css")
             # tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
           ),
           includeScript("www/js/CreateTabButton.js"),
           includeScript("www/js/changeActiveTabTitle.js"),
           includeScript("www/js/enter_password.js"),
           includeScript("www/js/event_log.js"),
           includeScript("www/js/refocus_cursor.js"),
           includeScript("www/js/reset_colOrder.js"),
           includeScript("www/js/returnTextAreaBinding.js"),
           includeScript("www/js/run_return.js"),
           

           tabsetPanel(id="TABSETPANEL",type = "pills",
                       tabPanel(title = "Table_1",viewTabUi("Onglet_1"))
           )
  ),
  tabPanel("Console SQL",
    sidebarLayout(
     sidebarPanel(
       wellPanel(
        selectInput("db_sql","Sélection de la base :", choices = c("Select database to explore"="",db_choices),selected=default_db),
        uiOutput("ui_schemas_sql_panel"),
        uiOutput("ui_tables_sql_panel"),
        actionButton("trigtest_SQL", "button_test", icon = icon("sync", verify_fa = FALSE), style = "color:black"),
       ),
       width=3
     ),
     mainPanel(
       p("Console SQL en construction"),
       fluidRow(uiOutput("UI_ace_editor")),
       fluidRow(column(actionButton("run_sql", "Run"),width = 6),column(uiOutput("ui_dl_sql_tab"),width = 6)),
       tags$br(),
       DT::dataTableOutput("sql_dt",height = NULL),
       width=9
     )
    )
  )
)