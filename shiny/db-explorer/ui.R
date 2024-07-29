.libPaths(c( "~/R_Commun/Adam/custom_lib/db-explorer/",.libPaths()))
# .libPaths(.libPaths()[1])
unloadNamespace("nzsdse")
unloadNamespace("dbplyr")
unloadNamespace("dplyr")
unloadNamespace("readr")
unloadNamespace("tibble")
unloadNamespace("pillar")
unloadNamespace("tidyselect")
unloadNamespace("pgsdse")
unloadNamespace("orasdse")
unloadNamespace("RJDBC")
unloadNamespace("crayon")


unloadNamespace("RPostgres")
unloadNamespace("ROracle")
unloadNamespace("DBI")
unloadNamespace("stringr")
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
  
  title = "Exploration des bases de données",
  
  # includeCSS("www/dbexplorer-style.css"),
  # includeCSS("www/style.css"),
  tabPanel("Navigation",
     shinyjs::useShinyjs(),
     tags$head(
       tags$link(rel = "stylesheet", type = "text/css", href = "dbexplorer-style.css")
     ),
    includeScript("www/js/refocus_cursor.js"),
    includeScript("www/js/returnTextAreaBinding.js"),
    includeScript("www/js/returnTextInputBinding.js"),
    includeScript("www/js/run_return.js"),
    includeScript("www/js/enter_password.js"),
    
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          selectInput("db","Sélection de la base :", choices = c("Select database to explore"="",db_choices),selected=default_db),
          uiOutput("ui_schemas"),
          uiOutput("ui_tables")
        ),
        # uiOutput("ui_filters"),
        wellPanel(
          checkboxInput("filterByClick", "Cliquer pour filtrer?", value = F),
          checkboxInput("cumulateFilters", "Accumuler filtres?", value = F),
          br(),
          fluidRow(
            column(width = 10,actionLink("clearFilters", "Clear filters", icon = icon("sync", verify_fa = FALSE), style = "color:black")),
            column(width = 2,actionLink("help_filter", "", icon = icon("question-circle", verify_fa = FALSE), style = "color:#4b8a8c"))
          ),
          returnTextAreaInput("data_filter",
            label = "Data filter:",
            value = "",
            rows=2,
            placeholder = "Ecrire une condition de filtre et appuyer sur Entrée"
          ),
          uiOutput("ui_filter_error")
        ),
        uiOutput("ui_view_vars"),
        actionButton("trigtest", "button_test", icon = icon("sync", verify_fa = FALSE), style = "color:black"),
        width = 3
      ),
      mainPanel(
        htmlOutput("ui_summary"),
        br(),
        DT::dataTableOutput("dataviewer",height = NULL), # le height = NULL permet de laisser la taille ajusté par CSS 
        width = 9
      )
    )
  ),
  tabPanel("Console SQL",
           sidebarLayout(
             sidebarPanel(
               wellPanel(
                selectInput("db_sql","Sélection de la base :", choices = c("Select database to explore"="",db_choices),selected=default_db),
                uiOutput("ui_schemas_sql_panel"),
                uiOutput("ui_tables_sql_panel")
               ),
               width=3
             ),
             mainPanel(
               # fluidRow(aceEditor("sql_code", mode = "sql", height = "100px", value = "SELECT * FROM ...")),
               p("Console SQL en construction"),
               p("Plante si clique trop rapide sur RUN aprés selection de la table: attendre 1 sec"),
               p("Aprés un premier run, plante lors du changement de schéma"),
               fluidRow(uiOutput("UI_ace_editor")),
               fluidRow(actionButton("run_sql", "Run")),
               tags$br(),
               fluidRow(DT::dataTableOutput("sql_dt")),
               width=9
             ),
             
           )
    )
)