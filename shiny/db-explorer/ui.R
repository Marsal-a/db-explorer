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
  
  title = "Exploration des bases de données",
  
  # includeCSS("www/dbexplorer-style.css"),
  # includeCSS("www/style.css"),
  tabPanel("Navigation",
     shinyjs::useShinyjs(),
     tags$head(
       tags$link(rel = "stylesheet", type = "text/css", href = "dbexplorer-style.css")
       # tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
     ),
     # tags$script(HTML("var header = $('.navbar > .container-fluid');
     # header.append('<div style=\"float:right\"><a href=\"URL\"><img src=\"Logo-SSM_Justice.png\" alt=\"alt\" style=\"float:right;width:90px;height:50px;padding-top:0px;\"> </a></div>');
     # console.log(header)")
     # ),
     tags$script(
       "$(function() {
      console.log(Shiny);
    });
    "
    ),
    includeScript("www/js/refocus_cursor.js"),
    includeScript("www/js/reset_colOrder.js"),
    includeScript("www/js/returnTextAreaBinding.js"),
    includeScript("www/js/returnTextInputBinding.js"),
    includeScript("www/js/run_return.js"),
    includeScript("www/js/enter_password.js"),
    tags$script(
      "$(document).on('shiny:inputchanged', function(event) {
          if (event.name != 'changed') {
            Shiny.setInputValue('changed', event.name);
          }
        });"
    ),

    sidebarLayout(
      sidebarPanel(
        wellPanel(
          selectInput("db","Sélection de la base :", choices = c("Select database to explore"="",db_choices),selected=default_db),
          uiOutput("ui_schemas"),
          uiOutput("ui_tables")
        ),
        uiOutput("ui_filters"),
        uiOutput("ui_arrange"),
        uiOutput("ui_filter_error"),
        uiOutput("ui_view_vars"),
        actionButton("trigtest", "button_test", icon = icon("sync", verify_fa = FALSE), style = "color:black"),
        width = 3
      ),
      mainPanel(
        fluidRow(htmlOutput("ui_summary")),
        fluidRow(uiOutput("ui_dl_view_tab")),
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
       p("Console SQL en construction"),
       fluidRow(uiOutput("UI_ace_editor")),
       fluidRow(column(actionButton("run_sql", "Run"),width = 6),column(uiOutput("ui_dl_sql_tab"),width = 6)),
       tags$br(),
       DT::dataTableOutput("sql_dt",height = NULL),
       width=9
     )
    )
  ),
  tabPanel("DEV_",
    sidebarLayout(
    
      sidebarPanel(
        wellPanel(
          selectInput("navig_db","Sélection de la base :", choices = c("Select database to explore"="",db_choices),selected=default_db),
          uiOutput("ui_navig_schemas"),
          uiOutput("ui_navig_tables")
        ),
        uiOutput("ui_navig_filters"),
        uiOutput("ui_navig_arrange"),
        uiOutput("ui_navig_filter_error"),
        uiOutput("ui_navig_view_vars"),
        actionButton("trigtest_DEV", "button_test", icon = icon("sync", verify_fa = FALSE), style = "color:black"),
        width = 3
      ),
      mainPanel(
        fluidRow(htmlOutput("ui_navig_summary")),
        fluidRow(uiOutput("ui_navig_dl_view_tab")),
        DT::dataTableOutput("ui_navig_dataviewer",height = NULL), # le height = NULL permet de laisser la taille ajusté par CSS 
        width = 9
      )
    
    )
  ),
  # tabPanel("DEV_DYNAMIC_1",
  #          sidebarLayout(
  #            sidebarPanel(
  #              wellPanel(
  #                uiOutput(paste0("ui_navig_db","_","tab1")),
  #                uiOutput(paste0("ui_navig_schemas","_","tab1")),
  #                uiOutput(paste0("ui_navig_tables","_","tab1")),
  #                actionButton("trigtest_dynamic_1", "button_test", icon = icon("sync", verify_fa = FALSE), style = "color:black"),
  #              ),
  #              width = 3
  #            ),
  #            mainPanel(
  #              DT::dataTableOutput(paste0("navig_dataviewer","_","tab1"),height = NULL)
  #            )
  #          )
  # ),
  tabPanel("DEV_DYNAMIC_1",
    viewTabUi("tab1")
  ),
  tabPanel("DEV_DYNAMIC_2",
    viewTabUi("tab2")
  )
)