.libPaths(c( "~/R_Commun/Adam/custom_lib/db-explorer/",.libPaths()))
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


unloadNamespace("RPostgres")
unloadNamespace("ROracle")
unloadNamespace("DBI")
unloadNamespace("stringr")
unloadNamespace("glue")

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


ui <- navbarPage(title = "database-explorer",
                 tabPanel("Navigation",
    
    
    includeCSS("www/style.css"),
    
    tags$script('
      Shiny.addCustomMessageHandler("refocus",
            function(e_id) {
            document.getElementById(e_id).focus();
                                  });'),
    
    includeScript("www/js/returnTextAreaBinding.js"),
    includeScript("www/js/returnTextInputBinding.js"),
    includeScript("www/js/run_return.js"),
    tags$style(HTML("
                  .shiny-output-error {
                    color: red;
                  }
                  ")),
    
    # titlePanel("Explore db"),
    sidebarLayout(
        sidebarPanel(
            wellPanel(
                selectInput("db","Sélection de la base :", choices = c("Select database to explore"="",db_choices),selected=default_db),
                uiOutput("ui_schemas"),
                uiOutput("ui_tables"),
                width=3
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
            actionButton("trigtest", "button_test", icon = icon("sync", verify_fa = FALSE), style = "color:black")
            
        ),
        mainPanel(
            htmlOutput("ui_summary"),
            br(),
            DT::dataTableOutput("dataviewer")
        )
        # mainPanel(
        #     tabsetPanel(
        #         tabPanel("Table2",
        #                  htmlOutput("ui_summary"),
        #                  br(),
        #                  DT::dataTableOutput("dataviewer")
        #                  ),
        #         tabPanel("Table3",
        #                  aceEditor("sql_code", mode = "sql", height = "100px", value = "SELECT * FROM ..."),
        #                  actionButton("run_sql", "Run"),
        #                  DT::dataTableOutput("sql_dt")
        #                  )
        #     )
        # )
    )
)
)
