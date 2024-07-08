.libPaths(c( "~/R_Commun/Adam/custom_lib/db-explorer/",.libPaths()))
unloadNamespace("nzsdse")
unloadNamespace("dbplyr")
unloadNamespace("dplyr")
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


ui <- fluidPage(
    
    includeCSS("www/style.css"),
    
    includeScript("www/js/returnTextAreaBinding.js"),
    includeScript("www/js/returnTextInputBinding.js"),
    includeScript("www/js/run_return.js"),
    tags$style(HTML("
                  .shiny-output-error {
                    color: red;
                  }
                  ")),
    
    titlePanel("Explore db"),
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
                    column(width = 2,actionLink("help_filter", "", icon = icon("fa fa-question-circle", verify_fa = FALSE), style = "color:black"))
                ),
                # actionLink("clearFilters", "Clear filters", icon = icon("sync", verify_fa = FALSE), style = "color:black"),
                # bsModal("modalExample", "fancy report", "help_filter", size = "large",
                #         uiOutput("ui_filter_help")
                # ),
                returnTextAreaInput("data_filter",
                                    label = "Data filter:",
                                    value = "",
                                    rows=2,
                                    placeholder = "Ecrire une condition de filtre et appuyer sur Entrée"
                                    # placeholder = "Ecrire une condition de filtre et appuyer sur Entrée. Exemple :\nPER_ID==\"123\"\nou\nPER_ID %in% c(\"1\",\"2\",\"3\")\n"

                ),
                uiOutput("ui_filter_error")
            ),
            uiOutput("ui_view_vars"),
            actionButton("trigtest", "button_test", icon = icon("sync", verify_fa = FALSE), style = "color:black")
            
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Table2",
                         htmlOutput("ui_summary"),
                         DT::dataTableOutput("dataviewer")
                         ),
                tabPanel("Table3",
                         aceEditor("sql_code", mode = "sql", height = "100px", value = "SELECT * FROM ..."),
                         actionButton("run_sql", "Run"),
                         DT::dataTableOutput("sql_dt")
                         )
            )
        )
    )
)

