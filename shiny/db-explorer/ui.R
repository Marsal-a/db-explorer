library(bslib)
library(shiny)
library(dplyr)
library(dbplyr)
library(purrr)
library(rlang)
library(shinyAce)


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

                # actionButton("clearFilters", "Clear filters", icon = icon("sync", verify_fa = FALSE), style = "color:black"),
                actionLink("clearFilters", "Clear filters", icon = icon("sync", verify_fa = FALSE), style = "color:black"),
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
                tabPanel("Table2",DT::dataTableOutput("dataviewer")),
                tabPanel("Table3",
                         aceEditor("sql_code", mode = "sql", height = "100px", value = "SELECT * FROM ..."),
                         actionButton("run_sql", "Run"),
                         DT::dataTableOutput("sql_dt")
                         )
            )
        )
    )
)

