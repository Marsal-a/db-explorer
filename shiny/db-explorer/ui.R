library(shiny)
library(bslib)


ui <- fluidPage(
    
    includeCSS("www/style.css"),
    includeScript("www/js/returnTextAreaBinding.js"),
    includeScript("www/js/returnTextInputBinding.js"),
    includeScript("www/js/run_return.js"),
    # includeScript("www/js/returnTextAreaBinding.js"),
    # includeScript("www/js/returnTextAreaBinding.js"),
    
    titlePanel("Explore db"),
    sidebarLayout(
        sidebarPanel(
            wellPanel(
                selectInput("db","Sélection de la base :", choices = c("Select database to explore"="",db_choices),selected=default_db),
                uiOutput("ui_schemas"),
                uiOutput("ui_tables"),
                width=3
            ),
            wellPanel(
                checkboxInput("filterByClick", "Cliquer pour filtrer?", value = F),
                checkboxInput("cumulateFilters", "Accumuler filtres?", value = F),
                br(),
                actionLink("clearFilters", "Clear filters", icon = icon("sync", verify_fa = FALSE), style = "color:black"),
                returnTextAreaInput("data_filter",
                                    label = "Data filter:",
                                    value = "",
                                    placeholder = "Provide a filter (e.g., price >  5000) and press return"
                )
            ),
            wellPanel(
                uiOutput("ui_view_vars")
            )
            
        ),
        mainPanel(
            tabsetPanel(
                # tabPanel("Table",tableOutput('table')),
                tabPanel("Table2",DT::dataTableOutput("dataviewer"))
            )
        )
    )
)

