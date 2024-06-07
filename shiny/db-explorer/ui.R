library(shiny)
library(bslib)


ui <- fluidPage(

    # Application title
    titlePanel("Explore db"),

    sidebarLayout(
        sidebarPanel(
            tags$table(
                tags$td(textInput("db0", "Selection de la base :")),
                tags$td(actionButton("ok", "Store", icon = icon("plus", verify_fa = FALSE), class = "btn-success"), class = "top")
            ),
            selectInput("db","Selection de la base :",choices = db_choices,selected = NULL),
            uiOutput("ui_schemas"),
            uiOutput("ui_tables"),
            width=3
        ),
        mainPanel()
    )
)

