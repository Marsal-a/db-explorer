library(shiny)
library(bslib)


ui <- fluidPage(

    # Application title
    titlePanel("Explore db"),

    sidebarLayout(
        sidebarPanel(
            selectInput("db","Selection de la base :",choices = db_choices),
            uiOutput("ui_schemas"),
            uiOutput("ui_tables"),
            width=3
        ),
        mainPanel()
    )
)

