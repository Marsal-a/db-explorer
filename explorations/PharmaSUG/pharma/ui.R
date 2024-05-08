library(shiny)
library(ggplot2) # for the dataset
library("safetyGraphics")
library(shinythemes)


ui <- fluidPage(theme = shinytheme("united"),
                title = "Examples of DataTables",
                sidebarPanel(
                    fileInput('file1',
                              'Select your file',
                              accept=c(
                                  'text/csv',
                                  'text/comma-separated-values','.csv',
                                  '.sas7bdat'
                                )

                    ),

                    selectInput("varlist", "Columns to show:", choices=c(colnames(data())), multiple = TRUE)
                ),
                mainPanel(
                    tabsetPanel(
                        id = 'dataset',
                        tabPanel ("Data", DT:: dataTableOutput("mytable1"))
                    )
                )
)
