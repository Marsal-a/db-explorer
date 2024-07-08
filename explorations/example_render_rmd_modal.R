library(shinyBS)

report_file_md="/data/samba/adam.marsal/R/Travaux sser/comites-operationnels-r.wiki/Comité-du-19.06.2024.md"
report_file_html="/data/samba/adam.marsal/R/Travaux sser/comites-operationnels-r.wiki/Comité-du-19.06.2024.html"

ui <- function(id) {
  
  fluidPage(
    fluidRow(
      column(3, 
             actionButton("button", "Go!")
      )
    ),
    fluidRow(
      column(10,
             bsModal("modalExample", "fancy report", "button", size = "large",
                     uiOutput("report_ui")
             )
      )
    )
  )
}

server <- function(input, output, session) {
  
  output$report_ui <- renderUI({
    
    # HTML(
    #   readLines(
    #     rmarkdown::render(report_file_md,
    #                       encoding = "UTF-8",
    #                       envir = new.env()
    #     ), encoding = "UTF-8"
    #   )
    # )
    
    # HTML(readLines(report_file_html))
    
    
  })
  
}
shinyApp(ui = ui, server = server)