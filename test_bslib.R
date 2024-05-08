library(bslib)
# Shiny example
ui <- page_sidebar(
  title = "My app"
)

server <- function(input, output) {
  bs_themer()
  
}



shinyApp(ui, server)