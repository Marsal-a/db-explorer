library(shiny)

# Define UI
ui <- fluidPage(
  navbarPage(
    "A-Title",
    tabPanel("A1",
             sidebarPanel(),
             mainPanel(
               tabsetPanel(type = "tabs",
                           id = "panels",
                           # Tabsets  in mainPanel----
                           tabPanel("B",
                                    hr(),
                                    actionLink("link_to_D", "Go to D")
                           ), # Tabpanel
                           tabPanel("C")
               )# tabsetPanel
             )# mainPanel
    ),#tabPanel
    
    navbarMenu("About",
               tabPanel("D", "I am D"),
               tabPanel("E", "I am E")
    ), #NavbarMenu
    id="MainNavBar" # Step 1: Give the navbarPage an id
  ) # NavbarPage
  
) # fluidPage


# Define server
server <- function(input, output,session) {
  
  # Link to D
  observeEvent(input$link_to_D, {
    #--updateTabsetPanel(session, "C", "D")
    # update selected tab to "D"
    updateNavbarPage(inputId="MainNavBar", selected = "D")
  })
  
  
  
}  # server


# Run the application
shinyApp(ui = ui, server = server)