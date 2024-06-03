js <- '
$(document).on("keyup", function(e) {
  if(e.keyCode == 13){
    Shiny.onInputChange("keyPressed", Math.random());
  }
});
'

shinyApp(
  ui = bootstrapPage(
    
    tags$script(js),
    
    textInput("title", label = "Title"),
    
    plotOutput("ggplot")
  ),
  
  server = function(input, output, session){
    
    Title <- reactiveVal()
    
    observeEvent(input[["keyPressed"]], {
      Title(input[["title"]])
    })
    
    output[["ggplot"]] <- renderPlot({
      ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) +
        geom_point() +
        ggtitle(Title())
    })
    
  }
)