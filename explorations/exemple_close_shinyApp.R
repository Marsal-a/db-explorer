library(shiny)
runApp(list(
  ui = bootstrapPage(
    tags$button(
      id = 'close',
      type = "button",
      class = "btn action-button",
      onclick = "setTimeout(function(){window.close();},500);",  # close browser
      "Close window"
    )
  ),
  server = function(input, output) {
    observe({
      if (input$close > 0) stopApp()                             # stop shiny
    })
  }
))