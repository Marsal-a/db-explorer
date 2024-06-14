"https://www.r-bloggers.com/2019/06/useful-callbacks-for-dt-in-shiny/"

library(shiny)
library(DT)
js <- c(
  "table.on('key', function(e, datatable, key, cell, originalEvent){",
  "  var targetName = originalEvent.target.localName;",
  "  if(key == 13 && targetName == 'body'){",
  "    $(cell.node()).trigger('dblclick.dt');",
  "  }",
  "});",
  "table.on('keydown', function(e){",
  "  var keys = [9,13,37,38,39,40];",
  "  if(e.target.localName == 'input' && keys.indexOf(e.keyCode) > -1){",
  "    $(e.target).trigger('blur');",
  "  }",
  "});",
  "table.on('key-focus', function(e, datatable, cell, originalEvent){",
  "  var targetName = originalEvent.target.localName;",
  "  var type = originalEvent.type;",
  "  if(type == 'keydown' && targetName == 'input'){",
  "    if([9,37,38,39,40].indexOf(originalEvent.keyCode) > -1){",
  "      $(cell.node()).trigger('dblclick.dt');",
  "    }",
  "  }",
  "});"
)
ui <- fluidPage(
  DTOutput("table")
)
server <- function(input, output, session){
  output[["table"]] <- renderDT({
    datatable(
      iris,
      selection = "none",
      editable = TRUE, 
      callback = JS(js),
      extensions = "KeyTable",
      options = list(
        keys = TRUE
      )
    )
  })
}
shinyApp(ui, server)