library(shiny)
library(DT)
callback <- c(
  "$.contextMenu({",
  "  selector: '#table th',", 
  "  trigger: 'right',",
  "  autoHide: true,",
  "  items: {",
  "    text: {",
  "      name: 'Enter column header:',", 
  "      type: 'text',", 
  "      value: ''", 
  "    }",
  "  },",
  "  events: {",
  "    show: function(opts){",
  "      $.contextMenu.setInputValues(opts, {text: opts.$trigger.text()});",
  "    },",
  "    hide: function(opts){",
  "      var $this = this;",
  "      var data = $.contextMenu.getInputValues(opts, $this.data());",
  "      var $th = opts.$trigger;",
  "      $th.text(data.text);",
  "    }",
  "  }",
  "});" 
)
ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet", 
      href = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.css"
    ),
    tags$script(
      src = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.js"
    )
  ),
  DTOutput("table")
)
server <- function(input, output){
  output[["table"]] <- renderDT({
    datatable(iris, callback = JS(callback))
  }, server = FALSE)  
}
shinyApp(ui, server)