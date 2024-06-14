"https://www.r-bloggers.com/2019/06/useful-callbacks-for-dt-in-shiny/"

library(shiny)
library(DT)
callback <- c(
  "var dt = table.table().node();",
  "$(dt).selectable({",
  "  distance : 10,",
  "  selecting: function(evt, ui){",
  "    $(this).find('tbody tr').each(function(i){",
  "      if($(this).hasClass('ui-selecting')){",
  "        table.row(i).select();",
  "      }",
  "    });",
  "  }",
  "}).on('dblclick', function(){table.rows().deselect();});"
)
ui <- fluidPage(
  DTOutput("dt")
)
server <- function(input, output){
  output[["dt"]] <- renderDT({
    dtable <- datatable(
      iris, extensions = "Select", 
      callback = JS(callback), selection = "multiple"
    )
    dep <- htmltools::htmlDependency("jqueryui", "1.12.1",
                                     "www/shared/jqueryui",
                                     script = "jquery-ui.min.js",
                                     package = "shiny")
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
  })
}
shinyApp(ui, server)