"https://www.r-bloggers.com/2019/06/useful-callbacks-for-dt-in-shiny/#google_vignette"
library(shiny)
library(DT)
rowNames <- TRUE # whether to show row names in the table
colIndex <- as.integer(rowNames)
callback <- c(
  sprintf("table.on('click', 'td:nth-child(%d)', function(){", colIndex+1),
  "  var td = this;",
  "  var cell = table.cell(td);",
  "  if(cell.data() === 'ok'){",
  "    cell.data('remove');",
  "  } else {",
  "    cell.data('ok');",
  "  }",
  "  var $row = $(td).closest('tr');",
  "  $row.toggleClass('excluded');",
  "  var excludedRows = [];",
  "  table.$('tr').each(function(i, row){",
  "    if($(this).hasClass('excluded')){",
  "      excludedRows.push(parseInt($(row).attr('id').split('_')[1]));",
  "    }",
  "  });",
  "  Shiny.setInputValue('excludedRows', excludedRows);",
  "})"
)
restore <- c(
  "function(e, table, node, config) {",
  "  table.$('tr').removeClass('excluded').each(function(){",
  sprintf("    var td = $(this).find('td').eq(%d)[0];", colIndex), 
  "    var cell = table.cell(td);", 
  "    cell.data('ok');",
  "  });",
  "  Shiny.setInputValue('excludedRows', null);",
  "}"
)
render <- c(
  'function(data, type, row, meta){',
  '  if(type === "display"){',
  '    var color = data === "ok" ? "forestgreen" : "red";',
  '    return "<span style=\\\"color:" + color +',
  '           "; font-size:18px\\\"><i class=\\\"glyphicon glyphicon-" +', 
  '           data + "\\\"></i></span>";',
  '  } else {',
  '    return data;',
  '  }',
  '}'
)
ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      ".excluded { color: rgb(211,211,211); font-style: italic; }"
    ))
  ),
  fluidRow(
    column(
      6, 
      tags$label("Excluded rows"),
      verbatimTextOutput("excludedRows")
    ),
    column(
      6, 
      tags$label("Included rows"),
      verbatimTextOutput("includedRows")
    )
  ),
  br(),
  DTOutput("mytable")
)
server <- function(input, output,session) {
  
  dat <- cbind(Selected = "ok", mtcars[1:6,], id = paste0("row_",1:6))
  
  output[["mytable"]] <- renderDT({
    datatable(dat, rownames = rowNames, 
              extensions = c("Select", "Buttons"), 
              selection = "none", 
              callback = JS(callback),
              options = list(
                rowId = JS(sprintf("function(data){return data[%d];}", 
                                   ncol(dat)-1+colIndex)), 
                columnDefs = list(
                  list(visible = FALSE, targets = ncol(dat)-1+colIndex),
                  list(className = "dt-center", targets = "_all"),
                  list(className = "notselectable", targets = colIndex),
                  list(targets = colIndex, render = JS(render)) 
                ),
                dom = "Bt",
                buttons = list("copy", "csv",
                               list(
                                 extend = "collection",
                                 text = 'Select all rows', 
                                 action = JS(restore)
                               )
                ),
                select = list(style = "single", 
                              selector = "td:not(.notselectable)")
              )
    )
  }, server = FALSE)
  
  output$excludedRows <- renderPrint({
    input[["excludedRows"]]
  })
  output$includedRows <- renderPrint({
    setdiff(1:nrow(dat), input[["excludedRows"]])
  })
  
}
shinyApp(ui, server)