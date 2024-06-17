library(shiny)
library(DT)
callback <- c(
  "var tbl = $(table.table().node());",
  "var id = tbl.closest('.datatables').attr('id');",
  "table.on('autoFill', function(e, datatable, cells){",
  "  var out = [];",
  "  for(var i = 0; i < cells.length; ++i){",
  "    var cells_i = cells[i];",
  "    for(var j = 0; j < cells_i.length; ++j){",
  "      var c = cells_i[j];",
  "      var value = c.set === null ? '' : c.set;", # null => problem in R
  "      out.push({",
  "        row: c.index.row + 1,",
  "        col: c.index.column,",
  "        value: value",
  "      });",
  # to color the autofilled cells, uncomment the two lines below  
  #  "      $(table.cell(c.index.row, c.index.column).node())",
  #  "        .css('background-color', 'yellow');",
  "    }",
  "  }",
  "  Shiny.setInputValue(id + '_cells_filled:DT.cellInfo', out);",
  "  table.rows().invalidate();", # this updates the column type
  "});"
)
ui <- fluidPage(
  br(),
  DTOutput("dt"),
  br(),
  verbatimTextOutput("table")
)
server <- function(input, output){
  
  dat <- iris[1:5,]
  dat$Species <- as.character(dat$Species)
  
  output[["dt"]] <- renderDT({
    datatable(dat, 
              editable = list(target = "cell"),
              selection = "none",
              extensions = "AutoFill",
              callback = JS(callback), 
              options = list(
                autoFill = TRUE
              )
    )
  }, server = TRUE)
  
  Data <- reactive({
    info <- rbind(input[["dt_cells_filled"]], input[["dt_cell_edit"]])
    if(!is.null(info)){
      info <- unique(info)
      info$value[info$value==""] <- NA
      dat <<- editData(dat, info, proxy = "dt")
    }
    dat
  })
  output[["table"]] <- renderPrint({Data()})  
}
shinyApp(ui, server)