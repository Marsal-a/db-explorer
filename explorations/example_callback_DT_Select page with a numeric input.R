library(shiny)
library(DT)
shinyApp(
  ui = fluidPage(
    tags$head(tags$style(".pagination {float: right;}")),
    fluidRow(
      div(id="pagination", 
          div(style = "display:inline-block;", 
              tags$a(id = "first", style = "cursor: pointer;", "First")),
          div(style = "display:inline-block;", 
              tags$a(id = "previous", style = "cursor: pointer;", " Previous")),
          div(style = "display:inline-block;", 
              tags$input(id="page", type="number", class="input-sm", value="1", min="1")
          ),
          div(style = "display:inline-block;", 
              tags$span(id = "of")),
          div(style = "display:inline-block;", 
              tags$a(id = "next", style = "cursor: pointer;", "Next ")),
          div(style = "display:inline-block;", 
              tags$a(id = "last", style = "cursor: pointer;", "Last"))
      )
    ),
    fluidRow(
      column(12, DTOutput('tbl'))
    )
  ),
  server = function(input, output) {
    output$tbl = renderDT({
      datatable(
        iris, options = list(
          dom = "lfrti<'pagination'>", 
          initComplete = JS(c(
            "function(settings, json){",
            "  var table = settings.oInstance.api();",
            "  var pageinfo = table.page.info();",
            "  $('#of').text('of ' + pageinfo.pages);",
            "}"
          ))
        ),
        callback = JS(c(
          "$('div.pagination').append($('#pagination'));",
          "$('#first').on('click', function(){", 
          "  table.page('first').draw('page');",
          "  $('#page').val(1);",
          "});",
          "$('#previous').on('click', function(){", 
          "  table.page('previous').draw('page');",
          "  $('#page').val(table.page.info().page + 1);",
          "});",
          "$('#next').on('click', function(){", 
          "  table.page('next').draw('page');",
          "  $('#page').val(table.page.info().page + 1);",
          "});",
          "$('#last').on('click', function(){", 
          "  table.page('last').draw('page');",
          "  $('#page').val(table.page.info().pages);",
          "});",
          "$('#page').on('change', function(){",
          "  var page = parseInt($('#page').val());",
          "  if(!isNaN(page)){ table.page(page-1).draw('page'); }",
          "});"
        ))
      )
    })
  }
)