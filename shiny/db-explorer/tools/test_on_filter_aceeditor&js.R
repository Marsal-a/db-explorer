


output$ui_filters <- renderUI({
  req(input$selected_table)
  wellPanel(
    checkboxInput("filterByClick", "Cliquer pour filtrer?", value = F),
    checkboxInput("cumulateFilters", "Accumuler filtres?", value = F),
    br(),
    fluidRow(
      column(width = 10,actionLink("clearFilters", "Clear filters", icon = icon("sync", verify_fa = FALSE), style = "color:black")),
      column(width = 2,actionLink("help_filter", "", icon = icon("question-circle", verify_fa = FALSE), style = "color:#4b8a8c"))
    ),
    returnTextAreaInput("data_filter",
                        label = "Data filter:",
                        value = "",
                        rows=2,
                        placeholder = "Ecrire une condition de filtre et appuyer sur Entréea"
    ),
    shiny::textAreaInput("data_filter2",
                         label = "Data filter:",
                         value = "",
                         rows=2,
                         placeholder = "Ecrire une condition de filtre et appuyer sur Entréeb"
    ),
    textAreaInput("data_filter3",
                  label = "Data filter:",
                  value = "",
                  rows=2,
                  placeholder = "Ecrire une condition de filtre et appuyer sur Entréeb"
    ),
    p("ACE"),
    aceEditor(outputId = "data_filter3",mode="r",value = "ACE",height = 10,showLineNumbers = FALSE),
    
    uiOutput("ui_filter_error")
  )
})