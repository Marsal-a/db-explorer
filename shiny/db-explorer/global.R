options(
  path_db_explorer = ifelse(grepl("/shiny/db-explorer", getwd()),"../../",".")
)


# source(paste0(getOption("path_db_explorer"),"/R/create_sqlite_db.R"))

db_choices=c("SQLite","Netezza","PostgreSQL - Prod","Oracle - Prod")



default_db     <- ifelse(exists("dbexplorer_dbhost"), dbexplorer_dbhost, "")
default_schema <- ifelse(exists("dbexplorer_schema"), dbexplorer_schema, "")
default_table  <- ifelse(exists("dbexplorer_table"), dbexplorer_table, "")


list_schema_SQLite<- function(con){
  return("Default")
}

list_table_SQLite <- function(con){
  "SELECT name FROM sqlite_master WHERE type='table'"
  tables <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table'")  
  return(tables)
}

get_class <- function(dat) {
  sapply(dat, function(x) class(x)[1]) %>%
    sub("ordered", "factor", .) %>%
    sub("POSIXct", "date", .) %>%
    sub("POSIXlt", "date", .) %>%
    sub("Date", "date", .) %>%
    sub("Period", "period", .)
}



fix_smart <- function(text, all = FALSE) {
  if (all) {
    ## to remove all non-ascii symbols use ...
    text <- stringi::stri_trans_general(text, "latin-ascii")
  } else {
    ## based on https://stackoverflow.com/a/1262210/1974918
    ## based on https://stackoverflow.com/a/54467895/1974918
    text <- gsub("\u2022", "*", text) %>%
      gsub("\u2026", "...", .) %>%
      gsub("\u2013", "-", .) %>%
      gsub("\u2019", "'", .) %>%
      gsub("\u2018", "'", .) %>%
      gsub("\u201D", '"', .) %>%
      gsub("\u201C", '"', .)
  }
  gsub("\r\n", "\n", text) %>%
    gsub("\r", "\n", .) %>%
    gsub("\f", "\n", .)
}

## check if a variable is null or not in the selected data.frame
not_available <- function(x) any(is.null(x)) || (sum(x %in% varnames()) < length(x))

## check if a variable is null or not in the selected data.frame
available <- function(x) !not_available(x)

is.empty <- function(x, empty = "\\s*") {
  # any should not be needed here but patchwork objects can have length == 1
  # and yet still return a vector of logicals
  is_not(x) || (length(x) == 1 && any(grepl(paste0("^", empty, "$"), x)))
}

is_not <- function(x) {
  # any should not be needed here but patchwork objects can have length == 1
  # and yet still return a vector of logicals
  length(x) == 0 || (length(x) == 1 && any(is.na(x)))
}


`%AND%` <- function(x, y) {
  if (!all(is.null(x)) && !all(is.na(x))) {
    if (!all(is.null(y)) && !all(is.na(y))) {
      return(y)
    }
  }
  return(NULL)
}

textAreaInput <- function(inputId, label, value = "", width = NULL,
                          height = NULL, cols = NULL, rows = NULL,
                          placeholder = NULL, resize = NULL,
                          autocomplete = "off", autocorrect = "off",
                          autocapitalize = "off", spellcheck = "true",
                          ...) {
  value <- restoreInput(id = inputId, default = value)
  if (!is.null(resize)) {
    resize <- match.arg(
      resize,
      c("both", "none", "vertical", "horizontal")
    )
  }
  style <- paste(if (!is.null(width)) {
    paste0("width: ", validateCssUnit(width), ";")
  }, if (!is.null(height)) {
    paste0("height: ", validateCssUnit(height), ";")
  }, if (!is.null(resize)) {
    paste0("resize: ", resize, ";")
  })
  if (length(style) == 0) {
    style <- NULL
  }
  div(
    class = "form-group shiny-input-container",
    label %AND% tags$label(label, `for` = inputId),
    tags$textarea(
      id = inputId,
      class = "form-control",
      placeholder = placeholder,
      style = style,
      rows = rows,
      cols = cols,
      autocomplete = autocomplete,
      autocorrect = autocorrect,
      autocapitalize = autocapitalize,
      spellcheck = spellcheck,
      ...,
      value
    )
  )
}

returnTextAreaInput <- function(inputId, label = NULL, rows = 2,
                                placeholder = NULL, resize = "vertical",
                                value = "") {
  ## avoid all sorts of 'helpful' behavior from your browser
  ## see https://stackoverflow.com/a/35514029/1974918
  tagList(
    tags$div(
      # using containing element based on
      # https://github.com/niklasvh/html2canvas/issues/2008#issuecomment-1445503369
      tags$label(label, `for` = inputId), br(),
      tags$textarea(
        value,
        id = inputId,
        type = "text",
        rows = rows,
        placeholder = placeholder,
        resize = resize,
        autocomplete = "off",
        autocorrect = "off",
        autocapitalize = "off",
        spellcheck = "false",
        class = "returnTextArea form-control"
      )
    )
  )
}


