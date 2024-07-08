### Fonction utilitaires pour l'application : 
### La plupart des fonctions sont issues ou inspirées du package Radiant : 
### https://github.com/radiant-rstats

# options(
#   path_db_explorer = ifelse(grepl("/shiny/db-explorer", getwd()),"../../","./")
# )


## Si l'application est lancé depuis le projet Rstudio : on veut path_db_explorer = "./"
## Si l'application est lancé par un job rstudio, stocké sur R_commun (temporaire pour les tests beta) : on veut path_db_explorer = "./"

options(
  path_db_explorer = ifelse(grepl("/shiny/db-explorer|R_Commun/Adam", getwd()),"./","./shiny/db-explorer/")
)

options(shiny.reactlog = TRUE)


# print(getwd())
# print(usethis::proj_get())
# print(paste0(getOption("path_db_explorer"),"init.R"))
source(paste0(getOption("path_db_explorer"),"init.R"))

### Issu du package radiant
### > Permet d'ajouter le type de variable à la liste des variables 
get_class <- function(dat) {
  sapply(dat, function(x) class(x)[1]) %>%
    sub("ordered", "factor", .) %>%
    sub("POSIXct", "date", .) %>%
    sub("POSIXlt", "date", .) %>%
    sub("Date", "date", .) %>%
    sub("Period", "period", .)
}


### Issu du package radiant
### permet de corriger certains caractères spéciaux dans la création de la condition de filtre
### Ainsi que de traiter les retour à la ligne  
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


### Issu du package radiant
## check if a variable is null or not in the selected data.frame
not_available <- function(x) any(is.null(x)) || (sum(x %in% varnames()) < length(x))

### Issu du package radiant
## check if a variable is null or not in the selected data.frame
available <- function(x) !not_available(x)

### Issu du package radiant
is.empty <- function(x, empty = "\\s*") {
  # any should not be needed here but patchwork objects can have length == 1
  # and yet still return a vector of logicals
  is_not(x) || (length(x) == 1 && any(grepl(paste0("^", empty, "$"), x)))
}

### Issu du package radiant
is_not <- function(x) {
  # any should not be needed here but patchwork objects can have length == 1
  # and yet still return a vector of logicals
  length(x) == 0 || (length(x) == 1 && any(is.na(x)))
}

### Issu du package radiant
`%AND%` <- function(x, y) {
  if (!all(is.null(x)) && !all(is.na(x))) {
    if (!all(is.null(y)) && !all(is.na(y))) {
      return(y)
    }
  }
  return(NULL)
}

### Issu du package radiant
### Propose un textAreaInput surchageant celui de base de shiny afin d'éliminer certains comportement navigateurs

################################################################
## functions used to create Shiny in and outputs
################################################################

## using a custom version of textInput to avoid browser "smartness"
# textInput <- function(inputId, label, value = "", width = NULL,
#                       placeholder = NULL, autocomplete = "off",
#                       autocorrect = "off", autocapitalize = "off",
#                       spellcheck = "false", ...) {
#   value <- restoreInput(id = inputId, default = value)
#   div(
#     class = "form-group shiny-input-container",
#     style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
#     label %AND% tags$label(label, `for` = inputId),
#     tags$input(
#       id = inputId,
#       type = "text",
#       class = "form-control",
#       value = value,
#       placeholder = placeholder,
#       autocomplete = autocomplete,
#       autocorrect = autocorrect,
#       autocapitalize = autocapitalize,
#       spellcheck = spellcheck,
#       ...
#     )
#   )
# }

## using a custom version of textAreaInput to avoid browser "smartness"
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

### ces 2 fonctions "return" permettent d'utiliser les composants js returnTextAreaBinding et returnTextInputBinding
### afin de "valider" l'input uniquement au moment ou l'on tappel la touche "Enter" (event.keyCode == 13)

## avoid all sorts of 'helpful' behavior from your browser
## based on https://stackoverflow.com/a/35514029/1974918
returnTextInput <- function(inputId, label = NULL,
                            placeholder = NULL, value = "") {
  tagList(
    tags$label(label, `for` = inputId),
    tags$input(
      id = inputId,
      type = "text",
      value = value,
      placeholder = placeholder,
      autocomplete = "off",
      autocorrect = "off",
      autocapitalize = "off",
      spellcheck = "false",
      class = "returnTextInput form-control"
    )
  )
}

## textarea where the return key submits the content
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


### Fonction de débogage, à insérer dans les fonctions réactives pour suivre l'éxecution des déclenchements
ts_print <- function(x,...){
  
  p=paste0(format(Sys.time(), "%H:%M:%OS3")," - ",x)
  print(p,...)
  
}


ansi2html <- function(ansi){
  HTML(sprintf(
    "<pre>%s</pre>",
    gsub("\n", "<br/>", as.character(fansi::sgr_to_html(ansi)))
  ))
}

