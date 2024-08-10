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



source(paste0(getOption("path_db_explorer"),"init.R"))
source(paste0(getOption("path_db_explorer"),"dev_connector.R"))
source(paste0(getOption("path_db_explorer"),"view_tab_function.R"))



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

print_session <- function(){
  
  session=sessionInfo()
  pkg_load=c(session$loadedOnly,session$otherPkgs)
  
  list_pkg=purrr::map(pkg_load,function(pkg){
    df=data.frame(pkg$Package,pkg$Version)
  })
  print(bind_rows(list_pkg)|>arrange(pkg.Package),row.names=FALSE)
  
}

logger <- function(session,path_out){
  
  Sys.sleep(1)
  fname=paste0(path_out,fs::path_sanitize(paste0("log_db-explorer_",Sys.info()['user'],"_",format(Sys.time(), "%Y%m%d_%H%M%S"),".txt")))
  session=sessionInfo()
  pkg_load=c(session$loadedOnly,session$otherPkgs)
  
  list_pkg=purrr::map(pkg_load,function(pkg){
    df=data.frame(pkg$Package,pkg$Version)
  })
  
  res=c(
    Sys.info(),
    start_Time=format(start_time,"%Y%m%d_%H%M%S"),
    bind_rows(list_pkg)|>arrange(pkg.Package),
    end_Time=format(Sys.time(),"%Y%m%d_%H%M%S")
  )
  
  
  write.csv(res, file = fname, row.names = T, quote = FALSE)
  writeLines(capture.output(print(res)), con = fname)
  
  
}


