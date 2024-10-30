#' Lancement d'une appli Shiny en fond
#'
#' @param shinyPath le chemin de l'application shiny
#' @param port le port d'ecoute
#' @param sleep temp sleep avant lancement
#' @param name nom du job
#' @param opt_cmd commande optionnelle à lancer avant le run de l'appl
#'
#'
#' @return rien
launch_shiny_backgroundJob <- function(shinyPath,port=NULL,sleep=5,name=NULL,opt_cmd=NULL){

  if(is.null(port)){
    for (i in 1:20) {
      while (TRUE) {
        port <- shiny:::p_randomInt(3000, 8000)
        if (!port %in% c(3659, 4045, 5060, 5061, 6000, 6566, 6665:6669, 6697)) {
          break
        }
      }
    }
  }


  ### La fonction jobRunScript ne peut utiliser qu'un script, il faut donc ecrire cette commande dans un fichier :
  script=""
  script=c(script,opt_cmd)
  script=c(script,glue::glue("shiny::runApp(appDir = \"{shinyPath}\",
                    display.mode = \"normal\",
                    quiet = TRUE,
                    host = \"127.0.0.1\",
                    port = {port})"))

  filename=tempfile("db-explorer")
  fileConn <- file(filename)
  writeLines(c(script),fileConn)
  close.connection(fileConn)


  job <- rstudioapi::jobRunScript(path = filename ,importEnv =FALSE,exportEnv = "R_GlobalEnv",name = paste0(name," (port = ", port,")"))

  url <- glue::glue("http://127.0.0.1:{port}")

  test_url <- function(url) {
    response <- tryCatch({
      httr::GET(url,httr::use_proxy(""))
    }, error = function(e) {
      return(NULL)
    })

    if (!is.null(response) && httr::status_code(response) == 200) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }


  max_attempts <- 30

  for (i in 1:max_attempts) {

    if (test_url(url)) {
      browseURL(url)
      rstudioapi::executeCommand("activateConsole")
      break
    } else {
      Sys.sleep(1)
    }
  }

}
