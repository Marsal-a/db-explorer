#' Lancement d'une appli Shiny en background
#'
#' @param shinyPath le chemin de l'application shiny
#' @param host l'adresse de connexion à l'application
#' @param port le port d'ecoute
#' @param name nom du job
#' @param opt_cmd commande optionnelle à lancer avant le run de l'appl
#' @param max_attempts Nombre de tentative de connexion à l'application
#'
#' @importFrom rstudioapi jobRunScript
#' @importFrom glue glue
#' @importFrom httr GET use_proxy
#' @importFrom utils browseURL
#' @importFrom httpuv randomPort
#' @return rien
launch_shiny_backgroundJob <- function(shinyPath,host="127.0.0.1",port=NULL,name=NULL,opt_cmd=NULL,max_attempts=30){

  if (is.null(port)) {
    port <- httpuv::randomPort(min=3000,max=15000,n=50)
  }

  ### La fonction jobRunScript ne peut prendre qu'un script en entrée (et pas une commande),
  ### il faut donc ecrire cette commande dans un fichier :
  script=""
  script=c(script,opt_cmd)
  script=c(script,glue::glue("shiny::runApp(appDir = \"{shinyPath}\",
                    display.mode = \"normal\",
                    quiet = TRUE,
                    host = \"127.0.0.1\",
                    port = {port})"))

  filename <- tempfile("db-explorer")
  fileConn <- file(filename)
  writeLines(c(script),fileConn)
  close.connection(fileConn)

  job <- rstudioapi::jobRunScript(path = filename ,importEnv =FALSE,exportEnv = "R_GlobalEnv",name = paste0(name," (port = ", port,")"))

  url <- glue::glue("http://127.0.0.1:{port}")

  isUrlReady <- function(url) {
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


  ## L'application shiny peut prendre quelques secondes pour être "up"
  ## Le lancement de l'app via shiny::runApp démarre un serveur et le chargement
  ## des packages nécessaires peut prendre du temps
  ## Pour ouvrir l'application, on procède à {max_attemps} tentatives de connexion
  ## et on attends 1 secondes entre chaque tentative.
  ## Si l'url répond, on ouvre l'application et on repositionne rstudio sur la
  ## console (car par défault se positionne sur l'onglet job)

  for (i in 1:max_attempts) {
    if (isUrlReady(url)) {
      utils::browseURL(url)
      rstudioapi::executeCommand("activateConsole")
      break
    } else {
      Sys.sleep(1)
    }
  }

  unlink(filename)
  return(invisible(NULL))

}

#' Lancement d'une appli Shiny en fond
#'
#'@param connectorFile Nom et chemin du fichier de connexion. Le fichier de connexion doit permettre la construction
#' d'une liste de connecteur, nommé connectors.
#'
#' @export
ExplorerDonnees <- function(connectorFile=NULL){

  if(is.null(connectorFile)){
    if(is.null(getOption("dbExplorer.connectorFile"))){
      stop(paste0("Veuillez spécifier un fichier de connexion :\n",
      "- dans le paramètre connectorFile : ExplorerDonnees(connectorFile=FICHIER_CONNEXION)\n",
      "ou \n",
      "- dans via l'option R \"dbExplorer.connectorFile\" : options(dbExplorer.connectorFile = FICHIER_CONNEXION)"))
    }else{
      connectorFile=getOption("dbExplorer.connectorFile")
    }
  }
  connectorFile <- normalizePath(connectorFile)
  if(TRUE){
    cmd_load_connector = paste0("source(\"",connectorFile,"\")")
  }

  path_shinyApp <- system.file("shinyApp",package="dbExplorer")
  launch_shiny_backgroundJob(path_shinyApp,opt_cmd = cmd_load_connector)

}
