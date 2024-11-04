#' Création d'un connecteur postgreSQL pour l'application shiny
#'
#' Cette fonction permet de construire un fichier .R qui pourra être utilisé en source de l'application shiny.
#'
#' @param host adresse du serveur postgreSQL
#' @param dbname nom de la base de donnéespostgreSQL
#' @param port port du serveur postgreSQL
#' @param filename nom du fichier R de connexion
#' @importFrom utils file.edit

createPostgreSQLConnector <- function(host,dbname,port=5432,filename="./PostgreSQLConnector.R"){

  read_template <- readLines(system.file("template_connector","template_postgre.R",package="dbExplorer"))

  read_template <- gsub("{{HOST}}",host, read_template,fixed = T)
  read_template <- gsub("{{PORT]}",port, read_template,fixed = T)
  read_template <- gsub("{{DBNAME]}",dbname, read_template,fixed = T)

  filename = normalizePath(filename,mustWork = FALSE)

  writeLines(read_template,filename)
  file.edit(filename)
  return(invisible(NULL))
}
