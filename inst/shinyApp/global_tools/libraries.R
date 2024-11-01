## Si l'application est lancé depuis R_Commun ou depuis le projet R, on source des versions particulières des librairies.
if(grepl("db-explorer/inst/shinyApp|R_Commun",getwd())){
  .libPaths(c( "~/R_Commun/Adam/custom_lib/db-explorer/",.libPaths()))
}

## Si l'application est lancé via la fonction du package, on source les librairies depuis le répertoire du package
if(grepl("/4.1/dbExplorer/shinyApp",getwd())){
  .libPaths(c(system.file("library",package = "dbExplorer"),.libPaths()))
}

library(magrittr)
library(crayon)
library(shinyAce)
library(shiny)
library(glue)
library(dplyr)
library(DBI)
library(dbplyr)
library(purrr)
library(rlang)
library(fontawesome)
library(stringr)
library(shinyjs)
library(shinyBS)

# library(DT)
# library(fan)
# library(tidyr)
# library(rmarkdown)
# library(stringi)
# library(data.table)
# library(htmltools)
# library(shinyWidgets)
# library(withr)

options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m","-Xss3m"))
