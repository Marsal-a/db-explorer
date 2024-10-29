
.libPaths(c( "~/R_Commun/Adam/custom_lib/db-explorer/",.libPaths()))
# .libPaths(.libPaths()[1])

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
library(stringi)
library(stringr)
library(shinyjs)
library(shinyBS)
# library(rclipboard)

options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m","-Xss3m"))

