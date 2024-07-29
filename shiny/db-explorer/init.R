# library(shinyAce)
# library(shiny)
# library(shinyhelper)
# library(glue)
# library(bslib)
# library(dplyr)
# library(DBI)
# library(dbplyr)
# library(purrr)
# library(rlang)
# library(orasdse)
# library(pgsdse)
# library(nzsdse)
# library(fontawesome)

db_choices=c("SQLite","Netezza","PostgreSQL - Prod","Oracle - Prod")
db_choices=c("Netezza","PostgreSQL - Prod","Oracle - Prod")

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


### Render des .md
# rmarkdown::render(paste0(getOption("path_db_explorer"),"tools/help/help_filters.md"))

addResourcePath("help","./tools/help/")

# packages=c("dplyr","dbplyr","RJDBC","pillar","nzsdse","DBI")
# purrr::walk(packages,function(p){
#   print(paste0(p," :",packageVersion(p)))
# })

## Cloture des sessions au maximum aprés 12h d'ouverture
start_time <- Sys.time()
max_session_time <- 3600*1


sql_keywords <- c("SELECT", "FROM", "WHERE", "AND", "OR", "NOT", 
                  "JOIN", "INNER JOIN", "LEFT JOIN", "RIGHT JOIN", "FULL JOIN", 
                  "ON", "GROUP BY", "HAVING", "ORDER BY", "LIMIT", "OFFSET", 
                  "DISTINCT", "COUNT", "SUM", "AVG", "MAX", "MIN", "BETWEEN", 
                  "LIKE", "IN", "EXISTS", "CASE", "WHEN", "THEN", "ELSE", "END", 
                  "IS", "NULL", "TRUE", "FALSE", "ALL", "ANY", "SOME", "ASC", 
                  "DESC", "UNION", "INTERSECT", "EXCEPT", "WITH", "RECURSIVE")
