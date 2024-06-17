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